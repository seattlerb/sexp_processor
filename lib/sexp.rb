$TESTING ||= false # unless defined $TESTING

##
# Sexps are the basic storage mechanism of SexpProcessor.  Sexps have
# a +type+ (to be renamed +node_type+) which is the first element of
# the Sexp. The type is used by SexpProcessor to determine whom to
# dispatch the Sexp to for processing.

class Sexp < Array # ZenTest FULL

  attr_writer :line
  attr_accessor :file, :comments

  @@array_types = [ :array, :args, ]

  ##
  # Create a new Sexp containing +args+.

  def initialize(*args)
    super(args)
  end

  ##
  # Creates a new Sexp from Array +a+.

  def self.from_array(a)
    ary = Array === a ? a : [a]

    result = self.new

    ary.each do |x|
      case x
      when Sexp
        result << x
      when Array
        result << self.from_array(x)
      else
        result << x
      end
    end

    result
  end

  def ==(obj) # :nodoc:
    obj.class == self.class and super
  end

  ##
  # Returns true if this Sexp's pattern matches +sexp+.

  def ===(sexp)
    return nil unless Sexp === sexp
    pattern = self # this is just for my brain

    return true if pattern == sexp

    sexp.each do |subset|
      return true if pattern === subset
    end

    return nil
  end

  ##
  # Returns true if this Sexp matches +pattern+.  (Opposite of #===.)

  def =~(pattern)
    return pattern === self
  end

  ##
  # Returns true if the node_type is +array+ or +args+.
  #
  # REFACTOR: to TypedSexp - we only care when we have units.

  def array_type?
    type = self.sexp_type
    @@array_types.include? type
  end

  def compact # :nodoc:
    self.delete_if { |o| o.nil? }
  end

  ##
  # Recursively enumerates the sexp yielding to +block+ for every element.

  def deep_each(&block)
    return enum_for(:deep_each) unless block_given?

    self.each_sexp do |sexp|
      block[sexp]
      sexp.deep_each(&block)
    end
  end

  def depth
    1 + (each_sexp.map(&:depth).max || 0)
  end

  ##
  # Enumeratates the sexp yielding to +b+ when the node_type == +t+.

  def each_of_type(t, &b)
    return enum_for(:each_of_type) unless block_given?

    each do | elem |
      if Sexp === elem then
        elem.each_of_type(t, &b)
        b.call(elem) if elem.sexp_type == t
      end
    end
  end

  ##
  # Recursively enumerates all sub-sexps skipping non-Sexp elements.

  def each_sexp
    return enum_for(:each_sexp) unless block_given?

    self.each do |sexp|
      next unless Sexp === sexp

      yield sexp
    end
  end

  ##
  # Replaces all elements whose node_type is +from+ with +to+. Used
  # only for the most trivial of rewrites.

  def find_and_replace_all(from, to)
    each_with_index do | elem, index |
      if Sexp === elem then
        elem.find_and_replace_all(from, to)
      else
        self[index] = to if elem == from
      end
    end
  end

  ##
  # Replaces all Sexps matching +pattern+ with Sexp +repl+.

  def gsub(pattern, repl)
    return repl if pattern == self

    new = self.map do |subset|
      case subset
      when Sexp then
        subset.gsub(pattern, repl)
      else
        subset
      end
    end

    return Sexp.from_array(new)
  end

  def inspect # :nodoc:
    sexp_str = self.map {|x|x.inspect}.join(', ')
    if ENV['VERBOSE'] && line then
      "s(#{sexp_str}).line(#{line})"
    else
      "s(#{sexp_str})"
    end
  end

  def find_node name, delete = false
    matches = find_nodes name

    case matches.size
    when 0 then
      nil
    when 1 then
      match = matches.first
      delete match if delete
      match
    else
      raise NoMethodError, "multiple nodes for #{name} were found in #{inspect}"
    end
  end

  ##
  # Find every node with type +name+.

  def find_nodes name
    find_all { | sexp | Sexp === sexp and sexp.sexp_type == name }
  end

  ##
  # If passed a line number, sets the line and returns self. Otherwise
  # returns the line number. This allows you to do message cascades
  # and still get the sexp back.

  def line(n=nil)
    if n then
      @line = n
      self
    else
      @line ||= nil
    end
  end

  ##
  # Returns the maximum line number of the children of self.

  def line_max
    @line_max ||= self.deep_each.map(&:line).max
  end

  ##
  # Returns the size of the sexp, flattened.

  def mass
    @mass ||=
      inject(1) { |t, s|
      if Sexp === s then
        t + s.mass
      else
        t
      end
    }
  end

  ##
  # Returns the node named +node+, deleting it if +delete+ is true.

  def method_missing meth, delete = false
    r = find_node meth, delete
    if ENV["DEBUG"] then
      if r.nil? then
        warn "%p.method_missing(%p) => nil from %s" % [self, meth, caller.first]
      elsif ENV["VERBOSE"]
        warn "%p.method_missing(%p) from %s" % [self, meth, caller.first]
      end
    end
    r
  end

  def respond_to? msg, private = false # :nodoc:
    # why do I need this? Because ruby 2.0 is broken. That's why.
    super
  end

  def pretty_print(q) # :nodoc:
    nnd = ')'
    nnd << ".line(#{line})" if line && ENV['VERBOSE']

    q.group(1, 's(', nnd) do
      q.seplist(self) {|v| q.pp v }
    end
  end

  ##
  # Returns the node type of the Sexp.

  def sexp_type
    first
  end

  ##
  # Sets the node type of the Sexp.

  def sexp_type= v
    self[0] = v
  end

  ##
  # Returns the Sexp body, ie the values without the node type.

  def sexp_body
    self[1..-1]
  end

  ##
  # Returns the Sexp body, ie the values without the node type.

  def sexp_body= v
    self[1..-1] = v
  end

  alias :head :sexp_type
  alias :rest :sexp_body

  ##
  # If run with debug, Sexp will raise if you shift on an empty
  # Sexp. Helps with debugging.

  def shift
    raise "I'm empty" if self.empty?
    super
  end if ($DEBUG or $TESTING) unless (defined?(RUBY_ENGINE) and RUBY_ENGINE == "maglev")

  ##
  # Returns the bare bones structure of the sexp.
  # s(:a, :b, s(:c, :d), :e) => s(:a, s(:c))

  def structure
    if Array === self.sexp_type then
      s(:bogus, *self).structure # TODO: remove >= 4.2.0
    else
      result = s(self.sexp_type)
      self.each do |subexp|
        result << subexp.structure if Sexp === subexp
      end
      result
    end
  end

  ##
  # Replaces the Sexp matching +pattern+ with +repl+.

  def sub(pattern, repl)
    return repl.dup if pattern == self

    done = false

    new = self.map do |subset|
      if done then
        subset
      else
        case subset
        when Sexp then
          if pattern == subset then
            done = true
            repl.dup
          elsif pattern === subset then
            done = true
            subset.sub pattern, repl
          else
            subset
          end
        else
          subset
        end
      end
    end

    return Sexp.from_array(new)
  end

  def to_a # :nodoc:
    self.map { |o| Sexp === o ? o.to_a : o }
  end

  def to_s # :nodoc:
    inspect
  end
end

##
# I'm starting to warm up to this idea!
# ENV["STRICT_SEXP"] turns on various levels of conformance checking
#
# 1 = sexp[0]         => sexp_type
# 1 = sexp.first      => sexp_type
# 1 = sexp[0] = x     => sexp_type = x
# 1 = sexp[1..-1]     => sexp_body
# 1 = sexp[1..-1] = x => sexp_body = x
# 1 = sexp[-1]        => last
# 2 = sexp[1]         => no
# 2 = sexp[1] = x     => no
# 3 = sexp[n]         => no
# 3 = sexp[n] = x     => no
# 4 = sexp.replace x  => no
# 4 = sexp.concat x   => no

class Sexp

  alias safe_idx []
  alias safe_asgn []=
  alias sexp_type= sexp_type=
  alias sexp_body= sexp_body=

  def self.__strict
    ENV["STRICT_SEXP"].to_i
  end

  def __strict
    self.class.__strict
  end

  def [] i
    raise "use sexp_type" if i == 0
    raise "use sexp_body" if i == (1..-1)
    raise "use last" if i == -1
    raise "no idx>1: #{inspect}[#{i}]" if Integer === i && i > 1 if __strict > 1
    raise "no idx: #{inspect}[#{i}]" if __strict > 2
    self.safe_idx i
  end

  def []= i, v
    raise "use sexp_type=" if i == 0
    raise "use sexp_body=" if i == (1..-1)
    raise "no asgn>1: #{inspect}[#{i}] = #{v.inspect}" if Integer === i && i > 1 if
      __strict > 1
    raise "no asgn: #{inspect}[#{i}] = #{v.inspect}" if
      __strict > 2
    self.safe_asgn i, v
  end

  def first
    raise "use sexp_type"
  end

  def replace o
    raise "no: %p.replace %p" % [self, o]
  end if __strict > 3

  def concat o
    raise "no: %p.concat %p" % [self, o]
  end if __strict > 3

  def sexp_type
    safe_idx 0
  end

  def sexp_body
    safe_idx 1..-1
  end

  def sexp_type= v
    self.safe_asgn 0, v
  end

  def sexp_body= v
    self.safe_asgn 1..-1, v
  end
end unless Sexp.new.respond_to? :safe_asgn if ENV["STRICT_SEXP"]

class SexpMatchSpecial < Sexp; end

class SexpAny < SexpMatchSpecial
  def ==(o)
    Sexp === o
  end

  def ===(o)
    return Sexp === o
  end

  def inspect
    "ANY"
  end
end

module SexpMatchSpecials
  def ANY(); return SexpAny.new; end
end

##
# This is a very important shortcut to make using Sexps much more awesome.

def s(*args)
  Sexp.new(*args)
end
