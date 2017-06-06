$TESTING ||= false # unless defined $TESTING

require "forwardable"

##
# Sexps are the basic storage mechanism of SexpProcessor.  Sexps have
# a +type+ (to be renamed +node_type+) which is the first element of
# the Sexp. The type is used by SexpProcessor to determine whom to
# dispatch the Sexp to for processing.

class Sexp < Array # ZenTest FULL
  attr_writer :line
  attr_accessor :file, :comments

  @@array_types = [ :array, :args ] # TODO: remove

  ##
  # Create a new Sexp containing +args+.

  def initialize *args
    super(args)
  end

  ##
  # Creates a new Sexp from Array +a+.

  def self.from_array a
    ary = Array === a ? a : [a]

    self.new(*ary.map { |x|
               case x
               when Sexp
                 x
               when Array
                 self.from_array(x)
               else
                 x
               end
             })
  end

  def map &blk # :nodoc:
    s(*super(&blk)) # ensures a sexp from map
  end

  def == obj # :nodoc:
    obj.class == self.class and super # only because of a bug in ruby
  end

  ##
  # Tree equivalent to +String#=~+, returns true if this Sexp or a
  # sub-sexp matches +pattern+.

  def =~ pattern
    raise ArgumentError, "Not a pattern" unless Matcher === pattern
    pattern =~ self
  end

  ##
  # Returns true if the node_type is +array+ or +args+.
  #
  # REFACTOR: to TypedSexp - we only care when we have units.

  def array_type?
    warn "DEPRECATED: please file an issue if you actually use this. from #{caller.first}"
    type = self.sexp_type
    @@array_types.include? type
  end

  def compact # :nodoc:
    self.delete_if(&:nil?)
  end

  ##
  # Recursively enumerates the sexp yielding to +block+ for every element.

  def deep_each &block
    return enum_for(:deep_each) unless block_given?

    self.each_sexp do |sexp|
      block[sexp]
      sexp.deep_each(&block)
    end
  end

  ##
  # Return the maximum depth of the sexp. One-based.

  def depth
    1 + (each_sexp.map(&:depth).max || 0)
  end

  ##
  # Enumeratates the sexp yielding to +b+ when the node_type == +t+.

  def each_of_type t, &b
    return enum_for(:each_of_type) unless block_given?

    each_sexp do | sexp |
      sexp.each_of_type(t, &b)
      yield sexp if sexp.sexp_type == t
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

  def find_and_replace_all from, to
    each_with_index do | elem, index |
      if Sexp === elem then
        elem.find_and_replace_all(from, to)
      elsif elem == from
        self[index] = to
      end
    end
  end

  ##
  # Replaces all Sexps matching +pattern+ with Sexp +repl+.

  def gsub pattern, repl
    return repl if pattern == self

    new = self.map { |subset|
      case subset
      when Sexp then
        if Matcher === pattern && pattern.satisfy?(subset) then # TODO: make === be satisfy? maybe?
          repl.dup
        else
          subset.gsub pattern, repl
        end
      else
        subset
      end
    }

    Sexp.from_array new
  end

  def inspect # :nodoc:
    sexp_str = self.map(&:inspect).join ", "
    if ENV["VERBOSE"] && line then
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
    each_sexp.find_all { |sexp| sexp.sexp_type == name }
  end

  ##
  # If passed a line number, sets the line and returns self. Otherwise
  # returns the line number. This allows you to do message cascades
  # and still get the sexp back.

  def line n = nil
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
    @mass ||= inject(1) { |t, s| Sexp === s ? t + s.mass : t }
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

  def pretty_print q # :nodoc:
    nnd = ")"
    nnd << ".line(#{line})" if line && ENV["VERBOSE"]

    q.group(1, "s(", nnd) do
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
  # Returns the Sexp body (starting at +from+, defaulting to 1), ie
  # the values without the node type.

  def sexp_body from = 1
    self[from..-1]
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
  end if ($DEBUG or $TESTING)

  ##
  # Returns the bare bones structure of the sexp.
  # s(:a, :b, s(:c, :d), :e) => s(:a, s(:c))

  def structure
    if Array === self.sexp_type then
      warn "NOTE: form s(s(:subsexp)).structure is deprecated. Removing in 5.0"
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

  def sub pattern, repl
    return repl.dup if pattern == self
    return repl.dup if Matcher === pattern && pattern.satisfy?(self)

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
          elsif Matcher === pattern && pattern.satisfy?(subset) then
            done = true
            repl.dup
          else
            subset.sub pattern, repl
          end
        else
          subset
        end
      end
    end

    Sexp.from_array new
  end

  def to_a # :nodoc:
    self.map { |o| Sexp === o ? o.to_a : o }
  end

  def to_s # :nodoc:
    inspect
  end
end

##
# This is a very important shortcut to make using Sexps much more awesome.

def s *args, &blk
  return Sexp.q(&blk) if blk
  Sexp.new(*args)
end

##
# Extends Sexp w/ matchy stuff
#
# For example here is a Sexp query that looks for s(:a):
#
#   query = s { s(:a) }
#
# A more interesting query might look for classes with names starting
# in Test:
#
#   query = s { s(:class, m(/^Test\w+/ % 'name', _, _)) }
#
# This makes use of a Sexp::Matcher::Pattern, two Sexp::Matcher::Wild
# matchers and Sexp::Traverse#capture_as for capturing the name to a
# variable 'name'.
#
# For more examples, see the various Sexp class methods, the examples,
# and the tests supplied with Sexp.

class Sexp
  # def !@ # TODO? I dunno, needs to convert s -> q?
  #   Sexp::Matcher::Not.new self
  # end

  ##
  # Searches for the +pattern+ returning a SexpCollection containing
  # a SexpResult for each match.
  #
  # Example:
  #   s(:a, s(:b)) / Q{ s(:b) } => [s(:b)]

  def / pattern
    raise ArgumentError, "Not a pattern: #{pattern.inspect}" unless Matcher === pattern
    pattern / self
  end

  ##
  # Searches for the +pattern+ yielding a SexpResult for each match.

  def search_each pattern, &block
    raise ArgumentError, "Needs a pattern" unless pattern.kind_of? Matcher

    return enum_for(:search_each, pattern) unless block_given?

    if pattern.satisfy? self then
      res = MatchResult.new self
      yield res
    end

    self.each_sexp do |subset|
      subset.search_each pattern, &block
    end
  end

  ##
  # Searches for the +pattern+ yielding a SexpResult
  # for each match, and replacing it with the result of
  # the block.

  def replace_sexp pattern, &block # TODO: rename to gsub
    raise ArgumentError, "Needs a pattern" unless pattern.kind_of? Matcher

    if pattern.satisfy? self then
      res = Sexp::MatchResult.new self
      return yield res
    end

    self.class.new(*self.map { |subset|
                     case subset
                     when Sexp then
                       subset.replace_sexp pattern, &block
                     else
                       subset
                     end
                   })
  end

  ##
  # This is the longhand method for create a Sexp query, normally
  # one would use s { ... }, however it is also possible to do:
  #
  #   Sexp.q { s() }

  def self.q *args, &block
    return class_eval(&block) if block
    Matcher.new(*args)
  end

  ##
  # Matches an S-Expression.
  #
  # example
  #   s(:a) / s { s(:a) }        #=> [m(:a)]
  #   s(:a) / s { s() }          #=> []
  #   s(:a, s(:b)) / s { s(:b) } #=> [m(:b)]

  def self.s *args
    Matcher.new(*args)
  end

  ##
  # Matches any single item.
  #
  # example:
  #   s(:a) / s { _ }        #=> [s(:a)]
  #   s(:a, s(s(:b))) / s { s(_) } #=> [s(s(:b))]

  def self._
    Wild.new
  end

  ##
  # Matches all remaining input.
  #
  # This is a special case pattern and has a few odd characteristics.
  #
  # - You cannot capture with +remaining+
  # - If remaining comes before any other matchers, they will be ignored.
  #
  # example:
  #   s(:a) / s { s(:a, ___ ) }         #=> [s(:a)]
  #   s(:a, :b, :c) / s { s(:a, ___ ) } #=> [s(:a, :b, :c)]

  def self.___
    Remaining.new
  end

  ##
  # Matches an expression or any expression that includes the child.
  #
  # example:
  #   s(:a, :b)   / s { include(:b) }
  #   s(s(s(:a))) / s { include(:a) }

  def self.include child # TODO: rename, name is generic ruby
    Include.new(child)
  end

  ##
  # Matches any atom.
  #
  # example:
  #   s(:a) / s { s(atom) }        #=> [s(:a)]
  #   s(:a, s(:b)) / s { s(atom) } #=> [s(:b)]

  def self.atom
    Atom.new
  end

  ##
  # Matches when any of the sub expressions match.
  #
  # example:
  #   s(:a) / s { any(s(:a), s(:b)) } #=> [s(:a)]
  #   s(:a) / s { any(s(:b), s(:c)) } #=> []

  def self.any *args
    Any.new(*args)
  end

  ##
  # Matches only when all sub expressions match
  #
  # example:
  #   s(:a) / s { all(s(:a), s(:b)) } #=> []
  #   s(:a,:b) / s { t(:a), include(:b)) } #=> [s(:a,:b)]

  def self.all *args
    All.new(*args)
  end

  ##
  # Matches when sub expression does not match, see Sexp::Matcher#-@
  #
  # example:
  #   s(:a) / s { is_not(s(:b)) } #=> [s(:a)]
  #   s(:a) / s { s(is_not :a) } #=> []

  def self.not? arg
    Not.new arg
  end

  ##
  # Matches anything that has a child matching the sub expression
  #
  # example:
  #   s(s(s(s(s(:a))))) / s { child(s(:a)) } #=> [s(s(s(s(s(:a)))))]

  def self.child child
    Child.new child
  end

  ##
  # Matches anything having the same sexp_type, which is the first value in a Sexp.
  #
  # example:
  #   s(:a, :b) / s { t(:a) } #=> [s(:a, :b)]
  #   s(:a, :b) / s { t(:b) } #=> []
  #   s(:a, s(:b, :c)) / s { t(:b) } #=> [s(:b, :c)]

  def self.t name
    Type.new name
  end

  ##
  # Matches any atom who's string representation matches the patterns passed in.
  #
  # example:
  #   s(:a) / s { m('a') } #=> [s(:a)]
  #   s(:a) / s { m(/\w/,/\d/) } #=> [s(:a)]
  #   s(:tests, s(s(:test_a), s(:test_b))) / s { m(/test_\w/) } #=> [s(:test_a), s(:test_b)]

  def self.m * values
    res = values.map { |value|
      case value
      when Regexp then
        value
      else
        re = Regexp.escape value.to_s
        Regexp.new "\\A%s\\Z" % re
      end
    }
    Pattern.new Regexp.union(*res)
  end

  ##
  # This is an abstract matcher class.
  #
  # A matcher should implement the following methods:
  #
  # * satisfy?
  # * inspect
  #
  # +satisfy?+ determines whether the matcher matches a given input,
  # and +inspect+ will print the matcher nicely in a user's console.

  class Matcher < Sexp
    ##
    # This just reverses the call order so the matcher/pattern is the
    # argument, not the receiver. Returns +sexp+ =~ +self+.
    # TODO: push this up to Sexp and make this the workhorse
    # TODO: do the same with ===/satisfy?

    def satisfy? o
      return unless o.kind_of?(Sexp) &&
        (length == o.length || Matcher === last && last.greedy?)

      each_with_index.all? { |child, i|
        sexp = o.at i # HACK? avoiding [] to flush out other problems in r2r
        if Sexp === child then # TODO: when will this NOT be a matcher?
          sexp = o.sexp_body i if child.respond_to?(:greedy?) && child.greedy?
          child.satisfy? sexp
        else
          child == sexp
        end
      }
    end

    def self.match_subs?
      @@match_subs
    end

    def self.match_subs= o
      @@match_subs = o
    end

    self.match_subs = true

    def =~ o
      raise ArgumentError, "can't both be matchers" if Matcher === o

      self.satisfy?(o) ||
        (self.class.match_subs? && o.each_sexp.any? { |sub| self =~ sub })
    end

    alias === =~
    # alias === satisfy?

    def / sexp
      raise ArgumentError, "can't both be matchers" if Matcher === sexp

      MatchCollection.new sexp.search_each(self).map(&:itself)
    end

    ##
    # Combines the Matcher with another Matcher, the resulting one will
    # be satisfied if either Matcher would be satisfied.
    #
    # Example:
    #   s(:a) | s(:b)

    def | other
      Any.new(self, other)
    end

    ##
    # Combines the Matcher with another Matcher, the resulting one will
    # be satisfied only if both Matchers would be satisfied.
    #
    # Example:
    #   t(:a) & include(:b)

    def & other
      All.new(self, other)
    end

    ##
    # Returns a Matcher that matches whenever this Matcher would not have matched
    #
    # Example:
    #   -s(:a)

    def -@
      Not.new self
    end

    ##
    # Returns a Matcher that matches if this has a sibling +o+
    #
    # Example:
    #   s(:a) >> s(:b)

    def >> other
      Sibling.new(self, other)
    end

    def greedy?
      false
    end

    def inspect # :nodoc:
      s = super
      s[0] = "q"
      s
    end

    def pretty_print q # :nodoc:
      q.group 1, "q(", ")" do
        q.seplist self do |v|
          q.pp v
        end
      end
    end

    def self.parse s
      Parser.new(s).parse
    end

    ##
    # Converts from a lispy string to Sexp matchers in a safe manner.
    #
    #   "(a 42 _ (c) [t x] ___)" => s { s(:a, 42, _, s(:c), t(:x), ___) }

    class Parser
      attr_accessor :tokens

      def initialize s
        self.tokens = []
        lex s
      end

      def lex s
        tokens.concat s.scan(%r%[()\[\]]|\"[^"]*\"|/[^/]*/|[\w-]+%)
      end

      def next_token
        raise SyntaxError, "unbalanced input" if tokens.empty?
        tokens.shift
      end

      def peek_token
        tokens.first
      end

      def parse
        result = parse_sexp until tokens.empty?
        result
      end

      ##
      # Parses a string into a sexp matcher:
      #
      #   SEXP : "(" SEXP:args* ")"          => Sexp.q(*args)
      #        | "[" CMD:cmd sexp:args* "]"  => Sexp.cmd(*args)
      #        | "nil"                       => nil
      #        | /\d+/:n                     => n.to_i
      #        | "___"                       => Sexp.___
      #        | "_"                         => Sexp._
      #        | /^\/(.*)\/$/:re             => Regexp.new re[0]
      #        | /^"(.*)"$/:s                => String.new s[0]
      #        | NAME:name                   => name.to_sym
      #   NAME : /\w+/
      #    CMD : "t" | "m" | "atom"

      def parse_sexp
        token = next_token

        case token
        when "(" then
          parse_list
        when "[" then
          parse_cmd
        when "nil" then
          nil
        when /^\d+$/ then
          token.to_i
        when "___" then
          Sexp.___
        when "_" then
          Sexp._
        when %r%^/(.*)/$% then
          re = $1
          raise SyntaxError, "Not allowed: /#{re.inspect}/" unless
            re =~ /\A([\w()|.*+^$]+)\z/
          Regexp.new re
        when /^"(.*)"$/ then
          $1
        when /^\w+$/ then
          token.to_sym
        else
          raise SyntaxError, "unhandled token: #{token.inspect}"
        end
      end

      def parse_list
        result = Sexp.q

        result << parse_sexp while peek_token && peek_token != ")"
        next_token # pop off ")"

        result
      end

      ALLOWED = [:t, :m, :atom]

      def parse_cmd
        args = Sexp.q

        args << parse_sexp while peek_token && peek_token != "]"
        next_token # pop off "]"

        cmd = args.shift

        raise SyntaxError, "bad cmd: #{cmd.inspect}" unless ALLOWED.include? cmd

        result = Sexp.send cmd, *args

        result
      end
    end # class Parser
  end # class Matcher

  ##
  # See SexpQueryBuilder.wild and SexpQueryBuilder._

  class Wild < Matcher
    ##
    # Matches any single element.

    def satisfy? o
      true
    end

    def inspect
      "_"
    end

    def pretty_print q # :nodoc:
      q.text "_"
    end
  end

  ##
  # See SexpQueryBuilder.___

  class Remaining < Matcher
    ##
    # Always satisfied once this is reached. Think of it as a var arg.

    def satisfy? o
      true
    end

    def greedy?
      true
    end

    def inspect
      "___"
    end

    def pretty_print q # :nodoc:
      q.text "___"
    end
  end

  ##
  # See SexpQueryBuilder.any

  class Any < Matcher
    attr_reader :options

    ##
    # Create an Any matcher which will match any of the +options+.

    def initialize *options
      @options = options
    end

    def == o
      super && self.options == o.options
    end

    ##
    # Satisfied when any sub expressions match +o+

    def satisfy? o
      options.any? { |exp|
        Sexp === exp && exp.satisfy?(o) || exp == o
      }
    end

    def inspect
      options.map(&:inspect).join(" | ")
    end

    def pretty_print q # :nodoc:
      q.group 1, "any(", ")" do
        q.seplist options do |v|
          q.pp v
        end
      end
    end
  end

  ##
  # See SexpQueryBuilder.all

  class All < Matcher
    attr_reader :options

    def == o
      super && self.options == o.options
    end

    ##
    # Create an All matcher which will match all of the +options+.

    def initialize *options
      @options = options
    end

    ##
    # Satisfied when all sub expressions match +o+

    def satisfy? o
      options.all? { |exp|
        exp.kind_of?(Sexp) ? exp.satisfy?(o) : exp == o
      }
    end

    def inspect
      options.map(&:inspect).join(" & ")
    end

    def pretty_print q # :nodoc:
      q.group 1, "all(", ")" do
        q.seplist options do |v|
          q.pp v
        end
      end
    end
  end

  ##
  # See SexpQueryBuilder.not

  class Not < Matcher
    attr_reader :value

    ##
    # Creates a Matcher which will match any Sexp that does not match the +value+

    def initialize value
      @value = value
    end

    def == o
      super && self.value == o.value
    end

    ##
    # Satisfied if a +o+ does not match the +value+

    def satisfy? o
      !(value.kind_of?(Sexp) ? value.satisfy?(o) : value == o)
    end

    def inspect
      "not?(%p)" % [value]
    end

    def pretty_print q # :nodoc:
      q.group 1, "not?(", ")" do
        q.pp value
      end
    end
  end

  ##
  # See SexpQueryBuilder.child

  class Child < Matcher
    attr_reader :child

    def == o
      super && self.child == o.child
    end

    ##
    # Create a Child matcher which will match anything having a
    # descendant matching +child+.

    def initialize child
      @child = child
    end

    ##
    # Satisfied if matches +child+ or +o+ has a descendant matching
    # +child+.

    def satisfy? o
      if child.satisfy? o
        true
      elsif o.kind_of? Sexp
        o.search_each(child).any?
      end
    end

    def inspect
      "child(%p)" % [child]
    end

    def pretty_print q # :nodoc:
      q.group 1, "child(", ")" do
        q.pp child
      end
    end
  end

  ##
  # See SexpQueryBuilder.atom

  class Atom < Matcher
    ##
    # Satisfied when +o+ is an atom (anything that is not an S-Expression)

    def satisfy? o
      !(o.kind_of? Sexp)
    end

    ##
    # Prints as +atom+

    def inspect
      "atom"
    end

    def pretty_print q # :nodoc:
      q.text "atom"
    end
  end

  ##
  # See SexpQueryBuilder.m

  class Pattern < Matcher
    attr_reader :pattern

    def == o
      super && self.pattern == o.pattern
    end

    ##
    # Create a Patten matcher which will match any atom that either
    # matches the input +pattern+.

    def initialize pattern
      @pattern = pattern
    end

    ##
    # Satisfied if +o+ is an atom, and +o+ matches +pattern+

    def satisfy? o
      !o.kind_of?(Sexp) && o.to_s =~ pattern # TODO: question to_s
    end

    def inspect
      "m(%p)" % pattern
    end

    def pretty_print q # :nodoc:
      q.group 1, "m(", ")" do
        q.pp pattern
      end
    end
  end

  ##
  # See SexpQueryBuilder.t

  class Type < Matcher
    attr_reader :sexp_type

    ##
    # Creates a Matcher which will match any Sexp who's type is +type+, where a type is
    # the first element in the Sexp.

    def initialize type
      @sexp_type = type
    end

    def == o
      super && self.sexp_type == o.sexp_type
    end

    ##
    # Satisfied if the sexp_type of +o+ is +type+.

    def satisfy? o
      o.kind_of?(Sexp) && o.sexp_type == sexp_type
    end

    def inspect
      "t(%p)" % sexp_type
    end

    def pretty_print q # :nodoc:
      q.group 1, "t(", ")" do
        q.pp sexp_type
      end
    end
  end

  ##
  # See SexpQueryBuilder.include

  class Include < Matcher
    attr_reader :value

    def == o
      super && self.value == o.value
    end

    ##
    # Creates a Matcher which will match any Sexp that contains the
    # +value+.

    def initialize value
      @value = value
    end

    ##
    # Satisfied if a +o+ is a Sexp and one of +o+'s elements matches
    # value

    def satisfy? o
      Sexp === o && o.any? { |c|
        # TODO: switch to respond_to??
        Sexp === value ? value.satisfy?(c) : value == c
      }
    end

    def inspect
      "include(%p)" % [value]
    end

    def pretty_print q # :nodoc:
      q.group 1, "include(", ")" do
        q.pp value
      end
    end
  end

  ##
  # See Matcher for sibling relations: <,<<,>>,>

  class Sibling < Matcher
    attr_reader :subject, :sibling, :distance

    def == o
      super &&
        self.subject  == o.subject &&
        self.sibling  == o.sibling &&
        self.distance == o.distance
    end

    ##
    # Creates a Matcher which will match any pair of Sexps that are siblings.
    # Defaults to matching the immediate following sibling.

    def initialize subject, sibling, distance = nil
      @subject = subject
      @sibling = sibling
      @distance = distance
    end

    ##
    # Satisfied if o contains +subject+ followed by +sibling+

    def satisfy? o
      # Future optimizations:
      # * Shortcut matching sibling
      subject_matches = index_matches(subject, o)
      return nil if subject_matches.empty?

      sibling_matches = index_matches(sibling, o)
      return nil if sibling_matches.empty?

      subject_matches.any? { |i1, _data_1|
        sibling_matches.any? { |i2, _data_2|
          distance ? (i2-i1 == distance) : i2 > i1
        }
      }
    end

    def inspect
      "%p >> %p" % [subject, sibling]
    end

    def pretty_print q # :nodoc:
      if distance then
        q.group 1, "sibling(", ")" do
          q.seplist [subject, sibling, distance] do |v|
            q.pp v
          end
        end
      else
        q.group 1 do
          q.pp subject
          q.text " >> "
          q.pp sibling
        end
      end
    end

    private

    def index_matches pattern, o
      indexes = []
      return indexes unless o.kind_of? Sexp

      o.each_with_index do |e, i|
        data = {}
        if pattern.kind_of?(Sexp) ? pattern.satisfy?(e) : pattern == o[i]
          indexes << [i, data]
        end
      end

      indexes
    end
  end # class Sibling

  ##
  # Wraps the results of a Sexp query. The matching Sexp
  # is placed in SexpResult#sexp. Any named captures will be
  # available with SexpResult#[].
  #
  # For instance:
  #   res = s(:a) / s { s( _ % :name) }
  #
  #   res.first.sexp == s(:a)
  #   res.first[:name] == :a

  class MatchResult
    extend Forwardable
    def_delegators :@data, :[], :[]=, :values_at

    attr_accessor :sexp
    attr_accessor :data

    def initialize sexp, data = {}
      @sexp = sexp
      @data = data.dup
    end

    def == o
      self.class == o.class && self.sexp == o.sexp && self.data == o.data
    end

    ##
    # Shortcut for querying directly against a result's
    # Sexp.

    def / pattern, d = {}
      pattern / sexp
    end

    def to_s
      if data.empty?
        "MatchResult.new(%s)" % [sexp]
      else
        "MatchResult.new(%s, %s)" % [sexp, data]
      end
    end

    def inspect
      "MatchResult.new(%p, %p)" % [sexp, data]
    end

    def pretty_print q # :nodoc:
      q.group 1, "MatchResult.new(", ")" do
        q.pp sexp
        q.text ", "
        q.pp data
      end
    end
  end # class MatchResult

  ##
  # Wraps the results of a Sexp query.
  # SexpCollection defines SexpCollection#search so that you can
  # chain queries.
  #
  # For instance:
  #   res = s(:a, s(:b)) / s { s(:a,_) } / s { s(:b) }

  class MatchCollection < Array
    ##
    # See Traverse#search

    def / pattern
      inject(self.class.new) { |result, match|
        result.concat match / pattern
      }
    end

    def inspect
      "MatchCollection.new(%s)" % self.to_a.inspect[1..-2]
    end

    alias :to_s :inspect

    def pretty_print q
      q.group 1, "MatchCollection.new(", ")" do
        q.seplist(self) {|v| q.pp v }
      end
    end
  end # class MatchCollection
end
