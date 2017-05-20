$TESTING = true

if ENV["COV"]
  require "simplecov"
  SimpleCov.start # "rails"
  warn "Running simplecov"
end

require 'minitest/autorun'
require "minitest/benchmark" if ENV["BENCH"]
require 'sexp_processor'
require "sexp"
require 'stringio'
require 'pp'

def pyramid_sexp max
  # s(:array,
  #   s(:array, s(:s, 1)),
  #   s(:array, s(:s, 1), s(:s, 2)),
  #   ...
  #   s(:array, s(:s, 1), s(:s, 2), ... s(:s, max-1)))

  s(:array,
    *(1...max).map { |n|
      s(:array, *(1..n).map { |m|
          s(:s, m) })})
end

class SexpTestCase < Minitest::Test
  # KEY for regex tests
  # :a == no change
  # :b == will change (but sometimes ONLY once)
  # :c == change to

  def assert_equals3 x, y
    assert_operator x, :===, y
  end

  def refute_equals3 x, y
    refute_operator x, :===, y
  end
end

class TestSexp < SexpTestCase # ZenTest FULL

  class SexpFor
    def method
      1
    end
  end

  def assert_pretty_print expect, input
    assert_equal expect, input.pretty_inspect.chomp
  end

  def setup
    super
    @sexp_class = Object.const_get(self.class.name[4..-1])
    @processor = SexpProcessor.new
    @sexp = @sexp_class.new(1, 2, 3)
    @basic_sexp = s(:lasgn, :var, s(:lit, 42).line(1)).line(1)
    @complex_sexp = s(:block,
                      s(:lasgn, :foo, s(:str, "foo").line(1)).line(1),
                      s(:if, s(:and, s(:true).line(2), s(:lit, 1).line(2)).line(2),
                        s(:if, s(:lvar, :foo).line(3),
                          s(:str, "bar").line(3),
                          nil).line(3),
                        s(:true).line(5)).line(2)).line(1)

    @re = s(:lit, 42)
    @bad1 = s(:lit, 24)
    @bad1 = s(:blah, 42)
  end

  def test_class_from_array
    skip 'Need to write test_class_from_array'
  end

  def test_class_index
    skip 'Need to write test_class_index'
  end

  def test_array_type_eh
    assert_equal false, @sexp.array_type?
    @sexp.unshift :array
    assert_equal true, @sexp.array_type?
  end

  def test_each_of_type
    # TODO: huh... this tests fails if top level sexp :b is removed
    @sexp = s(:b, s(:a, s(:b, s(:a), :a, s(:b, :a), s(:b, s(:a)))))
    count = 0
    @sexp.each_of_type(:a) do |exp|
      count += 1
    end
    assert_equal(3, count, "must find 3 a's in #{@sexp.inspect}")
  end

  def test_equals2_array
    refute_equal @sexp, [1, 2, 3]        # Sexp == Array
    assert_raises Minitest::Assertion do # Array == Sexp.
      refute_equal [1, 2, 3], @sexp      # This is a bug in ruby:
    end
    # TODO: test if it is calling to_ary first? seems not to
  end

  def test_equals2_not_body
    sexp2 = s(1, 2, 5)
    refute_equal(@sexp, sexp2)
  end

  def test_equals2_sexp
    sexp2 = s(1, 2, 3)
    if @sexp.class == Sexp then
      skip "Not applicable to this target."
    else
      refute_equal(@sexp, sexp2)
    end
  end

  def test_equals3_any
    any = s{ ___ }
    assert_equals3 any, s()
    assert_equals3 any, s(:a)
    assert_equals3 any, s(:a, :b, s(:c))
  end

  def test_equals3_full_match
    assert_equals3 s(), s()             # 0
    assert_equals3 s(:blah), s(:blah)   # 1
    assert_equals3 s(:a, :b), s(:a, :b) # 2
    assert_equals3 @basic_sexp, @basic_sexp.dup     # deeper structure
  end

  def test_equals3_mismatch
    refute_equals3 s(),                      s(:a)
    refute_equals3 s(:a),                    s()
    refute_equals3 s(:blah1),                s(:blah2)
    refute_equals3 s(:a),                    s(:a, :b)
    refute_equals3 s(:a, :b),                s(:a)
    refute_equals3 s(:a1, :b),               s(:a2, :b)
    refute_equals3 s(:a, :b1),               s(:a, :b2)
    refute_equals3 @basic_sexp,              @basic_sexp.dup.push(42)
    refute_equals3 @basic_sexp.dup.push(42), @basic_sexp
  end

  def test_equals3_subset_match
    assert_match   s{s(:a)},    s(s(:a), s(:b))                  # left - =~
    skip "broken in new ==="
    assert_equals3 s{s(:a)},    s(s(:a), s(:b))                  # left - ===
    assert_equals3 s(:a),       s(:blah, s(:a   ), s(:b))        # mid 1
    assert_equals3 s(:a, 1),    s(:blah, s(:a, 1), s(:b))        # mid 2
    assert_equals3 @basic_sexp, s(:blah, @basic_sexp.dup, s(:b)) # mid deeper
    assert_equals3 @basic_sexp, s(@basic_sexp.dup, s(:a), s(:b)) # left deeper

    assert_equals3 s(:a),       s(:blah, s(:blah, s(:a)))        # left deeper
  end

  def test_equalstilde_fancy
    assert_match s{ s(:b) }, s(:a, s(:b), :c)
    assert_match s(:a, s(:b), :c), s{ s(:b) }

    e = assert_raises ArgumentError do
      s(:b) =~ s(:a, s(:b), :c)
    end
    assert_equal "Not a pattern", e.message

    e = assert_raises ArgumentError do
      s(:a, s(:b), :c) =~ s(:b)
    end
    assert_equal "Not a pattern", e.message
  end

  def test_equalstilde_plain
    skip "broken in new ==="

    result = @basic_sexp =~ @re
    assert result
  end

  def test_find_and_replace_all
    @sexp    = s(:a, s(:a, :b, s(:a, :b), s(:a), :b, s(:a, s(:a))))
    expected = s(:a, s(:a, :a, s(:a, :a), s(:a), :a, s(:a, s(:a))))

    @sexp.find_and_replace_all(:b, :a)

    assert_equal(expected, @sexp)
  end

  def test_gsub
    assert_equal s(:c),        s(:b).       gsub(s(:b), s(:c))
    assert_equal s(:a),        s(:a).       gsub(s(:b), s(:c))
    assert_equal s(:a, s(:c)), s(:a, s(:b)).gsub(s(:b), s(:c))
  end

  def test_gsub_empty
    assert_equal s(:c), s().gsub(s(), s(:c))
  end

  def test_gsub_multiple
    assert_equal(s(:a, s(:c), s(:c)),
                 s(:a, s(:b), s(:b)).        gsub(s(:b), s(:c)))
    assert_equal(s(:a, s(:c), s(:a, s(:c))),
                 s(:a, s(:b), s(:a, s(:b))). gsub(s(:b), s(:c)))
  end

  def test_inspect
    k = @sexp_class
    n = k.name[0].chr.downcase
    assert_equal("#{n}()",
                 k.new().inspect)
    assert_equal("#{n}(:a)",
                 k.new(:a).inspect)
    assert_equal("#{n}(:a, :b)",
                 k.new(:a, :b).inspect)
    assert_equal("#{n}(:a, #{n}(:b))",
                 k.new(:a, k.new(:b)).inspect)
  end

  def test_line
    assert_nil @sexp.line
    assert_equal 1, @basic_sexp.line
    assert_equal 1, @complex_sexp.line
  end

  def test_line_max
    assert_nil @sexp.line_max
    assert_equal 1, @basic_sexp.line_max
    assert_equal 5, @complex_sexp.line_max
  end

  def test_mass
    assert_equal 1, s(:a).mass
    assert_equal 3, s(:a, s(:b), s(:c)).mass

    s = s(:iter,
          s(:call, nil, :a, s(:arglist, s(:lit, 1))),
          s(:lasgn, :c),
          s(:call, nil, :d, s(:arglist)))

    assert_equal 7, s.mass
  end

  def test_mass_auto_shift
    assert_equal 1, s(:a).mass
    assert_equal 3, s(s(:b), s(:c)).mass

    s = s(s(:call, nil, :a, s(:arglist, s(:lit, 1))),
          s(:lasgn, :c),
          s(:call, nil, :d, s(:arglist)))

    assert_equal 7, s.mass
  end

  def test_mass_huge
    max = 100
    sexp = pyramid_sexp max

    exp = (max*max + max)/2 # pyramid number 1+2+3+...+m

    assert_equal exp, sexp.mass
  end

  def test_method_missing
    assert_nil @sexp.not_there
    assert_equal s(:lit, 42), @basic_sexp.lit
  end

  def test_method_missing_ambigious
    assert_raises NoMethodError do
      pirate = s(:says, s(:arrr!), s(:arrr!), s(:arrr!))
      pirate.arrr!
    end
  end

  def test_method_missing_deep
    sexp = s(:blah, s(:a, s(:b, s(:c, :yay!))))
    assert_equal(s(:c, :yay!), sexp.a.b.c)
  end

  def test_method_missing_delete
    sexp = s(:blah, s(:a, s(:b, s(:c, :yay!))))

    assert_equal(s(:c, :yay!), sexp.a.b.c(true))
    assert_equal(s(:blah, s(:a, s(:b))), sexp)
  end

  def test_pretty_print
    assert_pretty_print("s()",
                        s())
    assert_pretty_print("s(:a)",
                        s(:a))
    assert_pretty_print("s(:a, :b)",
                        s(:a, :b))
    assert_pretty_print("s(:a, s(:b))",
                        s(:a, s(:b)))
  end

  def test_sexp_body
    assert_equal [2, 3], @sexp.sexp_body
  end

  def test_shift
    skip "https://github.com/MagLev/maglev/issues/250" if maglev?

    assert_equal(1, @sexp.shift)
    assert_equal(2, @sexp.shift)
    assert_equal(3, @sexp.shift)

    assert_raises(RuntimeError) do
      @sexp.shift
    end
  end

  def test_deep_clone
    @sexp    = s(:a, 1, 2, s(:b, 3, 4), 5, 6)
    backup = @sexp.deep_clone
    refute_same @sexp, backup, "deep clone is broken again?"
    assert_equal @sexp, backup, "deep clone is broken again?"
  end

  def test_structure
    @sexp    = s(:a, 1, 2, s(:b, 3, 4), 5, 6)
    backup = @sexp.deep_clone
    refute_same @sexp, backup, "deep clone is broken again?"
    assert_equal @sexp, backup, "deep clone is broken again?"
    expected = s(:a, s(:b))

    assert_equal(expected, @sexp.structure)
    assert_equal(backup, @sexp)
  end

  def test_sub
    assert_equal s(:c),               s(:b).               sub(s(:b), s(:c))
    assert_equal s(:a, s(:c), s(:b)), s(:a, s(:b), s(:b)). sub(s(:b), s(:c))
    assert_equal s(:a, s(:c), s(:a)), s(:a, s(:b), s(:a)). sub(s(:b), s(:c))
  end

  def test_sub_miss
    assert_equal s(:a),               s(:a).        sub(s(:b), s(:c))
    assert_equal s(:a, s(:c)),        s(:a, s(:c)). sub(s(:b), s(:c))
  end

  def test_sub_empty
    assert_equal s(:c),               s().          sub(s(), s(:c))
  end

  def test_sub_structure
    assert_equal(s(:a, s(:c, s(:b))), s(:a, s(:b)). sub(s(:b), s(:c, s(:b))))
  end

  def test_to_a
    assert_equal([1, 2, 3], @sexp.to_a)
  end

  def test_to_s
    test_inspect
  end

  def test_each_sexp
    result = []
    @basic_sexp.each_sexp { |_, val| result << val }
    assert_equal [42], result
  end

  def test_each_sexp_without_block
    assert_kind_of Enumerator, @basic_sexp.each_sexp
    assert_equal [42], @basic_sexp.each_sexp.map { |_, n| n }
  end

  def test_depth
    assert_equal 1, s(:a).depth
    assert_equal 2, s(:a, s(:b)).depth
    assert_equal 3, s(:a, s(:b1, s(:c)), s(:b2)).depth
    assert_equal 5, s(:a, s(:b, s(:c, s(:d, s(:e))))).depth
  end

  def test_deep_each
    result = []
    @complex_sexp.deep_each { |s| result << s if s.sexp_type == :if }
    assert_equal [:if, :if], result.map { |k, _| k }
  end

  def test_deep_each_without_block
    assert_kind_of Enumerator, @complex_sexp.deep_each
    assert_equal [:if, :if], @complex_sexp.deep_each.select { |s, _| s == :if }.map { |k, _| k }
  end
end

class TestSexpMatchers < SexpTestCase
  CLASS_LIT = s(:class, :X, nil,
                s(:lasgn, :x, s(:lit, 42)),
                s(:cdecl, :Y,
                  s(:hash, s(:lit, :a), s(:lit, 1), s(:lit, :b), s(:lit, 2))))

  SEXP = s(:class, :X, nil, s(:defn, :x, s(:args)))

  def test_match_subset
    assert_match s{ child(s(:a)) }, s(:blah, s(:blah, s(:a)))
    assert_match s{ child(s(:a)) }, s(:a)
  end

  def test_match_simple
    assert_match s{ s(:lit, _) }, s(:lit, 42)
  end

  def test_match_mismatch_type
    refute_match s{ s(:xxx, 42) }, s(:lit, 42)
  end

  def test_match_mismatch_data
    refute_match s{ s(:lit, 24) }, s(:lit, 42)
  end

  def test_match_mismatch_length_shorter
    refute_match s{ s(:a, :b) }, s(:a, :b, :c)
  end

  def test_match_mismatch_length_longer
    refute_match s{ s(:a, :b, :c) }, s(:a, :b)
  end

  def test_match_wild
    assert_match s{ s(:class, _, _, _) }, SEXP
  end

  def test_match_rest_same_length
    assert_match s{ s(:class, _, _, ___) }, SEXP
  end

  def test_match_rest_diff_length
    assert_match s{ s(:class, ___) }, SEXP
  end

  def test_match_reversed
    assert_match SEXP, s{ s(:class, _, _, ___) }
  end

  # NOTE: eqt is =~ (equal-tilde)

  # cmt = create_match_test
  def self.cmt e1, e2, e3, e4, lit, pat
    Class.new SexpTestCase do
      attr_accessor :lit, :pat

      define_method :setup do
        self.lit = lit
        self.pat = pat
      end

      define_method :test_match_lit_eqt_pat do
        if e1 then
          assert_match lit, pat
        else
          refute_match lit, pat
        end
      end

      define_method :test_match_pat_eqt_lit do
        if e2 then
          assert_match pat, lit
        else
          refute_match pat, lit
        end
      end

      define_method :test_match_lit_eq3_pat do
        if e3 then
          assert_equals3 lit, pat
        else
          refute_equals3 lit, pat
        end
      end

      define_method :test_match_pat_eq3_lit do
        if e4 then
          assert_equals3 pat, lit
        else
          refute_equals3 pat, lit
        end
      end
    end
  end

  l_a   = s(:a)
  l_abc = s(:a, s(:b, s(:c)))
  l_cls = s(:class, :X, nil,
           s(:something_in_between),
           s(:cdecl, :Y, s(:hash, s(:lit, :a), s(:lit, 1))))
  p_cls1 = s{ s(:class, ___) & include(s(:cdecl, _, s(:hash, ___))) }
  p_cls2 = s{ s(:class, _, _, s(:cdecl, _, s(:hash, ___))) }

  x, o = true, false
  TestMatcherDirectMatch       = cmt x, x, o, x, l_a,   s{ s(:a) }
  TestMatcherSubtree           = cmt x, x, o, x, l_abc, s{ s(:c) }
  TestMatcherSubtreeType       = cmt x, x, o, x, l_abc, s{ t(:c) }
  TestMatcherDisparateSubtree  = cmt x, x, o, x, l_cls, p_cls1
  TestMatcherDisparateSubtree2 = cmt o, o, o, o, l_cls, p_cls2 # TODO: make pass

  def test_match_class_literals__sanity_check_block
    block = s(:block,
              s(:class, :X, nil,
                s(:lasgn, :x, s(:lit, 42)),
                s(:lasgn, :x, s(:lit, 42)),
                s(:cdecl, :Y, s(:hash)),
                s(:lasgn, :x, s(:lit, 42))))

    pat = s{ s(:class, ___) & include(s(:cdecl, _, s(:hash, ___))) }

    assert_equals3 pat, block
    refute_equals3 block, pat
    assert_match   pat, block
    assert_match   block, pat
  end

  def test_match_class_literals__dont_match_children__sanity_check
    sub_class_lit = s(:class, :X, nil,
                      s(:lasgn, :x, s(:lit, 42)),
                      s(:lasgn, :x, s(:lit, 42)),
                      s(:class, :Y, nil, s(:cdecl, :Y, s(:hash))),
                      s(:lasgn, :x, s(:lit, 42)))

    pat = s{ s(:class, ___) & include(s(:cdecl, _, s(:hash, ___))) }

    refute_equals3 sub_class_lit, pat
    assert_equals3 pat, sub_class_lit
    assert_match   pat, sub_class_lit
    assert_match   sub_class_lit, pat
  end

  def assert_match_case pat, data
    case data
    when pat then
      assert true
    else
      flunk "Expected %p to match %p" % [pat, data]
    end
  end

  def test_match_case
    assert_match_case s { s(:class, _, _, ___) }, SEXP
  end
end

## test/sexp_path_capture_test.rb

class Minitest::Test
  make_my_diffs_pretty!
end

class SexpPathCaptureTest < Minitest::Test
  SC = Sexp::MatchCollection
  SR = Sexp::MatchResult

  def setup
    @ast_sexp =
      s(:class, :cake, nil,
        s(:defn, :foo, s(:args), s(:add, :a, :b)),
        s(:defn, :bar, s(:args), s(:sub, :a, :b)))
  end

  def test_simple_searching
    act = @ast_sexp / s { s(:class, atom % "name", _, ___) }

    exp = coll @ast_sexp, "name" => :cake

    assert_equal exp, act
  end

  def test_iterative_searching
    act = @ast_sexp / s { s(:class, atom % "class", _, ___) } / s { s(:defn, atom % "method", _, _) }

    exp = coll(@ast_sexp[3], { "class" => :cake, "method" => :foo },
               @ast_sexp[4], { "class" => :cake, "method" => :bar })

    assert_equal exp, act
  end

  def test_capturing_any_matchers
    sexp = s(:add, :a, :b)
    assert res = s { any(s(:add, :a, :b), s(:sub, :a, :b)) % "match" }.satisfy?( sexp )
    assert_equal sexp, res["match"]

    assert res = s { any(s(atom % "name", :a, :b), s(:sub, :a, :b)) % "match" }.satisfy?( sexp )
    assert_equal sexp, res["match"]
    assert_equal :add, res["name"]
  end

  def test_capturing_all_matchers
    sexp = s(:add, :a, :b)
    assert res = s { all(s(_, :a, :b), s(atom, :a, :b)) % "match" }.satisfy?( sexp )
    assert_equal sexp, res["match"]

    assert res = s { all(s(_ % "wild", :a, :b), s(atom % "atom", :a, :b)) % "match" }.satisfy?( sexp )
    assert_equal sexp, res["match"]
    assert_equal :add, res["wild"]
    assert_equal :add, res["atom"]
  end

  def test_capturing_type_matches
    sexp = s(:add, :a, :b)
    assert res = s { t(:add) % "match" }.satisfy?( sexp )
    assert_equal sexp, res["match"]
  end

  def test_capturing_child_matches
    sexp = s(:a, s(:b, s(:c)))
    assert res = s { s(:a, child( s(atom % "atom") ) % "child" ) }.satisfy?( sexp )
    assert_equal s(:b, s(:c)), res["child"]
    assert_equal :c, res["atom"]
  end

  def test_catpuring_pattern_matches
    sexp = s(:add, :a, :b)
    assert res = s { s(m(/a../) % "regexp", :a, :b) }.satisfy?( sexp )
    assert_equal :add, res["regexp"]
  end

  def test_catpuring_include_matches
    sexp = s(:add, :a, :b)
    assert res = s { include(:a) % "include" }.satisfy?( sexp )
    assert_equal sexp, res["include"]
  end

  def test_catpuring_nested_include_matches
    sexp = s(:add, s(:a), :b)
    assert res = s { include(s(atom % "atom" )) % "include" }.satisfy?( sexp )
    assert_equal sexp, res["include"]
    assert_equal :a, res["atom"]
  end

  def test_capturing_negations
    sexp = s(:b)
    assert res = s { (-s(:a)) % "not" }.satisfy?( sexp )
    assert_equal s(:b), res["not"]
  end

  def test_capturing_negation_contents
    sexp = s(:a, :b)
    assert res = s { -((include(:b) % "b") & t(:c)) }.satisfy?( sexp )
    assert !res["b"], "b should not be included"
  end

  def test_capturing_siblings
    sexp = s(s(:a), s(s(:b)), s(:c))
    assert res = s { (s(atom) % "a") >> (s(atom) % "c") }.satisfy?( sexp )
    assert_equal s(:a), res["a"]
    assert_equal s(:c), res["c"]
  end

  def test_capturing_remaining
    sexp = s(s(:a), s(:b), s(:c))
    assert res = s { s(s(:a), ___ % "match") }.satisfy?( sexp )
    assert_equal [s(:b), s(:c)], res["match"]
  end

  def test_capturing_remaining_atoms
    sexp = s(:a, :b, :c)
    assert res = s { s(:a, ___ % "match") }.satisfy?( sexp )
    assert_equal [:b, :c], res["match"]
  end

  def coll *args
    exp = SC.new

    args.each_slice 2 do |sexp, hash|
      exp << res(sexp, hash)
    end

    exp
  end

  def res sexp, hash
    SR.new sexp.deep_clone, hash
  end
end

## test/sexp_path_matching_test.rb

class SexpMatchingPathTest < Minitest::Test
  def setup
    @ast_sexp = # Imagine it looks like a ruby AST
      s(:class, :cake, nil,
        s(:defn, :foo, s(:add, :a, :b)),
        s(:defn, :bar, s(:sub, :a, :b)))
  end

  def test_searching_simple_examples
    assert_search_count @ast_sexp, :class, 0,
                        "Literal should match nothing"

    assert_search_count @ast_sexp, s { s(:class) }, 0,
                        "Should not exactly match anything"

    assert_search_count @ast_sexp, s { s(:add, :a, :b) }, 1,
                        "Should exactly match once"

    assert_search_count s(:a, s(:b, s(:c))), s { s(:b, s(:c)) }, 1,
                        "Should match an exact subset"

    assert_search_count s(:a, s(:b, s(:c))), s { s(:a, s(:c)) }, 0,
                        "Should not match the child s(:c)"

    assert_search_count @ast_sexp, s { s(:defn, :bar, s(:sub, :a, :b)) }, 1,
                        "Nested sexp should exactly match once"
  end

  def test_equality_of_atom
    a = Sexp::Atom.new
    assert a.satisfy?(:a),  "Should match a symbol"
    assert a.satisfy?(1),   "Should match a number"
    assert a.satisfy?(nil), "Should match nil"
    refute a.satisfy?(s()), "Should not match an sexp"
  end

  def test_searching_with_atom
    assert_search_count s(:add, :a, :b), s { s(:add, atom, :b) }, 1,
                        "atom should match :a"

    assert_search_count @ast_sexp, s { s(:defn, atom, s(atom, :a, :b) ) }, 2,
                        "atoms should match :foo/:bar and :add/:sub"

    assert_search_count s(:a, s()), s { s(:a, atom) }, 0,
                        "atom should not match s()"
  end

  def test_searching_with_any
    assert_search_count s(:foo, s(:a), s(:b)), s { s(any(:a, :b)) }, 2,
                        "should not match either :a or :b"

    assert_search_count s(:foo, s(:a), s(:b)), s { any( s(:a), s(:c)) }, 1,
                        "sexp should not match s(:a)"
  end

  def test_equality_of_wildacard
    w = Sexp::Wild.new
    assert w.satisfy?(:a  ), "Should match a symbol"
    assert w.satisfy?(1   ), "Should match a number"
    assert w.satisfy?(nil ), "Should match nil"
    assert w.satisfy?([]  ), "Should match an array"
    assert w.satisfy?(s() ), "Should match an sexp"
  end

  def test_searching_with_wildcard
    assert_search_count s(:add, :a, :b), s { s(:add, _, :b) }, 1,
                        "wild should match :a"

    assert_search_count @ast_sexp, s { s(:defn, :bar, _) }, 1,
                        "should match s(:defn, :bar, s(..))"

    assert_search_count @ast_sexp, s { s(:defn, _, s(_, :a, :b) ) }, 2,
                        "wilds should match :foo/:bar and :add/:sub"

    assert_search_count s(:a, s()), s { s(:a, _) }, 1,
                        "wild should match s()"

    assert_search_count s(:a, :b, :c), s { s(_, _, _) }, 1,
                        "multiple wilds should work"

    assert_search_count @ast_sexp, s { _ }, 5,
                        "_ should match every sub expression"
  end

  def test_searching_with_include
    assert_search_count s(:add, :a, :b), s { include(:a) }, 1,
                        "Sexp should include atom :a"

    assert_search_count @ast_sexp, s { include(:bar) }, 1,
                        "Sexp should include atom :bar"

    assert_search_count @ast_sexp, s { s(:defn, atom, include(:a)) }, 2,
                        "Sexp should match :defn with an sexp including :a"

    assert_search_count @ast_sexp, s { include(:a) }, 2,
                        "Sexp should match an sexp including :a"

    assert_search_count s(:a, s(:b, s(:c))), s { s(:a, include(:c)) }, 0,
                        "Include should not descend"
  end

  def test_or_matcher
    assert s { s(:a) | s(:b) }.satisfy?( s(:a) ), "q(:a) should match s(:a)"
    refute s { s(:a) | s(:b) }.satisfy?( s(:c) ), "Should not match s(:c)"

    assert_search_count s(:a, s(:b, :c), s(:b, :d)), s { s(:b, :c) | s(:b, :d) }, 2,
                        "Should match both (:b, :c) and (:b, :d)"

    assert_search_count @ast_sexp, s { s(:add, :a, :b) | s(:defn, :bar, _) }, 2,
                        "Should match at any level"
  end

  # For symmetry, kind of silly examples
  def test_and_matcher
    refute s { s(:a) & s(:b)   }.satisfy?(s(:a)), "s(:a) is not both s(:a) and s(:b)"
    assert s { s(:a) & s(atom) }.satisfy?(s(:a)), "s(:a) matches both criteria"
  end

  def test_child_matcher
    assert_search_count @ast_sexp, s { s(:class, :cake, _, _, child( s(:sub, :a, :b) ) ) }, 1,
                        "Should match s(:class, :cake ...) and descend to find s(:sub, :a, :b)"

    assert_search_count @ast_sexp, s { s(:class, :cake, _, _, child(include(:a))) }, 1,
                        "Should match once since there exists a child which includes :a"
  end

  def test_not_matcher
    refute s { -_            }.satisfy?(s(:a)), "-_ should not match s(:a)"
    assert s { -s(:b)        }.satisfy?(s(:a)), "s(:b) should not match s(:b)"
    assert s { is_not(s(:b)) }.satisfy?(s(:a)), "should behave the same as unary minus"
    refute s { -s(atom)      }.satisfy?(s(:a)), "should not match, :a is an atom"
    assert s { s(is_not(:b)) }.satisfy?(s(:a)), "should match s(:a) since the atom is not :b"
  end

  def test_sibling_matcher
    assert_equal Sexp::Sibling, s { (s(:a) >> s(:b)) }.class

    assert s { s(:a) >> s(:b) }.satisfy?( s(s(:a), s(:b)) ),        "should match s(:a) has an immediate sibling s(:b)"
    assert s { s(:a) >> s(:b) }.satisfy?( s(s(:a), s(:b), s(:c)) ), "should match s(:a) has an immediate sibling s(:b)"
    assert s { s(:a) >> s(:c) }.satisfy?( s(s(:a), s(:b), s(:c)) ), "should match s(:a) has a sibling s(:b)"
    refute s { s(:c) >> s(:a) }.satisfy?( s(s(:a), s(:b), s(:c)) ), "should not match s(:a) does not follow s(:c)"
    refute s { s(:a) >> s(:a) }.satisfy?( s(s(:a)) ),               "should not match s(:a) has no siblings"
    assert s { s(:a) >> s(:a) }.satisfy?( s(s(:a), s(:b), s(:a)) ), "should match s(:a) has another sibling s(:a)"

    assert_search_count @ast_sexp, s { t(:defn) >> t(:defn) }, 1,
                        "Should match s(:add, :a, :b) followed by s(:sub, :a, :b)"
  end

  def test_pattern_matcher
    assert s { m(/a/)     }.satisfy?(:a),         "Should match :a"
    assert s { m(/^test/) }.satisfy?(:test_case), "Should match :test_case"
    assert s { m("test")  }.satisfy?(:test),      "Should match :test #{s { m("test") }.inspect}"
    refute s { m("test")  }.satisfy?(:test_case), "Should only match whole word 'test'"
    refute s { m(/a/)     }.satisfy?(s(:a)),      "Should not match s(:a)"

    assert_search_count @ast_sexp, s { s(m(/\w{3}/), :a, :b) }, 2,
                        "Should match s(:add, :a, :b) and s(:sub, :a, :b)"
  end

  def test_remaining_matcher
    assert s { ___         }.satisfy?( s(:a) ),         "Should match a single atom"
    assert s { ___         }.satisfy?( s(:a, :b, :c) ), "Should match multiple atoms"
    assert s { s(:x, ___ ) }.satisfy?( s(:x, :y) ),     "Should match the remainder after :x"
    refute s { s(:y, ___ ) }.satisfy?( s(:x, :y) ),     "Should not match (initial atom doesn't match)"
  end

  def test_search_method
    assert_equal 1, @ast_sexp.search( s { s(:sub, :a, :b) }).length
    assert_equal 2, @ast_sexp.search( s { s(:defn, atom, _) } ).length
  end

  def test_search_collection
    # test method
    assert_kind_of Sexp::MatchCollection, @ast_sexp.search( s { s(:sub, :a, :b) })
    # test binary operator
    assert_kind_of Sexp::MatchCollection, (@ast_sexp / s { s(:sub, :a, :b) })
    # test sub searches
    collection = @ast_sexp / s { s(:defn, atom, _) } / s { s(atom, :a, :b) }
    assert_kind_of Sexp::MatchCollection, collection
    assert_equal 2, collection.length
    assert_equal [s(:add, :a, :b), s(:sub, :a, :b)], collection.map(&:sexp)
  end

  def test_sexp_type_matching
    assert s { t(:a) }.satisfy?( s(:a) )
    assert s { t(:a) }.satisfy?( s(:a, :b, s(:oh_hai), :d) )
    assert_search_count @ast_sexp, s { t(:defn) }, 2, "Should match s(:defn, _, _)"
  end

  def assert_search_count sexp, example, count, message
    i = 0
    sexp.search_each(example) { |_match| i += 1 }
    assert_equal count, i, message + "\nSearching for: #{example.inspect}\nIn: #{sexp.inspect}"
  end
end

## test/sexp_path_new_test.rb

class TestSexpPath < Minitest::Test
  def test_global_s_block
    sexp = s(:a, :b, :c) # s called outside block

    assert_instance_of Sexp,          s { sexp.deep_clone }
    assert_instance_of Sexp::Matcher, s { s(:a, :b, :c) }
    assert_instance_of Sexp::Matcher, s { s(:a, atom, :c) }
  end

  class TestSexp < Minitest::Test
    def test_satisfy_eh_no_capture
      sexp = s(:a, :b, :c)
      assert_equal Hash.new, s { s(:a, :b, :c) }.satisfy?(sexp.dup)
    end

    def test_satisfy_eh_named_capture
      sexp = s(:a, :b, :c)
      act = s { s(:a, :b, :c) % "cake" }.satisfy? sexp.dup
      exp = { "cake" => sexp.dup }

      assert_equal exp, act
    end

    def test_satisfy_eh_multiple_named_capture
      act = s { s(:add, atom % :A, atom % :B) }.satisfy?( s(:add, :a, :b) )

      assert_equal :a, act[:A]
      assert_equal :b, act[:B]
    end

    def ast_sexp
      s(:class, :cake, nil,
        s(:defn, :foo, s(:args), s(:add, :a, :b)),
        s(:defn, :bar, s(:args), s(:sub, :a, :b)))
    end

    def test_deep_matches
      assert act = s { s(:class, atom % "name", _, _ % "def1", _ % "def2") }.satisfy?( ast_sexp )
      assert_equal(:cake, act["name"])
      assert_equal(s(:defn, :foo, s(:args), s(:add, :a, :b)), act["def1"])
      assert_equal(s(:defn, :bar, s(:args), s(:sub, :a, :b)), act["def2"])
    end

    def test_satisfy_eh_bad_type
      refute_operator s { s(:a, :b, :c) }, :satisfy?, 42
    end

    def test_satisfy_eh_bad_length
      refute_operator s { s(:a, :b, :c) }, :satisfy?, s(:a, :b) # mismatched length
    end

    def test_satisfy_eh_bad_non_greedy
      refute_operator s { s(:a, :b, s(:c)) }, :satisfy?, s(:a, :b, :c) # not greedy
      refute_operator s { s(:a,     s(:c)) }, :satisfy?, s(:a, :b, :c) # not greedy
    end
  end

  # class TestMatchCollection < Minitest::Test
  #   def test_slash
  #     flunk 'Need to write test_slash'
  #   end
  #
  #   def test_search
  #     flunk 'Need to write test_search'
  #   end
  # end
  #
  # class TestSexpQueryBuilder < Minitest::Test
  #   def test_class__
  #     assert_kind_of Sexp::Matcher::Wild, Sexp::SexpQueryBuilder._
  #   end
  #
  #   def test_class____
  #     assert_kind_of Sexp::Matcher::Remaining, Sexp::SexpQueryBuilder.___
  #   end
  #
  #   def test_class_all
  #     assert_kind_of Sexp::Matcher::All, Sexp::SexpQueryBuilder.all
  #   end
  #
  #   def test_class_any
  #     flunk 'Need to write test_class_any'
  #   end
  #
  #   def test_class_atom
  #     flunk 'Need to write test_class_atom'
  #   end
  #
  #   def test_class_child
  #     flunk 'Need to write test_class_child'
  #   end
  #
  #   def test_class_do
  #     flunk 'Need to write test_class_do'
  #   end
  #
  #   def test_class_is_not
  #     flunk 'Need to write test_class_is_not'
  #   end
  #
  #   def test_class_m
  #     flunk 'Need to write test_class_m'
  #   end
  #
  #   def test_class_not_eh
  #     flunk 'Need to write test_class_not_eh'
  #   end
  #
  #   def test_class_s
  #     act = Sexp::SexpQueryBuilder.s :a, 42
  #     exp = Sexp::Matcher::Base.new(:a, 42) # TODO: Base seems wrong/bad
  #     assert_equal exp, act
  #   end
  #
  #   def test_class_t
  #     act = Sexp::SexpQueryBuilder.t :a
  #     exp = Sexp::Matcher::Type.new :a
  #     assert_equal exp, act
  #   end
  # end
  #
  # class TestSexpResult < Minitest::Test
  #   def test_slash
  #     flunk 'Need to write test_slash'
  #   end
  #
  #   def test_search
  #     flunk 'Need to write test_search'
  #   end
  #
  #   def test_sexp
  #     flunk 'Need to write test_sexp'
  #   end
  #
  #   def test_sexp_equals
  #     flunk 'Need to write test_sexp_equals'
  #   end
  # end
  #
  # class TestTraverse < Minitest::Test
  #   def test_capture_as
  #     flunk 'Need to write test_capture_as'
  #   end
  #
  #   def test_slash
  #     flunk 'Need to write test_slash'
  #   end
  #
  #   def test_percent
  #     flunk 'Need to write test_percent'
  #   end
  #
  #   def test_replace_sexp
  #     flunk 'Need to write test_replace_sexp'
  #   end
  #
  #   def test_search
  #     flunk 'Need to write test_search'
  #   end
  #
  #   def test_search_each
  #     flunk 'Need to write test_search_each'
  #   end
  # end
  #
  # class TestMatcher
  #   class TestAll < Minitest::Test
  #     def test_satisfy_eh
  #       flunk 'Need to write test_satisfy_eh'
  #     end
  #   end
  #
  #   class TestAny < Minitest::Test
  #     def test_satisfy_eh
  #       flunk 'Need to write test_satisfy_eh'
  #     end
  #   end
  #
  #   class TestAtom < Minitest::Test
  #     def test_satisfy_eh
  #       flunk 'Need to write test_satisfy_eh'
  #     end
  #   end
  #
  #   class TestBase < Minitest::Test
  #     def test_and
  #       flunk 'Need to write test_and'
  #     end
  #
  #     def test_greedy_eh
  #       flunk 'Need to write test_greedy_eh'
  #     end
  #
  #     def test_gt2
  #       flunk 'Need to write test_gt2'
  #     end
  #
  #     def test_or
  #       flunk 'Need to write test_or'
  #     end
  #
  #     def test_unary_minus
  #       flunk 'Need to write test_unary_minus'
  #     end
  #   end
  #
  #   class TestBlock < Minitest::Test
  #     def test_satisfy_eh
  #       flunk 'Need to write test_satisfy_eh'
  #     end
  #   end
  #
  #   class TestChild < Minitest::Test
  #     def test_satisfy_eh
  #       flunk 'Need to write test_satisfy_eh'
  #     end
  #   end
  #
  #   class TestInclude < Minitest::Test
  #     def test_satisfy_eh
  #       flunk 'Need to write test_satisfy_eh'
  #     end
  #   end
  #
  #   class TestNot < Minitest::Test
  #     def test_satisfy_eh
  #       flunk 'Need to write test_satisfy_eh'
  #     end
  #   end
  #
  #   class TestPattern < Minitest::Test
  #     def test_satisfy_eh
  #       flunk 'Need to write test_satisfy_eh'
  #     end
  #   end
  #
  #   class TestRemaining < Minitest::Test
  #     def test_greedy_eh
  #       flunk 'Need to write test_greedy_eh'
  #     end
  #
  #     def test_satisfy_eh
  #       flunk 'Need to write test_satisfy_eh'
  #     end
  #   end
  #
  #   class TestSibling < Minitest::Test
  #     def test_satisfy_eh
  #       flunk 'Need to write test_satisfy_eh'
  #     end
  #
  #     def test_sibling
  #       flunk 'Need to write test_sibling'
  #     end
  #
  #     def test_subject
  #       flunk 'Need to write test_subject'
  #     end
  #   end
  #
  #   class TestType < Minitest::Test
  #     def test_satisfy_eh
  #       flunk 'Need to write test_satisfy_eh'
  #     end
  #   end
  #
  #   class TestWild < Minitest::Test
  #     def test_satisfy_eh
  #       flunk 'Need to write test_satisfy_eh'
  #     end
  #   end
  # end
end

## test/sexp_replacement_test.rb

class SexpReplacementTest < Minitest::Test
  def test_replacing_exact_matches
    sexp = s(:a, s(:b), :c)
    actual = sexp.replace_sexp(s{ s(:b) }) { :b }

    assert_equal( s(:a, :b, :c), actual)
  end

  def test_replacing_root
    sexp = s(:a, s(:b), :c)
    actual = sexp.replace_sexp(s { t(:a) }) { s(:new) }

    assert_equal( s(:new), actual)
  end

end

## test/use_case_test.rb

# Here's a crazy idea, these tests actually use sexp_path on some "real"
# code to see if it can satisfy my requirements.
#
# These tests are two fold:
# 1. Make sure it works
# 2. Make sure it's not painful to use
class UseCaseTest < Minitest::Test
  @@sexp = eval File.read(__FILE__).split(/^__END__/).last

  def setup
    @sexp = @@sexp.deep_clone
  end

  def test_finding_methods
    methods = @sexp / s { t(:defn) }
    assert_equal 5, methods.length
  end

  def test_finding_classes_and_methods
    res = @sexp / s { s(:class, atom % "name", ___ ) }
    assert_equal 1, res.length
    assert_equal :ExampleTest, res.first["name"]

    methods = res / s { t(:defn) }
    assert_equal 5, methods.length
  end

  def test_finding_empty_test_methods
    empty_test = s { s(:defn, m(/^test_.+/) % "name", s(:args), s(:nil)) }
    res = @sexp / empty_test

    assert_equal 1, res.length
    assert_equal :test_b, res.first["name"]
  end

  def test_to_s
    res =  s(:class, :X, nil) / s { s(:class, _, _, ___) }
    exp = "[s(:class, :X, nil) {}]"

    assert_equal exp, res.to_s

    res =  s(:class, :X, nil) / s { s(:class, _ % :name, _, ___) }
    exp = "[s(:class, :X, nil) {:name=>:X}]"
    assert_equal exp, res.to_s
  end

  def test_inspect
    res =  s(:class, :X, nil) / s { s(:class, _, _, ___) }
    exp = "[s(:class, :X, nil) {}]"

    assert_equal exp, res.inspect

    res =  s(:class, :X, nil) / s { s(:class, _ % :name, _, ___) }
    exp = "[s(:class, :X, nil) {:name=>:X}]"
    assert_equal exp, res.inspect
  end

  def test_pretty_inspect
    res =  s(:class, :X, nil) / s { s(:class, _, _, ___) }
    exp = "[s(:class, :X, nil) {}]\n"

    assert_equal exp, res.pretty_inspect

    res =  s(:class, :X, nil) / s { s(:class, _ % :name, _, ___) }
    exp = "[s(:class, :X, nil) {:name=>:X}]\n"
    assert_equal exp, res.pretty_inspect
  end

  def test_finding_duplicate_test_names
    res = @sexp / s { s(:defn, m(/^test_.+/) % "name", ___ ) }
    counts = Hash.new { |h, k| h[k] = 0 }

    res.each do |m|
      method_name = m["name"]
      counts[method_name] += 1
    end

    assert_equal 1, counts[:test_b], "Should have seen test_b once"
    assert_equal 2, counts[:test_a], "Should have caught test_a being repeated"
  end

  def test_rewriting_colon2s_oh_man_i_hate_those_in_most_cases_but_i_understand_why_they_are_there
    colon2 = s { s(:colon2, s(:const, atom % "const"), atom % "scope") }

    # Hacky, could be done better
    while (results = (@sexp / colon2)) && !results.empty?
      results.each do |result|
        result.sexp.replace s(:const, result.values_at("const", "scope").join("::"))
      end
    end

    expected_sexp = s { s(:const, "Minitest::Test") }
    assert_equal 1, (@sexp / expected_sexp).length, @sexp.inspect
  end

  def test_rewriting_colon2s_again
    colon2   = s { s(:colon2, s(:const, atom % "const"), atom % "scope") }
    expected = s { s(:const, "Minitest::Test") }

    new_sexp = @sexp.replace_sexp(colon2) { |r|
      s(:const, r.values_at("const", "scope").join("::") )
    }

    assert_equal 1, (new_sexp / expected).length, @sexp.inspect
    assert_equal 0, (@sexp    / expected).length, @sexp.inspect
  end
end

class TestSexpMatcherParser < Minitest::Test
  def assert_parse exp, str
    act = Sexp::Matcher::Parser.new(str).parse

    if exp.nil? then
      assert_nil act
    else
      assert_equal exp, act
    end
  end

  def self.test_parse name, exp, str
    define_method "test_parse_#{name}" do
      assert_parse exp, str
    end
  end

  def self.test_bad_parse name, str
    define_method "test_parse_bad_#{name}" do
      assert_raises SyntaxError do
        assert_parse :whatever, str
      end
    end
  end

  # TODO: add unhappy path
  test_parse "nothing",  nil,                             ""
  test_parse "nil",      s{ nil },                        "nil"
  test_parse "empty",    s{ s() },                        "()"
  test_parse "simple",   s{ s(:a) },                      "(a)"
  test_parse "number",   s{ s(:a, 42) },                  "(a 42)"
  test_parse "string",   s{ s(:a, "s") },                 "(a \"s\")"
  test_parse "compound", s{ s(:b) },                      "(a) (b)"
  test_parse "complex",  s{ s(:a, _, s(:b, :cde), ___) }, "(a _ (b cde) ___)"
  test_parse "type",     s{ s(:a, t(:b)) },               "(a [t b])"
  test_parse "match",    s{ s(:a, m(/b/)) },              "(a [m /b/])"
  test_parse "not_atom", s{ s(:atom) },                   "(atom)"
  test_parse "atom",     s{ atom },                       "[atom]"

  test_bad_parse "open_sexp",   "(a"
  test_bad_parse "closed_sexp", "a)"
  test_bad_parse "open_cmd",    "[a"
  test_bad_parse "closed_cmd",  "a]"
end


class BenchSexp < Minitest::Benchmark
  def run
    GC.disable
    super
  ensure
    GC.enable
  end

  def self.bench_range
    bench_linear 100, 500, 50
  end

  @@data = Hash[bench_range.map { |n| [n, pyramid_sexp(n)] }]

  def bench_pyramid
    assert_performance_power do |max|
      pyramid_sexp max
    end
  end

  def bench_mass
    assert_performance_power do |max|
      @@data[max].mass
    end
  end
end if ENV["BENCH"]

# class ExampleTest < Minitest::Test
#   def setup
#     1 + 2
#   end
#
#   def test_a
#     assert_equal 1+2, 4
#   end
#
#   def test_b
#     # assert 1+1
#   end
#
#   def test_a
#     assert_equal 1+2, 3
#   end
#
#   private
#
#   def helper_method apples, oranges, cakes = nil
#     [apples, oranges, cakes].compact.map { |food| food.to_s.upcase }
#   end
# end

__END__
s(:block,
 s(:call, nil, :require, s(:str, "minitest/autorun")),
 s(:class,
  :ExampleTest,
  s(:colon2, s(:const, :Minitest), :Test),
  s(:defn, :setup, s(:args), s(:call, s(:lit, 1), :+, s(:lit, 2))),
  s(:defn,
   :test_a,
   s(:args),
   s(:call,
    nil,
    :assert_equal,
    s(:call, s(:lit, 1), :+, s(:lit, 2)),
    s(:lit, 4))),
  s(:defn, :test_b, s(:args), s(:nil)),
  s(:defn,
   :test_a,
   s(:args),
   s(:call,
    nil,
    :assert_equal,
    s(:call, s(:lit, 1), :+, s(:lit, 2)),
    s(:lit, 3))),
  s(:call, nil, :private),
  s(:defn,
   :helper_method,
   s(:args, :apples, :oranges, s(:lasgn, :cakes, s(:nil))),
   s(:iter,
    s(:call,
     s(:call,
      s(:array, s(:lvar, :apples), s(:lvar, :oranges), s(:lvar, :cakes)),
      :compact),
     :map),
    s(:args, :food),
    s(:call, s(:call, s(:lvar, :food), :to_s), :upcase)))))
