# Ethogram - Make a catalogue of behavior by your code

*Ethogram* is a testing framework for Common Lisp. This aims to define expressive tests. This refres like testing frameworks for the behavior-driven development but not only aims to that.

## Status

**CURRENTLY CONSIDERING**

## Features

- Several kinds of tests
    - *units*: checks samll target's outputs, mostly without side effects
    - *behaviors*: checks target's outputs with its side effects
    - *scenarios*: like behaviors, but more large and needs more user's point of view?
    - ...and anymore?
- Structurized tests
- Hooks in the lifecyles of tests
- Introspection in the REPL
- Useful modules
    - expectation matchers
        - needless? because of [cl-hamcrest](https://github.com/40ants/cl-hamcrest)?
    - mocking frameworks
        - needless? because of [cl-mock](https://github.com/Ferada/cl-mock) or [mockingbird](https://github.com/Chream/mockingbird)?
    - printing difference between expected and actual
        - I cannot found about this...
- Fast testing
    - but how? by what's infomation?

## How to define tests


With *Ethogram*, tests are defined by `test` macro and its DSL. The basic form of `test` macro is like this:

(WIP: I feel it should be a macro because of avoiding evaluation)

```lisp
(test :testing-context
      "blah blah blah"                   ; a string
  :with (:parallel t                     ; t or nil
         :ordered t                      ; t or nil
  :setup form...
  :teardown form...
  testing-forms-for-its-context...)
```

`test` macro has several ways to define tests via *testing context*. These allows us to describe contexts for what. *Testing contexts* are:

- targets: `:for`, `:about`
- contexts: `:when`, `:in` (in?)
- testcases: `:unit`, `:behavior`, `:scenario`

*Testcases* describe a type of the test describing here. `:unit` describes a small tests for an operator or a small unit, mostly without side effects. `:behavior` describes behaviors of the test targets with side effects. `:scenario` is like `:behavior` but checks more larger, more closer to users points of view.

### Defining a unit

```lisp
(test :unit "for an integer, it returns t"
  test-forms...)
```

### Defining a behavior

```lisp
;; some examples here
(test :behavior "description"
      :with opt-plist
  :step "do step1"                 ; make a label behavior-forms1 below. :step is temporal name...
  behavior-forms1...
  :step "do step2"
  behavior-forms2...)
```

### Defining a scenario

(WIP: useful feature aminig to describe scenarios)

### Structurizing tests

(WIP: Nesting seems ugly. But are there any solution? Annotations like [cl-annot](https://github.com/m2ym/cl-annot)? (Btw [this article](https://y2q-actionman.hatenablog.com/entry/2019/12/20/) discusses about cl-annot's problems.))

```lisp
;; test subject function
(defun my-evenp (n)
  (zerop (mod n 2)))

;; structurize tests
(test :about "A function: my-evenp"        ; is it good that "a function" is keyword, for grouping or something...?
  (test :when "an input is a number"
        :with (:parallel t)
    (test :unit "for an even number, returns t"
      :subject #'my-evenp
      :for 0 :returns t
      :for 2 :returns t)

    (test :unit "for an odd number, returns nil"
      ...))

  (test :when "an input is not a number"
    (test :behavior "it signals a condition"
      ...)))
```

### Lifecycle of test suites

(WIP: it's sufficient?)

*Ethogram* basically runs all tests defined by `relation` and `behavior`. Running tests progress like this:

1. invoke `setup` defined at top-level before running tests
2. for each targets:
    1. invoke `setup` defined in the `target`
    2. for each contexts:
        1. invoke `setup` defined in the `context`
        2. for each tests defined by `relation` and `behavior`:
            1. run the test
        3. invoke `teardown` defined in the `context`
    3. invoke `teardown` defined in the `target`
3. invoke `teardown` after running tests

### Lazy evaluation and memoization

A way to enable lazy evaluation and memoization in most other languages cannot use native assignment syntax. In [RSpec](https://rspec.info) on [Ruby](https://www.ruby-lang.org/), that realized with blocks (leical closures) like this: `let(:name) { obj.make.some.value }`. It's because two reasons; a) a syntax for assignment is native, and b) it cannot be modifiable by users. On the other hand, in Common Lisp, there is no assignment syntaxes anyway. Simply that is appears as binding forms (implemnted as special forms or macros) or `setf`-related forms. So the realization of lazy evaluation and memoization is very eazy.

```lisp
;; `foo` is evaluated when called subject first time, evaluated at once and memoized
(test :about "A function: *10-with-something"
      :let ((foo (make-instance 'foo))
  (test :unit "hoge"
    :subject (lambda (n) (*10-with-something foo n))
    :for 10 :returns 100
    :for 20 :returns 200)))
```

## References

- [Rspec](https://github.com/rspec/rspec-metagem): BDD-supported testing framework for Ruby
- [Ginkgo](https://github.com/onsi/ginkgo): Modern testing framework for Go, including BDD features

## Author

- t-sin (<shinichi.tanaka45@gmail.com>)

## License

*Ethogram* is licensed under [the MIT license](LICENSE).
