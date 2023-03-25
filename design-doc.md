# Design Doc for *Ethogram* testing framework

## Author

- t-sin (<shinichi.tanaka45@gmail.com>)

## Status

CURRENTLY WRITING

### TODO

- [ ] Basic concepts
- [ ] Required features
- [ ] Detailed DSL definitions for each testing contexts

## WIP: Objective

### Motivation

### Required features

- Several kinds of test contexts
    - for test subjects
    - for test structurizing
    - for points of view of test
- Hooks in the lifecyles of tests
- Introspection in the REPL
- Useful modules
    - expectation matchers
        - needless? because of [cl-hamcrest](https://github.com/40ants/cl-hamcrest)?
    - mocking frameworks
        - needless? because of [cl-mock](https://github.com/Ferada/cl-mock) or [mockingbird](https://github.com/Chream/mockingbird)?
    - printing difference between expected and actual
        - I cannot found about this...
    - test data templating
        - like [factory_bot](https://github.com/thoughtbot/factory_bot) for Ruby
- Fast testing
    - but how? by what's infomation?

### Alternatives considered (needed?)

## WIP: Required Specs

### Basic concepts 

With *Ethogram*, tests are defined by `test` macro and its DSL. The basic form of `test` macro is like this:

```lisp
(test :testing-context "description about the tested here"
  :subject form
  :input form
  :expect form)
```

`test` is a macro to prevent evaluation. A DSL for `test` is detailed below:

```lisp
(test :testing-context "describe what's tested"
  [:subject form]
  [:input form]
  [:expect (matcher-form or form)]
  [:with (:parallel t          ; t or nil
          :ordered t           ; t or nil
          ...)]
  [:setup form...]
  [:teardown form...])
```

`test` macro has several ways to define tests via *testing contexts*. These allows us to describe contexts for what. *Testing contexts* are:

- targets: `:for`, `:about`
- contexts: `:when`, `:in` (in?)
- testcases: `:unit`, `:action`, `:behavior`

*Testcases* describe a type of the test describing here. `:unit` describes a small tests for **an operator** or **a small unit**, mostly without side effects. `:action` describes some **affection caused by the test subject**. `:behavior` describes the **behaviors about a functionality** expected in a target system.

### Defining an unit

```lisp
(defun my-integer-p (n) ...)  ; a test subject

(test :about "A function my-integer-p returns t for integers"
  (test :unit #'my-integer-p
    :input 0 :expect t)
  (test :unit #'my-integer-p
    :input 10 :expect t)
```

### Defining an action

```lisp
(defun all-books () ...)
(defun create-book (title author) ...)  ; a test subject

(test :action "A book created"
  :subject #'create-book
  :input ("The Hitchhiker's Guide to Galaxy" "Daglas Adams")
  :expect :before(zero (length (all-books)))
  :expect :after (= (length (all-books)) 1))
```

### Defining a behavoir

```lisp
(test :about "The book database application"
  (test :behavoir "The user registers them book"
    :step "The user gets an empty book list"         ; make a label behavior-forms1 below. :step is temporal name...
    forms...

    :step "The user registers a book"
    forms...

    :step "The user checks that the book is registered"
    forms...)

  (test :bahavoir "The user searches a book"
    ...))
```

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

## WIP: High-level architecture

## WIP: Module overview

## WIP: Internal specs

## WIP: Implementation

### Directory structure
### Distribuution format

## WIP: Usage in some example cases

## WIP: Points of view from testing

## References

### Existing high-level testing frameworks for other languages

- [Rspec](https://github.com/rspec/rspec-metagem): BDD-supported testing framework for Ruby
- [Ginkgo](https://github.com/onsi/ginkgo): Modern testing framework for Go, including BDD features

### Existing testing frameworks for Common Lisp

### About Design Docs

- ["How to write Design Doc" (Japanese)](https://zenn.dev/shimakaze_soft/scraps/7a8170927a30cd)

## Memo

## Revisions

1. 2023-xx-xx: WRITING
