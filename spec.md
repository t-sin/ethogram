# A spec of Ethogram testing framework

## Status

**CURRENTLY WRITING**

### TODO

- [x] To define what has done and what is not treated by *Ethogram*
- [x] To introduce all *Ethogram*'s features
- [ ] To introduce details for all features

## What should be PROVIDED by *Ethogram*

These must be implemented in *Ethogram*

1.  a high-level testing framework to define these:
    1. test subjects type
    2. context hierarchy
    3. point of view for tests defined
    4. hooks in each steps in running tests
2.  some supports for the behavoir-driven development
3.  a DSL to test a simple boolean form or function
    - NOTE: see *NOT BE PROVIDED 1*
    - it will need small extention for running test part
4. some operations for introspecting tests themsselves **in REPL**

### What should NOT BE PROVIDED by *Ethogram*

These may not implemented in *Ethogram* may be provided as another library or using some existing libraries.

1. an expectation describing framework
    - needless? because of [cl-hamcrest](https://github.com/40ants/cl-hamcrest)?
2. a mocking framework
    - needless? because of [cl-mock](https://github.com/Ferada/cl-mock) or [mockingbird](https://git
3. a data templating framework
    - needed like [factory_bot](https://github.com/thoughtbot/factory_bot) for Ruby
    - but not found now
4. a pretty printing feature for the values between expected and actual
    - I cannot found about this...

### Future work

1. speed of running tests
2. loading/unloading test subjects? (needed?)

## Features

- A DSL to define tests: a `test` macro
    - Several kinds of *test contexts*
        - for test subjects: a funtion, a macro or a form
        - for test structurizing: 1) `about`, 2) `when` ...?
        - for points of view of test for: an unit test, a side-effect or a behavior
    - Hooks in the lifecyles of tests
        - before a context, after a context or any other?
- Introspection API for our REPL
- Check if the subjects satisfies some expectations by a boolean value returned

## Basic concepts
### Testing contexts
#### Test types
#### Test subjects
#### Structurizing tests
### Lifecycle of testing contexts
## Describing tests with `test` macro
## Describing expectations
## Hooks in running tests
## Introspection in the REPL
## Extending or defining a testing contexts

## References

- [Rspec](https://github.com/rspec/rspec-metagem): BDD-supported testing framework for Ruby
- [Ginkgo](https://github.com/onsi/ginkgo): Modern testing framework for Go, including BDD features

-----

OLD:

## How to define tests

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
  :expect :before (zero (length (all-books)))
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
