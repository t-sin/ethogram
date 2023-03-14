# Ethogram - A testing framework for Common Lisp

*Ethogram* is a testing framework for Common Lisp. This aims to define expressive tests. This refres like testing frameworks for the behavior-driven development but not only aims to that.

## Status

**CURRENTLY CONSIDERING**

## Features

- Several ways to define tests
- Grouping some tests
- Useful expectation matchers
- (Basic mocking frameworks?) (...is what?)

## How to define tests

*Ethogram* has several ways to define tests. One of them is for define a relation between inputs and outputs of an operator. Second one of them is for define a behavior of some operators. (and more test types...?.)

The definitions for a system are composed of some groups like *target* and *context*.

### Defining a relation

A DSL for `test-forms` is TBD. Should it be implemented as another operator...? I think expectation matchers should be separated `relation`/`behavior` and be extensible.

```lisp
(relation "for an integer, it returns t"
  test-forms...)
```

### Defining a behavior

A DSL for `test-forms` is TBD.

```lisp
;; some examples here
(behavior "description"
  test-forms...)
```

### Structurizing tests

```lisp
;; test subject function
(defun my-evenp (n)
  (zerop (mod n 2)))

;; structurize tests
(target "A function: my-evenp"
  (context "when an input is a number"
    (relation "for an even number, returns t"
      :subject #'my-evenp
      :for 0 :returns t
      :for 2 :returns t)

    (relation "for an odd number, returns nil"
      ...))

  (context "when an input is not a number"
    (relation "it signals a condition"
      ...)))
```

### Lazy evaluation and memoization

A way to enable lazy evaluation and memoization in most other languages cannot use native assignment syntax. In RSpec on Ruby, that realized with blocks (leical closures) like this: `let(:name) { obj.make.some.value }`. It's because two reasons; a) a syntax for assignment is native, and b) it cannot be modifiable by users. On the other hand, in Common Lisp, there is no assignment syntaxes anyway. Simply that is appears as binding forms (implemnted as special forms or macros) or `setf`-related forms. So the realization of lazy evaluation and memoization is very eazy.

```lisp
;; `foo` is evaluated when called subject first time, evaluated at once and memoized
(target "A function: *10-with-something"
  (let.lazy ((foo (make-instance 'foo)))
    (relation "hoge"
      :subject (lambda (n) (*10-with-something foo n))
      :for 10 :returns 100
      :for 20 :returns 200)))
```

## Author

- t-sin (<shinichi.tanaka45@gmail.com>)

## License

*Ethogram* is licensed under [the MIT license](LICENSE).
