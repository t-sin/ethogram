# Ethogram - A catalog of behaviors by your code

*Ethogram* is a testing framework for Common Lisp. *Ethogram* aims to be a tool assists your quick development in terms of TDD/BDD.

*Ethogram* can:

- describe specification as a set of usases called *examples*
    - *example* is a term of Behavior-driven development
- check if your code satisfies that specification
- redefine that specification in your REPL
- automate this describe-write-check-redefine iteration
    - by `bundle exec guard` like something

## Status

**CURRENTLY UNDER DEVELOPMENT ALONG [A ROADMAP](roadmap.md)**

## Design Philosophy

以下のこれは下書き。

1. In *ethogram*, *catalog* is a set of expectations for the target code
    1. expectations are like these:
        1. example arguments/returned values for a function
        2. pre/post conditions before/after side effects by a functoin
        3. state transitions by some codes
2. In *ethogram*, *catalogs* are not a specification
    1. to write a specification, DO NOT USE *ethogram*
    2. but *eghogram* allows to use catalogs as a sample code to understand the target code
3. *Ethogram* provides a DSL to express your intentions clearly
4. *Ethogram* DO NOT provides complex, taking more time to write DSL
5. Catalogs defined by *ethogram* can be run in REPL intermediately, and same for redefining

...ってコト！？

## Author

- t-sin (<shinichi.tanaka45@gmail.com>)

## License

*Ethogram* is licensed under [the MIT license](LICENSE).
