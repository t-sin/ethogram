# A Roadmap for *Ethogram*

## Short Goals

- v0.1.0
    - define specifications with `defspec` macro
        - specifying spec subject with `:subject FORM`
        - binding names with `:let NAME := INITFORM` (INITFORM is optional)
    - define function examples with `examples :function` macro
        - this is a set of input and expected ouput pairs
    - define sideeffect examples with `examples :sideeffect` macro
        - this is a pair of pre/post checking with subject form
    - MAYBE: tested all features by *Ethogram* itself
- v0.2.0
    - define binding examples with `examples :let` macro
        - this is a set of variable definitoins using anywhere in this examples

and so on...

## Future Goals

- vX.Y.1
    - introduce expectation describing language
        - matchers and something

- vX.Y.2
    - define usecase examples with `examples :usecase` macro
        - this is a list of sideeffects and checkings
        - to check a state transition
        - to check large sideeffects
        - to check a behavior at user's point of view

- vX.Z.0
    - introduce property-based testing feature
        - prpoerties are defined with `property` macro
