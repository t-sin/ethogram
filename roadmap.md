# A Roadmap for *Ethogram*

1. v0.1.0
    - `test` macro
    - testing context `:function FUNCTION-DESIGNATOR`
        - with keywords `:for ARGLIST`
        - with keywords `:returns EXPECTED`
2. v0.2.0
    - testing context `:about DESC` to group test cases
    - pretty printing for test structures
3. v0.3.0
    - testing context `:action FORM`
        - with keyword `:subject FORM`
    - introduce keywords for `test` macro
        - `:before FORM`
        - `:after FORM`
4. v0.4.0
    - testing context `:when` to group with bound values
5. v0.5.0
    - introduce keywords for `:action`
        - `:事前に EXPECTED`
        - `:事後に  EXPECTED`
6. v0.6.0
    - testing context `:behavior DESC`
        - with keywords `:step DESC`, `:expect FORM`
7. v0.7.0
    - testing context hook API
    - testing context extension API
    - input difference pretty printing feature (as other library?)

considering...

8. v0.8.0
9. v0.9.0
10. v1.0.0
    - documentation must be included at this version
