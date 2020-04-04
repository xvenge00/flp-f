# FLP - Functional project
Converts Extended Finate Automata to Deterministic Finate Automata.

## Build
To build the `rka-2-dka` executable use
```
make
```

## Tests
To run tests use
```
make test
```

`test/` directory contains test script with test cases.
`in` file contains input, `ref` file contains reference output.

## Final State Automata

Final State Automata (FSA) must be in format:
```
<state list>\n
<alphabet>\n
<start state>\n
<final states list>\n
<rule 1>\n
...
<rule N>\n
```

FSA doesn't have to have any rules.
`<state list>` must be non-empty and must contain `<start state>`.
FSA without `<start state>` is not considered valid.
Both `<final states>` and `<alphabet>` can be empty.

