# OOP Is Embarrassing: Examples of Functional Languages Doing It Better

The title of this repository is intentionally confrontational, but it's a bit tongue-in-cheek; I
seek to demonstrate that so-called purely-functional languages are equally or more capable than
mainstream object-oriented languages in clearly encoding object-oriented concepts in precise and
modular ways. Object-oriented concepts aren't on trial so much as object-oriented languages are.

The title is directly inspired By Brian Will's provocative monologue of the same name:
* [Object-Oriented Programming is Bad](https://www.youtube.com/watch?v=QM1iUe6IofM)
* [Object-Oriented Programming is Embarrassing](https://www.youtube.com/watch?v=IRTfhkiAqPw)
* [Object-Oriented Programming is Garbage](https://www.youtube.com/watch?v=V6VP-2aIcSc)

This project seeks realistic engineering problems rather than simply toys, though the scope is
focused on proving system design rather than implementation in complete detail.


## Explore

### Visitor Pattern
```bash
idris2 --repl VisitorBob.ipkg
```
```
:module VisitorBob
:browse VisitorBob
```

### Homogenizing Heterogeneous Collections
```bash
idris2 --repl RustDynTrait.idr
```
```
:module RustDynTrait
:browse RustDynTrait
```
