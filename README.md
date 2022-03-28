# OOP Is Embarrassing: Examples of Functional Languages Doing It Better

The title of this repository is intentionally confrontational, but it's a bit tongue-in-cheek; I
seek to demonstrate that pure-functional languages are equally or more capable than mainstream
object-oriented languages in clearly encoding object-oriented concepts in precise and modular ways.
Object-oriented concepts aren't on trial so much as so-called object-oriented languages are.

The title is directly inspired By Brian Will's provocative monologue of the same name:
* [Object-Oriented Programming is Bad](https://www.youtube.com/watch?v=QM1iUe6IofM)
* [Object-Oriented Programming is Embarrassing](https://www.youtube.com/watch?v=IRTfhkiAqPw)
* [Object-Oriented Programming is Garbage](https://www.youtube.com/watch?v=V6VP-2aIcSc)

This project seeks to represent plausible engineering problems rather than typical design
thought-experiment toys, though the scope is focused on proving core system design concepts.


## Dependencies

* idris2 --install [oop](https://github.com/alrunner4/idris2-oop).ipkg

## Explore

In general, `:doc` and `:printdef` will get you pretty far, though I try to keep the source
organized literately besides.

### Visitor Pattern
VisitorBob.idr follows the same ordering as the [chapter linked at the top of the module source](
https://web.archive.org/web/20151022084246/http://objectmentor.com/resources/articles/visitor.pdf).
```bash
idris2 --repl VisitorBob.ipkg
```
```
:module VisitorBob
:browse VisitorBob.Vendor
:browse VisitorBob.ClassicVisitor
:browse VisitorBob.AcyclicVisitor
:browse VisitorBob.ReportGenerator
:browse VisitorBob.Decorator
:browse VisitorBob.Extension
```

### Homogenizing Heterogeneous Collections
```bash
idris2 --repl RustDynTrait.idr
```
```
:module RustDynTrait
:browse RustDynTrait
```
