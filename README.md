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
My focus with this contrasting implementation is in demonstrating that nothing about the Visitor
pattern is inherently related to inheritance, and that especially in simple cases the use of
inheritance is merely a convoluted encoding of sum types. This doesn't include specific contrasts
with the many flavors of "visitation" recognized in pure functional languages such as the
distinction between Foldable and Traversable, though I did provide some new formalisms in the
[oop](https://github.com/alrunner4/idris2-oop) package to take issue with the common notion that
these patterns can't be codified into libraries.

Some of the improvement I consider to be the aesthetic difference between the Java-like languages
and Idris' hewing to more modern sensibilities around removing line noise punctuation, but I also
think that stripping-away of superfluity is part of what enables the simpler solution to be seen:
when you have a closed set of implementations, a sum type is simpler than twisting yourself into a
double dispatch mechanism just to avoid the syntax for checking the cases of an enumeration.

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
idris2 RustDynTrait.idr
```
```
:browse RustDynTrait
```

### Specification
From [Wikipedia](https://en.wikipedia.org/wiki/Specification_pattern):
> A specification pattern outlines a business rule that is combinable with other business rules.

Wikipedia gives a simple boolean predicate combinator example, the domain logic for which is again
largely obscured by inheritance schemes. The listed examples clock in with the following
non-punctuation line counts:

* C#: 57 source lines
* C# 6.0 with generics: 49 source lines
* Python: 34 source lines
* C++: 86 source lines
* TypeScript: 44 source lines

In contrast, the Idris implementation achieves all the same in _12 source lines_. Much of the time I
find my Idris code tending toward a higher token-per-line ratio than in the C family of languages,
but in this case, I think both reduced line count and line width in this sample dramatically
increase comprehensibility of the domain encoding.

```bash
idris2 SpecificationPattern.idr
```
```
:browse SpecificationPattern
```

### Template Method
This example is another included to demonstrate that these patterns that are commonly presented in
object-oriented design contexts as hinging on inheritance are again simpler without, so long as your
language is smart enough to support closures.

```bash
idris2 TemplateMethod.idr
```
```
:browse TemplateMethod
```
