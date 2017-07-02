[![Latest version](https://index.scala-lang.org/propensive/totalitarian/latest.svg)](https://index.scala-lang.org/propensive/totalitarian)
[![Join the chat at https://gitter.im/propensive/totalitarian](https://badges.gitter.im/propensive/totalitarian.svg)](https://gitter.im/propensive/totalitarian?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.propensive/totalitarian_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.propensive/totalitarian_2.11)

# Totalitarian

*Totalitarian* provides typesafe data structures for working with total (not
partial) functions.

## Disjunctions

It primarily provides the `Disjunct` type, a typesafe representation of a union
type, utilizing a type intersection to represent the branches of the disjunction.

For example, a disjunction of an `Int` and a `String` can be constructed with
```scala
  val disj = Disjunct.of[Int with String](1)
```
or
```scala
  val disj: Disjunct[Int with String] = Disjunct("two")
```
whereas the following would fail to compile:
```scala
  val disj: Disjunct[Int with String] = Disjunct('three)
```
Appropriate least upper-bound types will also be inferred, for example,
```scala
scala> val disjs = List(Disjunct(1), Disjunct('two), Disjunct("three"))
disjs: List[Disjunct[Int with Symbol with String]] = List(1, 'two, three)
```

### Transforming disjunctions

It is also possible to perform transformations on the values in a `Disjunct`.
This is most commonly done using the `when` method, which takes handlers for
each branch of the disjunction as repeated parameters, taking either a lambda
on the disjunction's value, or just a value, like so:
```scala
val disj: Disjunct[Int with String] = Disjunct("two")
disj.when(
  on[Int](42),
  on[String] { s => s"$s!" }
)
```
This returns another new `Disjunct[Int with String]`, in this example,
containing the string `"two!"`. Failure to handle any branch of the disjunction
would result in a compile error, and it is this requirement of totality which
gives *Totalitarian* its name.

The return type does not need to be the same, and the lambdas or values for
each `on`-case may return any type. For example, this,
```scala
val disj: Disjunct[Int with String] = Disjunct("two")
disj.when(
  on[Int] { i => i/2.0 },
  on[String] { s => Symbol(s) }
)
```
would return a new `Disjunct[Double with Symbol]`, in this example, containing
the value `'two`.

If, however, a `when` method transforms a `Disjunct` such that the same type is
returned regardless of its input type, the return value is no longer a
disjunction, and the return type will be the raw type. For example,
```scala
val disj: Disjunct[Int with String] = Disjunct("two")
val s: String = disj.when(
  on[Int]("an integer"),
  on[String] { s => s }
)
```


