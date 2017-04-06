# Lab 5: Type Checking

In this lab you will implement type checking in your SLAC
compiler. After this step, you will have completed the front-end of
your compiler. This means that it will be able to reject *all* invalid
programs, and accept *all* valid programs. You will then be able to
turn these valid programs into assembly code that runs on the JVM,
just like "javac" and "scalac" do.

**Update 2 may 2016:** we provide an <a
  href="testprograms_public_typer.zip">archive with test programs</a>
  which you can use to test your type checker (the archive contains a
  subdirectory "lab5").

## Type checking

A valid SLAC program has the following properties:

* It follows the SLAC concrete syntax.
* It respects all the constraints mentioned in [Lab 4](lab4.html).
* Method overriding respects some typing constraints:
  * The overriding method must have exactly as many parameters as the overridden one.
  * The types of the parameters in the overriding and overridden methods must match exactly (no contravariance allowed).
  * The return type must match exactly (no covariance allowed).
* All expressions typecheck and have the expected type (the returned expression matches the declared return type, for instance).

Your goal in this assignment is to enforce all the constraints not
enforced already by the previous phases.

**Note:** The language and the type rules presented in the course may
differ from the rules of SLAC. If there are any differences, please
use the description on the current page for your implementation, and
not the rules in the lecture. Of course, feel free to clarify with
us if you have any questions.

## Types

The following primitive types exist in SLAC (note that we prefix them
with **T** to differentiate them from the tree nodes with the same
name, for instance):

  * **TInt**
  * **TBoolean**
  * **TInt[]**
  * **TString** (We consider **String** to be a primitive type, unlike in Java where it is a proper class. No methods can be called on **String**s, for instance.)
  * **TUnit**

Additionnally, we have class types:

  * **TClass[*name*]**

We define a subtyping relation on these types. All primitive types are
subtypes of themselves and of no other type. For instance:

  * **TInt <: TInt**

All class types are subtypes of themselves and the special "Object"
class type. The subtyping relation is also transitive.

  * **TClass[*name*] <: TClass[*name*]** and **TClass[*name*] <: TClass[**Object**]**
  * **TClass[**B**] <: TClass[**A**]** and **TClass[**C**] <: TClass[**B**]** implies **TClass[**C**] <: TClass[**A**]**

With this in mind, we give some of the non-trivial typing
constraints. This is naturally not an exhaustive list of what you
should implement, but we expect you to be able to deduce the other
rules unambiguously yourself (if in doubt about a rule, ask on KTH
Social).

### Overloaded "+"

The "+" operator can represent integer addition, or string
concatenation. If the types of **e**1 and **e**2 are **T**1 and **T**2
respectively, we have for the type **T**s of **e**1 + **e**2:

  * **T**1 = **TInt** and **T**2 = **TInt** implies **T**s = **TInt**
  * **T**1 = **TString** and **T**2 = **TInt** implies **T**s = **TString**
  * **T**1 = **TInt** and **T**2 = **TString** implies **T**s = **TString**
  * **T**1 = **TString** and **T**2 = **TString** implies **T**s = **TString**

All other values for **T**1 and **T**2 should result in type errors.

### Comparison operator

The "==" operator is also overloaded. Expression `e1 == e2` is type
correct if and only if one of the following two cases applies:

  * `e1` and `e2` have both primitive types, and these types are equal
  * `e1` and `e2` have both class types (in which case they can be different classes)

Note that it is **not** type correct to compare a primitive type to a
class type. Note that strings and arrays of integers are considered
**primitive**!

Consider the following code.

    class A {}
    class B {}

Let e1:T1 and e2:T2. Then the following table summarizes some of the cases for `e1 == e2`.

T1     | T2     | type checks?
--     | --     | ------------
Int    | Int    | yes
String | String | yes
String | A      | no
A      | Int    | no
A      | B      | yes
A      | Int[]  | no
Int    | Int[]  | no

### Method calls

The dereferenced object must be of a class type, and its class must
declare the called method. The number of arguments must of course
match the number of parameters. The passed arguments must have
subtypes of the declared parameters (matching one-by-one).

### Assignment

Assignment of an expression of type **T** can only be done to a
variable of type **S** such that **T <: S**.

Assignment to array elements can only be done through an array
variable, and the index must be an integer expression. The assigned
value must be an integer.

The type of an assignment expression (including array assignment) is
**Unit**.


### Self

**self** is always considered to carry the class type corresponding to
the class where it occurs.


### Returned expression

The returned expression must be of a subtype of the declared return
type.


### The println expression

We will consider **println** calls to be type correct if the argument
is a string. The type of a **println** expression is **Unit**.


### The strOf expression

We will consider **strOf** expressions to be type correct if the
argument is either an integer or a Boolean. The type
of a **strOf** expression is **String**. If you want to accept
**strOf** expressions that convert objects or arrays to strings you
are free to do it, but please let us know in a README file. 


### The while expression

The type of a **while** expression is **Unit**. Its conditional expression must have type **Bool**, and its body must have type **Unit**.


### The main method

The main method is not allowed to take any arguments. Its return type
must be **Unit**.


## Suggested implementation

Here are the steps we suggest you take:

* Modify your analyzer such that it attaches types to the various
  symbols. Since symbols are shared, this has the advantage that you
  can recover the proper type from any occurrence of the symbol.

* Modify your analyzer so that it enforces the overriding type
  constraints on methods.

* Implement your typechecker. Make sure you attach the types to the
  expression subtrees (this will be required for code generation, to
  differentiate between the versions of the overloaded operators, for
  instance).

* While you typecheck expressions, attach the proper symbols to the
  occurrences of method calls (since you can now determine the class
  from which they are called; though, what you are able to determine
  may not match the run-time types).

* Test, test, test!

## User-friendliness

It is very important that your compiler does not stop at the first
type error! **TypeChecker.scala** contains some hints on how to
achieve this. Detect as many errors as possible!

## Stubs

We provide [code stubs](typer-stubs.zip) for your type checker
component. The ZIP archive contains all files of the previous stubs,
plus the following new or changed files:

  * **analyzer/Symbols.scala** (updated) contains an updated version of symbols with types.
  * **analyzer/TypeChecking.scala** (new) contains a stub for the type checker.
  * **analyzer/Types.scala** (new) contains a stub for the types.
  * **ast/Trees.scala** (updated) contains an updated version of trees with symbols and types.
