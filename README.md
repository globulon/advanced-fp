# Advanced FP

## Haskell Categorical

- [ ] GADT's
- [X] Yoneda Lemna (What You Needa know about Yoneda (G. Boisseau, J. Gibbons))
- [X] Profunctor
- [ ] Isomorphisms (Reason Isomorphically! (R. Hinze, DWH Jame))

## Optics

- [ ] Lenses
- [ ] Prisms

## Sorting

Exploring the content from the Third Summer School on Advanced Functional Programming, 
held in Braga, Portugal, in September 1998


- [ ] Part One Sorting Morphisms
  - [ ] Lists
      - [X] Catamorphism
      - [X] Anamorphism
      - [X] Hylomorphism
      - [ ] Sorts
  - [ ] Trees
      - [ ] Catamorphism
      - [ ] Anamorphism
      - [ ] Hylomorphism
      - [ ] Sorts
  - [ ] Paramorphisms  
- [ ] Generic Programming: An Introduction
- [ ] Generic Program Transformation
_ [ ] Designing and Implementing Combinator Languages

The first part of the book explore the the different types of morphisms as 
a pattern of behaviour that does not functionally include the categorical dimension
that we usually abstract code wise

In a first step we will implement the simple versions per structure using the
simplified approach proposed in the first part 

#### Catamorphism (fold)
A catamorphism destructs iteratively a recursive structure T
while constructing a new type U   
That effort requires an Algebra that provides a default value of type U
when the recursive structure is empty and an operation to construct the
new structure value of type U

```haskell
data Algebra t u = Algebra {
  empty :: u,
  fold :: t -> u -> u
}
```
 
#### Anamorphism(unfold)
An anamorphism construct iteratively a new recursive structure T
by destroying a structure of type U in multiple steps.
This operation is the counter part (co-algebra) of the catamorphism
and the destruction operation makes use of a co product to signify the
data structure instance being destroyed cannot "generate" any instance
of type T anymore
 
```haskell
newtype CoAlgebra x u = CoAlgebra {
  unfold :: u -> Either () (x, u)
}
``` 



