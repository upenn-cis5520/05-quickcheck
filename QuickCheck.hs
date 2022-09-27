{-
---
fulltitle: "Type-directed Property Testing"
date: October 3, 2022
---
-}

module QuickCheck where

{-
In this lecture, we will look at [QuickCheck][1], a technique that
cleverly exploits typeclasses and monads to deliver a powerful
automatic testing methodology.

Quickcheck was developed by [Koen Claessen][0] and [John Hughes][11]
more than ten years ago, and has since been ported to other languages
and is currently used, among other things to find subtle [concurrency
bugs][3] in [telecommunications code][4]. In 2010, it received the
[most influential paper award](http://www.sigplan.org/award-icfp.htm)
for the ICFP 2000 conference.

The key idea on which QuickCheck is founded is *property-based
testing*.  That is, instead of writing individual test cases (eg unit
tests corresponding to input-output pairs for particular functions)
one should write *properties* that are desired of the functions, and
then *automatically* generate *random* tests which can be run to
verify (or rather, falsify) the property.

By emphasizing the importance of specifications, QuickCheck yields
several benefits:

1. The developer is forced to think about what the code *should do*,

2. The tool finds corner-cases where the specification is violated,
   which leads to either the code or the specification getting fixed,

3. The specifications live on as rich, machine-checkable documentation
   about how the code should behave.

In this module, we'll import some of QuickCheck's types, type classes
and operators without qualification for convenience. But all of the functions
that we use from this module will be marked by `QC.`
-}

import Control.Monad (liftM2, liftM3)
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    Property,
    Testable (..),
    (==>),
  )
import qualified Test.QuickCheck as QC

{-
Properties
==========

A QuickCheck property is essentially a function whose output is a
boolean.  A standard "hello-world" QC property might be something
about common functions on lists.
-}

prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs ++ ys) == reverse xs ++ reverse ys

{-
That is, a property looks a bit like a mathematical theorem that the
programmer believes is true. A QC convention is to use the prefix `"prop_"`
for QC properties. Note that the type signature for the property is not the
usual polymorphic signature; we have given the concrete type `Int` for the
elements of the list. This is because QC uses the types to generate random
inputs, and hence is restricted to monomorphic properties (those that don't
contain type variables.)

To *check* a property, we simply invoke the `quickCheck` action with the
property. Note that only certain types of properties can be tested, these
properties are all in the 'Testable' type class.

~~~~~{.haskell}
      quickCheck :: (Testable prop) => prop -> IO ()
  	   -- Defined in Test.QuickCheck.Test
~~~~~

`[Int] -> [Int] -> Bool` is a Testable property, so
let's try quickCheck on our example property above. Note that because
`quickCheck` runs in the `IO` monad, you need to use `ghci` to see the
examples in this module. You can start ghci with the command:

          stack ghci QuickCheck.hs
          ghci> import Test.QuickCheck

Once you have done that, you should see a prompt that you can use to evaluate
definitions in the `QuickCheck` module. Try checking the property above.

~~~~~{.haskell}
ghci> quickCheck prop_revapp
~~~~~

   S
   P
   O
   I
   L
   E
   R

   S
   P
   A
   C
   E

   R
   U
   N

   I
   N

   G
   H
   C
   I

   F
   I
   R
   S
   T

What's that ?! Let's run the `prop_revapp` function on the two inputs that
quickCheck identified as counter-examples. (Your counterexamples may differ
from the ones below.)

    ghci> prop_revapp [0] [1]

QC has found inputs for which the property function *fails* ie, returns
`False`. Of course, those of you who are paying attention will realize there
was a bug in our property, namely it should be
-}

prop_revapp_ok :: [Int] -> [Int] -> Bool
prop_revapp_ok xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

{-
because `reverse` will flip the order of the two parts `xs` and `ys` of
`xs ++ ys`. Now, when we run

~~~~~{.haskell}
ghci> quickCheck prop_revapp_ok
~~~~~

you should see

      +++ OK, passed 100 tests.

That is, Haskell generated 100 test inputs and for all of those, the
property held. You can up the stakes a bit by changing the number of tests
you want to run
-}

quickCheckN :: (Testable prop) => Int -> prop -> IO ()
quickCheckN n = QC.quickCheck . QC.withMaxSuccess n

{-
and then ask quickcheck to run more tests.

~~~~~{.haskell}
ghci> quickCheckN 1000 prop_revapp_ok
~~~~~

QuickCheck QuickSort
--------------------

Let's look at a slightly more interesting example. Here is the canonical
implementation of *quicksort* in Haskell. (Some may quibble that this is
actually the quicksort algorithm because it does not modify the list in
place. But it is a reasonable purely functional analogue.)
-}

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort lhs ++ [x] ++ qsort rhs
  where
    lhs = [y | y <- xs, y < x] -- this is a "list comprehension"
    -- i.e. the list of all elements from
    --      xs that are less than x
    rhs = [z | z <- xs, z > x]

{-
Really doesn't need much explanation! Let's run it "by hand" on a
few inputs to see what it does.
-}

-- >>> [10,9..1]

-- >>> qsort [10,9..1]

-- >>> [2,4..20] ++ [1,3..11]

-- >>> qsort $ [2,4..20] ++ [1,3..11]

{-
Looks good -- let's try to test that the output is in
fact sorted. We need a function that checks that a
list is ordered
-}

isOrdered :: Ord a => [a] -> Bool
isOrdered (x : y : zs) = x <= y && isOrdered (y : zs)
isOrdered [_] = True
isOrdered [] = True

{-
and then we can use the above to write a property saying that the
result of qsort is an ordered list.
-}

prop_qsort_isOrdered :: [Int] -> Bool
prop_qsort_isOrdered xs = isOrdered (qsort xs)

{-
Let's test it!

~~~~~{.haskell}
ghci> quickCheckN 1000 prop_qsort_isOrdered
~~~~~

Conditional Properties
----------------------

Here are several other properties that we
might want. First, repeated `qsorting` should not
change the list. That is,
-}

prop_qsort_idemp :: [Int] -> Bool
prop_qsort_idemp xs = qsort (qsort xs) == qsort xs

{-
Second, the head of the result is the minimum element
of the input
-}

prop_qsort_min :: [Int] -> Bool
prop_qsort_min xs = head (qsort xs) == minimum xs

{-
~~~~~{.haskell}
ghci> quickCheck prop_qsort_min
~~~~~

   S
   P
   O
   I
   L
   E
   R

   S
   P
   A
   C
   E

However, when we run this, we run into a glitch.

But of course! The earlier properties held *for all inputs*
while this property makes no sense if the input list is empty!
This is why thinking about specifications and properties has the
benefit of clarifying the *preconditions* under which a given
piece of code is supposed to work.

In this case we want a *conditional properties* where we only want
the output to satisfy to satisfy the spec *if* the input meets the
precondition that it is non-empty.
-}

prop_qsort_nn_min :: [Int] -> Property
prop_qsort_nn_min xs =
  not (null xs) ==> head (qsort xs) == minimum xs

{-
We can write a similar property for the maximum element too.
-}

prop_qsort_nn_max :: [Int] -> Property
prop_qsort_nn_max xs =
  undefined

{-
~~~~~{.haskell}
ghci> quickCheckN 100 prop_qsort_nn_min

ghci> quickCheckN 100 prop_qsort_nn_max
~~~~~

This time around, both the properties hold.

Note that now, instead of just being a `Bool` the output
of the function is now a `Property`, a special type built into
the QC library. Similarly the *implies* operator `==>`
is one of many QC combinators that allow the construction
of rich properties.

Testing Against a Model Implementation
--------------------------------------

We could keep writing different properties that capture
various aspects of the desired functionality of `qsort`.
Another approach for validation is to test that our `qsort`
is *behaviorally* identical to a trusted *reference
implementation* which itself may be too inefficient or
otherwise unsuitable for deployment. In this case, let's
use the standard library's `sort` function
-}

prop_qsort_sort :: [Int] -> Bool
prop_qsort_sort xs = qsort xs == List.sort xs

{-
which we can put to the test

~~~~~{.haskell}
ghci> quickCheckN 1000 prop_qsort_sort
~~~~~

   S
   P
   O
   I
   L
   E
   R

   S
   P
   A
   C
   E

Say, what?!

~~~~~{.haskell}
ghci> qsort [-1,-1]
~~~~~

Ugh! So close, and yet ... Can you spot the bug in our code?

~~~~~{.haskell}
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
  where lhs  = [y | y <- xs, y < x]
        rhs  = [z | z <- xs, z > x]
~~~~~

We're assuming that the *only* occurrence of (the value) `x`
is itself! That is, if there are any *copies* of `x` in the
tail, they will not appear in either `lhs` or `rhs` and hence
they get thrown out of the output.

Is this a bug in the code? What *is* a bug anyway? Perhaps the
fact that all duplicates are eliminated is a *feature*! At any
rate there is an inconsistency between our mental model of how
the code *should* behave as articulated in `prop_qsort_sort`
and the actual behavior of the code itself.

We can rectify matters by stipulating that the `qsort` produces
lists of distinct elements
-}

isDistinct :: Eq a => [a] -> Bool
isDistinct = undefined

prop_qsort_distinct :: [Int] -> Bool
prop_qsort_distinct = isDistinct . qsort

{-
and then, weakening the equivalence to only hold on inputs that
are duplicate-free
-}

prop_qsort_distinct_sort :: [Int] -> Property
prop_qsort_distinct_sort xs =
  isDistinct xs ==> qsort xs == List.sort xs

{-
QuickCheck happily checks the modified properties

~~~~~{.haskell}
ghci> quickCheck prop_qsort_distinct

ghci> quickCheck prop_qsort_distinct_sort

~~~~~

The Perils of Conditional Testing
---------------------------------

Well, we managed to *fix* the `qsort` property, but beware! Adding
preconditions leads one down a slippery slope. In fact, if we paid
closer attention to the above runs, we would notice something

~~~~~{.haskell}
ghci> quickCheckN 10000 prop_qsort_distinct_sort
...
(5012 tests; 248 discarded)
...
+++ OK, passed 10000 tests.
~~~~~

The bit about some tests being *discarded* is ominous. In effect,
when the property is constructed with the `==>` combinator, QC
discards the randomly generated tests on which the precondition
is false. In the above case QC grinds away on the remainder until
it can meet its target of `10000` valid tests. This is because
the probability of a randomly generated list meeting the precondition
(having distinct elements) is high enough. This may not always be the case.

To see why, let's look at another sorting function.

The following code is (a simplified version of) the `insert` function from the
standard library

-}

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y : ys)
  | x <= y = x : y : ys
  | otherwise = y : insert x ys

{-
Given an element `x` and a list `xs`, the function walks along `xs`
till it finds the first element greater than `x` and it places `x`
to the left of that element. Thus

~~~~~{.haskell}
ghci> insert 8 ([1..3] ++ [10..13])
~~~~~

Indeed, the following is the well known [insertion-sort][5] algorithm
-}

isort :: Ord a => [a] -> [a]
isort = foldr List.insert []

{-
We could write our own tests, but why do something a machine can do better?!
-}

prop_isort_sort :: [Int] -> Bool
prop_isort_sort xs = isort xs == List.sort xs

{-
~~~~~{.haskell}
ghci> quickCheckN 1000 prop_isort_sort
~~~~~

Now, the reason that the above works is that the `insert`
routine *preserves* sorted-ness. That is, while of course
the property
-}

prop_insert_ordered' :: Int -> [Int] -> Bool
prop_insert_ordered' x xs = isOrdered (insert x xs)

{-
is bogus,

~~~~~{.haskell}
ghci> quickCheckN 1000 prop_insert_ordered'

ghci> insert 0 [0, -1]
~~~~~

the output *is* ordered if the input was ordered to begin with
-}

prop_insert_ordered :: Int -> [Int] -> Property
prop_insert_ordered x xs =
  isOrdered xs ==> isOrdered (insert x xs)

{-
Notice that now, the precondition is more *complex* -- the property
requires that the input list be ordered. If we QC the property

~~~~~{.haskell}
ghci> quickCheck prop_insert_ordered

~~~~~

<FILL IN WHAT HAPPENS HERE!>

*Aside* the above example also illustrates the benefit of
writing the property as `p ==> q` instead of using the boolean
operator `||` to write `not p || q`. In the latter case, there is
a flat predicate, and QC doesn't know what the precondition is,
so a property may hold *vacuously*. For example consider the
variant
-}

prop_insert_ordered_vacuous :: Int -> [Int] -> Bool
prop_insert_ordered_vacuous x xs =
  not (isOrdered xs) || isOrdered (insert x xs)

{-
QC will happily check it for us

~~~~~{.haskell}
ghci> quickCheckN 1000 prop_insert_ordered_vacuous
~~~~~

Unfortunately, in the above, the tests passed *vacuously*
only because their inputs were *not* ordered, and one
should use `==>` to avoid the false sense of security
delivered by vacuity.

QC provides us with some combinators for guarding against
vacuity by allowing us to investigate the *distribution*
of test cases

~~~~~{.haskell}
label    :: String -> Property -> Property
classify :: Bool -> String -> Property -> Property
~~~~~

We may use these to write a property that looks like
-}

prop_insert_ordered_vacuous' :: Int -> [Int] -> Property
prop_insert_ordered_vacuous' x xs =
  QC.label lbl $
    not (isOrdered xs) || isOrdered (insert x xs)
  where
    lbl =
      (if isOrdered xs then "Ordered, " else "Not Ordered, ")
        ++ show (length xs)

{-
When we run this, we get a detailed breakdown of the 100 passing tests:

~~~~~{.haskell}
ghci> quickCheck prop_insert_ordered_vacuous'
~~~~~

where in the first four lines, `P% COND, N` means that `P` percent of the
ordered inputs had length `N`, and satisfied the predicate denoted by the
string `COND`.

What percentage of lists were ordered? How long were they?  <FILL IN HERE>

Generating Data
===============

Before we start discussing how QC generates data (and how we can help it
generate data meeting some pre-conditions), we must ask ourselves a basic
question: how does QC behave *randomly* in the first place?!

~~~~~{.haskell}
ghci> quickCheck prop_insert_ordered'

ghci> quickCheck prop_insert_ordered'

~~~~~

Eh? This seems most *impure* -- same inputs yielding two totally different
outputs! How does that happen?

The QC library defines a type

   Gen a

of "generators for values of type a".

The impurity of random generation is bottled up inside the 'Gen' type. The
**monad** structure of this type let's us work with this impurity in a
controlled way, but we will get to that. For now, note that these generators
are a powerful mechanism for creating random data and that the QuickCheck
library contains multiple ways of constructing generators.

For example, we can construct a generator using the `chooseInt` function
to generate a random number in a given range:
-}

-- | generate an Int between 1 and 10, inclusive
genSmallInt :: Gen Int
genSmallInt = QC.chooseInt (1, 10)

{-
If you have a generator, you can see what it produces with the `sample` operation:

~~~~~{.haskell}
sample :: Show a => Gen a -> IO ()
~~~~~

This function will show you a sample of the values produced by the generator (and you'll get different values each time).

~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
ghci> sample genSmallInt
3
6
9
8
1
2
4
10
1
7
4
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This operation generates some example values and prints them to stdout.
Alternatively, if you want access to the randomly generated values, the
sample' function will return them to you.
-}

-- >>> QC.sample' genSmallInt
-- [10,5,10,4,8,6,10,3,9,9,9]

{-
Generator Combinators
---------------------

QC comes loaded with a set of combinators that allow us to create
generators for various data structures.

The first of these combinators is `choose`, which is the generalization of `chooseInt.`

~~~~~{.haskell}
choose :: (System.Random.Random a) => (a, a) -> Gen a
~~~~~

This function takes an *interval* and returns an random element from that interval.
(The typeclass `System.Random.Random` describes types which can be
*sampled*. For example, the following is a randomly chosen set of numbers
between `0` and `3`.

-}

-- >>> QC.sample' $ QC.choose (0, 3)
-- [0,3,0,2,1,0,1,2,0,0,2]

{-
A second useful combinator is `elements`

~~~~~{.haskell}
elements :: [a] -> Gen a
~~~~~

which returns a generator that produces values drawn from the input list
-}

-- >>> QC.sample' $ QC.elements [10, 20..100]
-- [70,40,60,100,70,70,80,80,90,60,100]

{-
A third combinator is `oneof`

~~~~~{.haskell}
oneof :: [Gen a] -> Gen a
~~~~~

which allows us to randomly choose between multiple generators
-}

-- >>> QC.sample' $ QC.oneof [QC.elements [10,20,30], QC.choose (0,3)]
-- [30,10,30,2,10,0,10,2,1,30,1]

{-
a related generator is `listOf`

~~~~~{.haskell}
listOf :: Gen a -> Gen [a]
~~~~~

that gives us random lists, where the elements are generated by the argument generator.
-}

-- >>> QC.sample' (QC.listOf (QC.elements [1,2,3]))
-- [[],[3],[2,2,3],[2],[3,2,2,2,1],[2,1,3,2,1,2,1],[3],[1,3,2,1],[2,3,1,1,3],[3,1,1,3,2,1,2,2,1,1,1,3,1,1,3,2,1,1],[3,1,1,1,2,3,1,1,3,2,1,2,2]]

{-
and finally, the above is generalized into the `frequency` combinator

~~~~~{.haskell}
frequency :: [(Int, Gen a)] -> Gen a
~~~~~

which allows us to build weighted combinations of individual generators.
-}

-- >>> QC.sample' $ QC.frequency [(1, QC.elements [1,2]), (5, QC.elements [100,200])]
-- [100,200,100,100,2,100,200,100,100,100,100]

{-
The Generator Monad
-------------------

The parameterized type 'Gen' is an instance of the monad type class. What this
means (for today) is that the monadic operations are available
for constructing new generators. Two of these operators come directly from
the Monad class itself:

~~~~~~~{.haskell}
-- from the class Monad
--
return :: a -> Gen a
(>>=)  :: Gen a -> (a -> Gen b) -> Gen b
~~~~~~~

The next three are from the library [Control.Monad](http://hackage.haskell.org/package/base-4.13.0.0/docs/Control-Monad.html).
These are defined in terms of return and (>>=) above, so they
are available for any type constructor that is an instance of
the Monad class, including Gen.

~~~~~~~{.haskell}
liftM  :: (a -> b) -> Gen a -> Gen b
liftM2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
liftM3 :: (a -> b -> c -> d) -> Gen a -> Gen b -> Gen c -> Gen d
~~~~~~~

The `lift` in these names comes from an analogy: we are taking normal functions
and "lifting" them to work with generators. For example, `liftM` takes any
regular function of type `a -> b` and converts it to be a function of
type `Gen a -> Gen b`.

Note, `liftM` above has another name---`fmap`.  That's right, every monad is
also a functor. Furthermore, the infix operator `(<$>)` is yet another name
for `fmap` that can look nice in your definitions.

We will cover what it exactly means for `Gen` to be a monad in a future
lecture. However, as we will see, these operations let us put generators
together compositionally.
-}

genThree :: Gen Int -- a generator that always generates the value '3'
genThree = return 3

genPair :: Gen a -> Gen b -> Gen (a, b)
genPair = liftM2 (,) -- a generator for pairs

{-
Generator Practice
------------------

Use the operators above to define generators. Make sure that you test them out
with `sample` to make sure that they are what you want.
-}

genBool :: Gen Bool
genBool = undefined

genTriple :: Gen a -> Gen b -> Gen c -> Gen (a, b, c)
genTriple = undefined

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe ga = undefined

{-
The Arbitrary Typeclass
-----------------------

To keep track of all these generators, QC defines a typeclass containing types
for which random values can be generated!

~~~~~{.haskell}
class Arbitrary a where
  arbitrary :: Gen a
~~~~~

Thus, to have QC work with (ie generate random tests for) values of type
`a` we need only make `a` an instance of `Arbitrary` by defining an
appropriate `arbitrary` function for it. QC defines instances for base
types like `Int` , `Float`, etc

~~~~~{.haskell}
ghci> sample (arbitrary :: Gen Int)
~~~~~

and lifts them to compound types.

~~~~~{.haskell}
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (a,b,c) where
  arbitrary = liftM3 (,,) arbitrary arbitrary arbitrary
~~~~~

-}

-- >>> QC.sample' (arbitrary :: Gen (Int,Float,Bool))

-- >>> QC.sample' (arbitrary :: Gen [Int])

{-
However, you'll need to make your own instances of `Arbitrary` for user
 defined datatypes. As we'll discuss below, there are two many options in
 generation for GHC to make this class automatically derivable. Below, we will
 walk through constructing a good generator for the list type as an example of
 constructing a good generator for an arbitrary datatype. (This code is just an
 example --- if you need to generate a list, you can use the `listOf` function
 from the library.) After reading this section, challenge yourself to write
 a generator for a `Tree` type.

Generating Lists
----------------

We can use the above combinators to write generators for lists
-}

genList1 :: (Arbitrary a) => Gen [a]
genList1 = liftM2 (:) arbitrary genList1

-- >>> QC.sample' (genList1 :: Gen [Int])

{-
Can you spot a problem in the above?

<FILL IN HERE>

Let's try again,
-}

genList2 :: (Arbitrary a) => Gen [a]
genList2 =
  QC.oneof
    [ return [],
      liftM2 (:) arbitrary genList2
    ]

-- >>> QC.sample' (genList2 :: Gen [Int])

{-
This is not bad, but there is still something undesirable.
What is wrong with this output?

<FILL IN HERE>

This version fixes the problem. We only choose `[]` one eighth of the time.
-}

genList3 :: (Arbitrary a) => Gen [a]
genList3 =
  QC.frequency
    [ (1, return []),
      (7, liftM2 (:) arbitrary genList3)
    ]

-- >>>  QC.sample' (genList3 :: Gen [Int])

{-
However, `genList3` has the opposite problem --- it generates a lot of long
lists (longer than length 2 or 3) but not so many short ones. But finding bugs
with shorter lists is a lot faster than finding bugs with long lists.

So, two last tweaks. We let quickcheck determine what frequency to use, and we
decrease the frequency of cons with each recursive call.  For the former, we
rely on the following function from QC library.

         sized :: (Int -> Gen a) -> Gen a

This function is higher-order; it takes a generator with a size parameter
 (i.e. the Int) and uses it to develop a new generator by progressively
 increasing this size.

For the latter, when we define this "size-aware" function, we cut the size in
half for each recursive call.
(Note: to give a type annotation for the local definition `gen`,
we have to bring the type variable `a` into scope with the `forall`
keyword.)
-}

genList4 :: forall a. (Arbitrary a) => Gen [a]
genList4 = QC.sized gen
  where
    gen :: Int -> Gen [a]
    gen n =
      QC.frequency
        [ (1, return []),
          (n, liftM2 (:) arbitrary (gen (n `div` 2)))
        ]

{-
Now look at that distribution! Not too small, not too big, not too many empty lists.

-}

-- >>> QC.sample' (genList4 :: Gen [Int])

{-
I encourage you to look at the implementation of `genList4` closely. This use
of `frequency` and `sized` is particularly important to controlling the
generation of tree-structured data.

For practice, see if you can generate arbitrary trees using the pattern shown
above in `genList4`.

-}

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Foldable)

genTree :: Arbitrary a => Gen (Tree a)
genTree = QC.sized gen
  where
    gen n = undefined

-- >>> QC.sample' (genTree :: Gen (Tree Int))

{-
Shrinking
---------

When properties fail, QuickCheck provides a counterexample. But sometimes
this counterexample could be rather complex and not much use in finding
your bug.

As an example, consider this buggy function that adds together all of the
values stored in a tree of integers. I've added an "optimization" to this
function to make it super fast.
-}

treeSum :: Tree Int -> Int
treeSum Empty = 0
treeSum (Branch x l r) = if x == 0 then 0 else treeSum l + x + treeSum r

{-
Can you see the bug? The special case of 0 would be great if we were
multiplying the values in the tree but computes the wrong answer for addition.

It turns out that, because I derived `Foldable` above, there is already
an overloaded function `sum` that I can use to sum up the tree values.
So I can use `sum` as the correct version and design a property to test
my `treeSum` function:
-}

prop_treeSum :: Tree Int -> Bool
prop_treeSum t = treeSum t == sum t

{-
Now, let's see what happens when I use QuickCheck. Oof! It took a few
tests, but it eventually generated a tree with a 0 value that failed
the property.

~~~~~~~~~~~{.haskell}
ghci> quickCheck prop_treeSum
*** Failed! Falsified (after 13 tests):
Branch (-6) (Branch 2 (Branch (-3) Empty (Branch 12 Empty Empty)) (Branch 11 Empty Empty)) (Branch 0 (Branch 2 (Branch 6 Empty Empty) Empty) (Branch 11 (Branch (-9) Empty Empty) (Branch 4 Empty Empty)))
~~~~~~~~~~~~

But, this counterexample doesn't really help me find the bug in my code.

However, I can ask quick check to produce not just a counterexample, but a small
counterexample, by explaining how to *shrink* data structures. The general idea is that
if quickcheck finds a random tree that fails the property, it can apply the shrinking function
to produce "smaller variants" of that tree, and then check those too. By repeated shrinking,
it can generate smaller and smaller trees until it finds the smallest one that fails, and that
is the one that it will return.

Here is how to shrink the Tree type shown above.
-}

-- | Produce some smaller trees
shrinkTree :: Arbitrary a => Tree a -> [Tree a]
shrinkTree (Branch x l r) =
  [l, r] -- left and right subtrees are smaller
    ++ map (\l' -> Branch x l' r) (shrinkTree l) -- shrink left subtree
    ++ map (\r' -> Branch x l r') (shrinkTree r) -- shrink right subtree
    ++ map (\x' -> Branch x' l r) (shrink x) -- shrink the value
shrinkTree Empty = []

{-
If the tree is a "Branch" then there are *many* potentially smaller trees to explore. These
trees are still similar to the original tree, so maybe if there was a bug in the original
tree then there might be a bug in some of the smaller trees too.
-}

-- >>> shrinkTree (Branch 0 (Branch 1 Empty Empty) (Branch 1 Empty Empty))
-- [Branch 1 Empty Empty,Branch 1 Empty Empty,Branch 0 Empty (Branch 1 Empty Empty),Branch 0 Empty (Branch 1 Empty Empty),Branch 0 (Branch 0 Empty Empty) (Branch 1 Empty Empty),Branch 0 (Branch 1 Empty Empty) Empty,Branch 0 (Branch 1 Empty Empty) Empty,Branch 0 (Branch 1 Empty Empty) (Branch 0 Empty Empty)]

-- >>> map prop_treeSum (shrinkTree (Branch 0 (Branch 1 Empty Empty) (Branch 1 Empty Empty)))
-- [True,True,False,False,False,False,False,False]

{-
Now, let's test our property with shrinking. We can use the `forAllShrink` function to
tell QuickCheck how to shrink.

~~~~~~~~~~~{.haskell}
ghci> quickCheck (forAllShrink genTree shrinkTree prop_treeSum)
*** Failed! Falsified (after 6 tests and 5 shrinks):
Branch 0 Empty (Branch 1 Empty Empty)
~~~~~~~~~~~

Success! We have a counterexample that is much easier to understand.
(And observe that any smaller version of this tree would not be
a counterexample!)

Because shrinking is so important, QuickCheck includes the shrinking
function as an optional member of the `Arbitrary` class. What this means
is that if we make an instance of `Arbitrary` for the `Tree` type, using
the generator and shrinking function we have just defined:
-}

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = genTree
  shrink = shrinkTree

{-
then we can test our property with shrinking directly.

~~~~~~~~~~~{.haskell}
ghci> quickCheck prop_treeSum
*** Failed! Falsified (after 4 tests and 3 shrinks):
Branch 0 (Branch 1 Empty Empty) Empty
~~~~~~~~~~~

Generating data that satisfies properties
-----------------------------------------

What if we want to generate not just lists but *ordered* lists?

We can build a custom generator for ordered lists by mapping
the `sort` function over the standard generator for lists
(via the overloaded `arbitrary` operation).
-}

genOrdList :: (Arbitrary a, Ord a) => Gen [a]
genOrdList = fmap List.sort arbitrary

-- >>> QC.sample' (genOrdList :: Gen [Int])
-- [[],[-1,2],[-4,1,4],[2],[-2,4,6,6],[],[-7,-4,-4,1,1,3,8,10],[-14,-1,7],[-16,-15,-10,-6,-5,-1,1,6,7,7,8,10,12,14,16],[-12,-11,-7,-6,7,14,14,17],[-17,-12,-10,-7,-1,4,6,20]]

{-
This is also a place I like to use the `<$>` operator. This notation captures
the idea that we are applying the function to every generated value in
the `Gen` monad.
-}

genOrdList' :: (Arbitrary a, Ord a) => Gen [a]
genOrdList' = List.sort <$> genList4

{-
NOTE: Above, just saying `sort genList4` doesn't work. We have that `genList4`
is a generator for lists, not a list itself. Because `Gen` is a functor, the
right way to compose generation with a transformation is to use `fmap`.

To *check* the output of a custom generator we can use the `forAll` combinator

~~~~~{.haskell}
forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
~~~~~

For example, we can check that in fact, the combinator only produces
ordered lists

~~~~~
ghci> quickCheck $ forAll genOrdList isOrdered
~~~~~

and now, we can properly test the `insert` property
-}

prop_insert :: Int -> Property
prop_insert x = QC.forAll genOrdList $ \xs ->
  isOrdered xs && isOrdered (insert x xs)

{-
~~~~~
ghci> quickCheck prop_insert
~~~~~

Using `newtype` for smarter test-case generation
------------------------------------------------

This works very well, but we might not want to write `forAll genOrdList`
everywhere we want to test a property on ordered lists only.  In order to get
around that, we can define a new type that *wraps* lists, but has a different
`Arbitrary` instance:
-}

newtype OrdList a = OrdList [a] deriving (Eq, Ord, Show, Read)

instance (Ord a, Arbitrary a) => Arbitrary (OrdList a) where
  arbitrary = fmap OrdList genOrdList

{-
This says that to generate an arbitrary `OrdList`, we use the `genOrdList`
generator we just defined, and package that up.
-}

-- >>> QC.sample' (arbitrary :: Gen (OrdList Int))

{-
Now, we can rewrite our `prop_insert` function more simply:
-}

prop_insert' :: Int -> OrdList Int -> Bool
prop_insert' x (OrdList xs) = isOrdered $ insert x xs

{-
And in fact, QuickCheck already has this type built in as [OrderedList](https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck.html#OrderedList).

This technique of using `newtype`s for special-purpose instances is very
common, both in QuickCheck and in other Haskell libraries.

QuickCheck outside of Haskell
-----------------------------

As a testing tool, QuickCheck has been ported to many languages, some of which are
listed on its wikipedia page [13]. Haskell's type classes (and monads) mean that
the implementation of QuickCheck in Haskell is surprisingly simple.

-------------------------------------------------------------------------

Credit: This lecture based on [12].

[0]: http://www.cse.chalmers.se/~koen/
[1]: http://www.cse.chalmers.se/~rjmh/QuickCheck/
[2]: http://www.cs.york.ac.uk/fp/smallcheck/
[3]: http://video.google.com/videoplay?docid=4655369445141008672#
[4]: http://www.erlang-factory.com/upload/presentations/55/TestingErlangProgrammesforMulticore.pdf
[5]: http://en.wikipedia.org/wiki/Insertion_sort
[6]: http://hackage.haskell.org/packages/archive/QuickCheck/latest/doc/html/src/Test-QuickCheck-Gen.html#Gen
[7]: http://book.realworldhaskell.org/read/monads.html
[8]: http://book.realworldhaskell.org/read/testing-and-quality-assurance.html
[11]: http://www.cse.chalmers.se/~rjmh
[12]: http://cseweb.ucsd.edu/classes/wi11/cse230/lectures/quickcheck.lhs
[13]: https://en.wikipedia.org/wiki/QuickCheck
-}
