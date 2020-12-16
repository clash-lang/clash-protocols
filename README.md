<!-- omit in toc -->
# Clash Protocols
A battery-included library for writing on-chip protocols, such as AMBA AXI and Altera Avalon.

<!-- omit in toc -->
# Table of Contents
- [Introduction](#introduction)
- [Using `Dfs`/`Df` `Circuit`s](#using-dfsdf-circuits)
  - [Invariants](#invariants)
    - [Note [Deasserting resets]](#note-deasserting-resets)
  - [Tutorial: `catMaybes`](#tutorial-catmaybes)
    - [Implementation](#implementation)
    - [Testing](#testing)
    - [Debugging](#debugging)
  - [Connecting multiple circuits](#connecting-multiple-circuits)
- [License](#license)
- [Project goals](#project-goals)
- [Contributing](#contributing)
- [TODO](#todo)

# Introduction
`clash-protocols` exists to make it easy to develop and use on-chip communication protocols, with a focus on protocols in need of bidirectional communication, such as _AMBA AXI_. To familiarize yourself with `clash-protocols`, read [hackage.haskell.org/package/clash-protocols](http://hackage.haskell.org/package/clash-protocols). To read the next section, read at least:

* `Protocols`
* `Protocols.Df.Simple`

The next section will guide you through the creation of a single `Dfs` based circuit.

# Using `Dfs`/`Df` `Circuit`s
The basic handshaking of `Dfs` and `Df` are heavily inspired by _AMBA AXI_:

 * `Df` circuits _send_ data to their right hand side
 * `Df` circuits _receive_ data from their left hand side.
 * `Df` circuits _send_ acknowledgments to their left hand side
 * `Df` circuits _receive_ acknowledgments from their right hand side

## Invariants

The protocols `Df` and `Dfs` impose a contract each component should follow. These are, where possible, checked by the various test harnesses in `Protocols.Hedgehog`.

* _Fwd a_ cannot depend on the _Bwd a_. In other words, deciding whether or not to send data cannot depend on the acknowledgment of that same data.

* A component may not assert an acknowledgment while its reset is asserted. Doing so can lead to data loss. For example, imagine two circuits _A_ and _B_ both driven by different resets and connected as follows:

  ```
      Reset domain A                             Reset domain B

  +----------------------+                   +----------------------+
  |                      |                   |                      |
  |                      |                   |                      |
  |    +-----------+     |                   |    +------------+    |
  |    |           |     |       Fwd a       |    |            |    |
  | +->+           +----------------------------->+            +--> |
  |    |           |     |                   |    |            |    |
  |    |           |     |                   |    |            |    |
  |    |           |     |                   |    |            |    |
  |    |           |     |                   |    |            |    |
  |    |           |     |       Bwd a       |    |            |    |
  | <--+           +<-----------------------------+            +<-+ |
  |    |           |     |                   |    |            |    |
  |    +-----------+     |                   |    +------------+    |
  |                      |                   |                      |
  |                      |                   |                      |
  +----------------------+                   +----------------------+
  ```

  If:
    * Circuit _A_ drives data; and
    * Circuit _B_'s reset is asserted; and
    * Circuit _B_ sends an acknowledgment

  ..circuit _A_ will consider its data send and will not try again on the next cycle. However, _B_'s reset is asserted so it will most likely not process anything. Test harnesses test this by driving a circuit under test before deasserting its reset.

  This invariant allows developers to insert arbitrary reset delays without worrying their design breaks. Caution should still be taken though, see [Note [Deasserting resets]](#note-deasserting-resets).

* When _Fwd a_ does not contain data (i.e., is `NoData`), its corresponding _Bwd a_ may contain any value, including an error/bottom. When driving a bottom, a component should use `errorX` to make sure it can be evaluated using `seqX`. Test harnesses in `Protocols.Hedgehog` occasionally drive `errorX "No defined Ack"` when seeing `NoData` to test this.

* A circuit driving `Data x` must keep driving the same value until it receives an acknowledgment. This is not yet checked by test harnesses.

### Note [Deasserting resets]
Care should be taken when deasserting resets. Given the following circuit:

```
     withReset rstA a
  |> withReset rstB b
  |> withReset rstC c
```

or schematically:

```
a > b > c
```

Component `a`'s reset should be deasserted first, then `b`'s, then `c`s. More generally, a [topological sort](https://en.wikipedia.org/wiki/Topological_sorting) determines the order in which to deassert resets. For example:

```
a > b > c > d
    ^   v
    f < e
```

should be deasserted as follows: `a`, `b,` `c`, `d`/`e`, `f`. Resets might also be deasserted simultaneously. For example, `a`'s and `b`'s reset might be deasserted at the same cycle.

This order is imposed by the fact that there is an invariant stating a component in reset must not acknowledge data while in reset, but there is - for performance reasons - no invariant stating a component must not send data while in reset.

## Tutorial: `catMaybes`
At this point you should have familiarized with the basic structures of the `Dfs`: its dataconstructors (`Data`, `NoData`, and `Ack`) and its invariants, as well as the structure of a `Circuit` itself. To quickly try things it's useful to keep a repl around. With `stack`:

```
stack exec --package clash-protocols ghci
```

with `cabal`, clone this project and run:

```
cabal update
cabal repl clash-protocols
```

Both should give you the same shell. Import the necessary modules:

```bash
>>> import qualified Clash.Prelude as C
>>> import qualified Protocols.Df.Simple as Dfs
```

You should now be able to query various things. For example:

```bash
>>> :t toSignals
toSignals :: Circuit a b -> (Fwd a, Bwd b) -> (Bwd a, Fwd b)
```

### Implementation
Similar to the console we start by importing all the necessary modules:

```haskell
module CatMaybes where

import Protocols
import qualified Clash.Prelude as C
import qualified Protocols.Df.Simple as Dfs
```

Then, we define the type and name of the component we'd like to write:

```haskell
catMaybes :: Circuit (Dfs dom (Maybe a)) (Dfs dom a)
```

I.e., a circuit that takes a `Dfs` stream of `Maybe a` on the left hand side (LHS) and produces a stream of `a` on the right hand side (RHS). Note that the data carried on `Dfs`s _forward_ path very much looks like a `Maybe` in the first place:

```
>>> :kind! Fwd (Dfs C.System Int)
..
= Signal "System" (Dfs.Data Int)
```

..because `Data Int` itself has two constructors: `Data Int` and `NoData`. In effect, we'd like squash `Data (Just a)` into `Data a`, and `Data Nothing` into `NoData`. Not unlike the way [join](https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Monad.html#v:join) would work on two `Maybe`s.

As the types of `Circuit`s become quite verbose and complex quickly, I like to let GHC do the heavy lifting. For example, I would write:

```haskell
catMaybes = Circuit go
```

At this point, GHC will tell us:

```haskell
CatMaybes.hs:8:21: error:
    Variable not in scope:
      go
        :: (C.Signal dom (Dfs.Data (Maybe a)), C.Signal dom (Dfs.Ack a))
           -> (C.Signal dom (Dfs.Ack (Maybe a)), C.Signal dom (Dfs.Data a))
  |
8 | catMaybes = Circuit go
  |                     ^^
```

This is something we can work with. We need to accept:

* A _data_ signal coming from the LHS
* An _acknowledgment_ signal coming from the RHS

and we need to return:

* An _acknowledgment_ signal coming to the LHS
* A _data_ signal going to the RHS

We can't really work on multiple signals at the same time, so we need to bundle them. Similarly, we unbundle the output of our function:

```
catMaybes = Circuit (C.unbundle . go . C.bundle)
```

Now GHC will tell us:

```haskell
CatMaybes.hs:8:35: error:
    Variable not in scope:
      go
        :: C.Signal dom (Dfs.Data (Maybe a), Dfs.Ack a)
           -> C.Signal dom (Dfs.Ack (Maybe a), Dfs.Data a)
  |
8 | catMaybes = Circuit (C.unbundle . go . C.bundle)
  |                                   ^^
```

Finally, we don't need any state for this function, so we might as well make our lives a little bit easier by using `fmap` to "get rid of" the `Signal`s:

```haskell
catMaybes = Circuit (C.unbundle . fmap go . C.bundle)
```

after which GHC will tell us:

```
CatMaybes.hs:8:40: error:
    Variable not in scope:
      go
        :: (Dfs.Data (Maybe a), Dfs.Ack a)
           -> (Dfs.Ack (Maybe a), Dfs.Data a)
  |
8 | catMaybes = Circuit (C.unbundle . fmap go . C.bundle)
  |                                        ^^
```


This is something we can write, surely! If the LHS does not send data, there's not much we can do. We send `NoData` to the RHS and send a /nack/:

```haskell
  go (Dfs.NoData, _) = (Dfs.Ack False, Dfs.NoData)
```

If we _do_ receive data from the LHS but it turns out to be _Nothing_, we'd like to acknowledge that we received the data and send `NoData` to the RHS:

```haskell
go (Dfs.Data Nothing, _) = (Dfs.Ack True, Dfs.NoData)
```

Finally, if the LHS sends data and it turns out to be a _Just_, we'd like to acknowledge that we received it and pass it onto the RHS. But we should be careful, we should only acknowledge it if our RHS received our data! In effect, we can just passthrough the ack:

```haskell
go (Dfs.Data (Just d), Dfs.Ack ack) = (Dfs.Ack ack, Dfs.Data d)
```

### Testing
We'll use `Hedgehog` for testing our circuit. Conveniently, `Protocols.Hedgehog` defines some pretty handy helpers! Let's import everything:

```haskell
import qualified Protocols.Hedgehog as H
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
```

Before we get to testing our circuit, we first need a Hedgehog generator that can generate input data for our circuit. Let's define it:

```haskell
genCatMaybesInput :: H.Gen [Maybe Int]
genCatMaybesInput =
  Gen.list (Range.linear 0 100) (genMaybe (genInt 10 20))
 where
  genMaybe genA = Gen.choice [Gen.constant Nothing, Just <$> genA]
  genInt a b = Gen.integral (Range.linear a b)
```

The explanation for the definition is out of scope for this tutorial, but it basically says: this generator generates a list with 0 to 100 elements, each a `Just` or a `Nothing`. If it is a `Just` it will contain an `Int` between 10 and 20. If you'd like to learn more about Hedgehog head over to [hackage.haskell.org/package/hedgehog](http://hackage.haskell.org/package/hedgehog).

For `Dfs` circuits we can define a pretty strong property: a `Circuit (Dfs dom a) (Dfs dom a)` is functionally the same as a function `[a] -> [a]` if we strip all the backpressure and `Signal` abstractions. Similarly, we for our `Circuit (Dfs dom (Maybe a)) (Dfs dom a)` our _pure model_ would be `[Maybe a] -> [a]`, i.e. [`Data.catMaybes`](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Maybe.html#v:catMaybes)!

The function `Protocols.Hedgehog.idWithModel` takes advantage of exactly that fact. You tell it:

* How to generate input data
* What function to consider the _pure model_
* What Circuit to consider its dataflow equivalent

In code, this looks like:

```haskell
import qualified Data.Maybe as Maybe

[..]

prop_catMaybes :: H.Property
prop_catMaybes =
  H.idWithModel
    H.defExpectOptions
    genCatMaybesInput
    Maybe.catMaybes
    (catMaybes @C.System)
```

From that point on, it will do the rest. By driving the circuit with arbitrary input and backpressure (among other things), it effectively tests whether a circuit implements the invariants of the `Dfs` protocol and whether it is (functionally) equivalent to its pure model. To actually run the tests we need some more boilerplate:



```haskell
import Test.Tasty
import Test.Tasty.TH (testGroupGenerator)
import Test.Tasty.Hedgehog.Extra (testProperty)

[..]

main :: IO ()
main = defaultMain $(testGroupGenerator)
```

Once that is done, we can run our tests:

```
*CatMaybes> main
CatMaybes
  prop_catMaybes: OK (0.06s)
      ✓ prop_catMaybes passed 100 tests.

All 1 tests passed (0.06s)
```

Yay, all done!

### Debugging
**HEADS UP**: [Hedgehog](https://hackage.haskell.org/package/hedgehog) expects to find source files locally in order to display pretty error messages. If you want pretty error messages in your package, please use [this _patched_ version of Hedgehog](https://github.com/martijnbastiaan/haskell-hedgehog/commits/find-source-files). When using cabal add the following to your `cabal.project`:

```
source-repository-package
    type: git
    location: https://github.com/martijnbastiaan/haskell-hedgehog.git
    tag: f7d25b0a1927b7c06d69535d5dcfcade560ec624
    subdir: hedgehog
```

Stack users can add the following to `stack.yaml`:

```
extra-deps:
- git: git@github.com:martijnbastiaan/haskell-hedgehog
  commit: f7d25b0a1927b7c06d69535d5dcfcade560ec624
  subdirs:
    - hedgehog
```

We'll try and upstream these patches.

----------------

Writing a `Dfs` component can be tricky business. Even for relatively simple circuits such as `catMaybes` it's easy to send a wrong acknowledgment. The test harness is supposed to catch this, but its output isn't always easy to parse. We'll go over a few common mistakes.

Let's introduce one:

```diff
- go (Dfs.Data Nothing, _) = (Dfs.Ack True, Dfs.NoData)
+ go (Dfs.Data Nothing, _) = (Dfs.Ack False, Dfs.NoData)
```

Rerunning the tests will give us a big error, which starts out as:

```
CatMaybes
  prop_catMaybes: FAIL (0.11s)
      ✗ prop_catMaybes failed at src/Protocols/Hedgehog/Internal.hs:167:7
        after 9 tests and 3 shrinks.
```

This notes our test has failed after _9 tests_. To produce a small example Hedgehog has tried to shrink the test to a minimal test case, reflected in its _and 3 shrinks_. The test fails at `src/Protocols/Hedgehog/Internal.hs:167:7`. Usually, you'd see your own files here, but in our case we used `idWithModel` - an externally defined property. Hedgehog will try and be helpful by printing the source code of the property interspersed with the data it generated.

So we get:

```haskell
    ┏━━ src/Protocols/Hedgehog.hs ━━━
 74 ┃ propWithModel ::
 75 ┃   forall a b .
 76 ┃   (Test a, Test b, HasCallStack) =>
 77 ┃   -- | Options, see 'ExpectOptions'
 78 ┃   ExpectOptions ->
 79 ┃   -- | Test data generator
 80 ┃   H.Gen (ExpectType a) ->
 81 ┃   -- | Model
 82 ┃   (ExpectType a -> ExpectType b) ->
 83 ┃   -- | Implementation
 84 ┃   Circuit a b ->
 85 ┃   -- | Property to test for. Function is given the data produced by the model
 86 ┃   -- as a first argument, and the sampled data as a second argument.
 87 ┃   (ExpectType b -> ExpectType b -> H.PropertyT IO ()) ->
 88 ┃   H.Property
 89 ┃ propWithModel eOpts genData model prot prop = H.property $ do
 90 ┃   dat <- H.forAll genData
    ┃   │ [ Nothing , Just 10 ]
```

`propWithModel` is used internally by `idWithModel`. In essence, `propWithModel` is the generalized version of `idWithModel`: it allows users to specify their own properties instead of a hardcoded equality test. Right at line 90 and the line below it, we can see something interesting happen:

```haskell
 90 ┃   dat <- H.forAll genData
    ┃   │ [ Nothing , Just 10 ]
```

At this point Hedgehog used our data generator to generate the input supplied to our circuit. Although it isn't perfect, Hedgehog tried to minimize the example so we can be pretty sure `Nothing` _then_ `Just 10` means something. In other words, we can be reasonably sure that this failure wouldn't have happened with just `Nothing` or just `Just 10`. (Note that this is correct: our `catMaybes` only gets stuck after having seen a `Nothing`. However, the test harness can't see the difference between a filtered `Nothing` and stuckness, hence the additional `Just 10`.)

We move on.

```haskell
 91 ┃   let n = maximum (expectToLengths (Proxy @a) dat)
 92 ┃
 93 ┃   -- TODO: Different distributions?
 94 ┃   let genStall = Gen.integral (Range.linear 0 10)
 95 ┃
 96 ┃   -- Generate stalls for LHS part of the protocol. The first line determines
 97 ┃   -- whether to stall or not. The second determines how many cycles to stall
 98 ┃   -- on each _valid_ cycle.
 99 ┃   lhsStallModes <- H.forAll (sequenceA (C.repeat @(SimulateChannels a) genStallMode))
    ┃   │ <NoStall>
100 ┃   lhsStalls <- H.forAll (traverse (genStalls genStall n) lhsStallModes)
    ┃   │ <(StallWithNack,[])>
101 ┃
102 ┃   -- Generate stalls for RHS part of the protocol. The first line determines
103 ┃   -- whether to stall or not. The second determines how many cycles to stall
104 ┃   -- on each _valid_ cycle.
105 ┃   rhsStallModes <- H.forAll (sequenceA (C.repeat @(SimulateChannels b) genStallMode))
    ┃   │ <NoStall>
106 ┃   rhsStalls <- H.forAll (traverse (genStalls genStall n) rhsStallModes)
    ┃   │ <(StallWithNack,[])>

```

Again, the unnumbered lines are the most interesting:

```
 99 ┃   lhsStallModes <- H.forAll (sequenceA (C.repeat @(SimulateChannels a) genStallMode))
    ┃   │ <NoStall>
100 ┃   lhsStalls <- H.forAll (traverse (genStalls genStall n) lhsStallModes)
    ┃   │ <(StallWithNack,[])>
```

This tells us that the test decided to _not_ produce any stalls on the LHS. The next line says basically the same. We'll get back to that soon. This logic is repeated for stalling the RHS too, with the same results. At this point, we can be pretty sure it has nothing to do with stalls either.

The last interesting bit of the test report is:

```
Circuit did not produce enough output. Expected 1 more values. Sampled only 0:

[]
```

The test tells us that no output was sampled, even though it expected to sample a single value. At this point there is no structured way to actually spot the error, but by now it should be pretty clear.

Let's revert the "mistake" we made and make another:

```diff
-  go (Dfs.Data (Just d), Dfs.Ack ack) = (Dfs.Ack ack, Dfs.Data d)
+  go (Dfs.Data (Just d), Dfs.Ack ack) = (Dfs.Ack True, Dfs.Data d)
```

Again, we get a pretty big error report. Let's skip right to the interesting bits:

```haskell
 90 ┃   dat <- H.forAll genData
    ┃   │ [ Just 10 ]
```
```haskell
 99 ┃   lhsStallModes <- H.forAll (sequenceA (C.repeat @(SimulateChannels a) genStallMode))
    ┃   │ <NoStall>
100 ┃   lhsStalls <- H.forAll (traverse (genStalls genStall n) lhsStallModes)
    ┃   │ <(StallWithNack,[])>
```
```haskell
105 ┃   rhsStallModes <- H.forAll (sequenceA (C.repeat @(SimulateChannels b) genStallMode))
    ┃   │ <NoStall>
106 ┃   rhsStalls <- H.forAll (traverse (genStalls genStall n) rhsStallModes)
    ┃   │ <(StallWithNack,[])>
```
```
Circuit did not produce enough output. Expected 1 more values. Sampled only 0:

[]
```

In this case, Hedgehog pretty much constrained us to pretty much one case in our implementation: the one where it matches on `Dfs.Data (Just d)`. Weirdly, no backpressure was needed to trigger this error, but we still see dropped values. This usually means we generated an _ack_ while the reset was asserted. And sure enough, we don't check for this. (Note that the "right" implementation moved the responsibility of this problem to the component on the RHS, hence not failing.)

At this point it might be tempting to use `Dfs.forceAckLow` to force proper reset behavior. To do so, apply the patch:

```diff
- catMaybes :: Circuit (Dfs dom (Maybe a)) (Dfs dom a)
- catMaybes = Circuit (C.unbundle . fmap go . C.bundle
+ catMaybes :: C.HiddenClockResetEnable dom => Circuit (Dfs dom (Maybe a)) (Dfs dom a)
+ catMaybes = Dfs.forceAckLow |> Circuit (C.unbundle . fmap go . C.bundle
```

Because our function is now stateful, we also need to change the test to:

```haskell
prop_catMaybes :: H.Property
prop_catMaybes =
  H.idWithModelSingleDomain
    H.defExpectOptions
    genCatMaybesInput
    (\_ _ _ -> Maybe.catMaybes)
    (C.exposeClockResetEnable (catMaybes @C.System))
```

Of course, the actual bug is still in there so we expect the test to fail still. And sure enough:

```haskell
 90 ┃   dat <- H.forAll genData
    ┃   │ [ Just 10 ]
```

```haskell
 99 ┃   lhsStallModes <- H.forAll (sequenceA (C.repeat @(SimulateChannels a) genStallMode))
    ┃   │ <NoStall>
100 ┃   lhsStalls <- H.forAll (traverse (genStalls genStall n) lhsStallModes)
    ┃   │ <(StallWithNack,[])>
```

```haskell
105 ┃   rhsStallModes <- H.forAll (sequenceA (C.repeat @(SimulateChannels b) genStallMode))
    ┃   │ <Stall>
106 ┃   rhsStalls <- H.forAll (traverse (genStalls genStall n) rhsStallModes)
    ┃   │ <(StallWithNack,[1])>
```

```
Circuit did not produce enough output. Expected 1 more values. Sampled only 0:

[]
```

This time the LHS of the circuit was not stalled, but the RHS was. Let's bisect:

```
105 ┃   rhsStallModes <- H.forAll (sequenceA (C.repeat @(SimulateChannels b) genStallMode))
    ┃   │ <Stall>
```

At 105, Hedgehog decided to stall the RHS. Note that if we had multiple input channels, we would have seen multiple items in this vector. The next line decides how to stall:

```
106 ┃   rhsStalls <- H.forAll (traverse (genStalls genStall n) rhsStallModes)
    ┃   │ <(StallWithNack,[1])>
```

The first part of the tuple, `StallWithNack`, indicates what the stall circuit does when it does _not_ receive data from the circuit. While it will stall with _nacks_ in this case, it could have stalled with _acks_ or even undefineds too as the protocol doesn't restrict that. The second part of the tuple, `[1]`, determines how many cycles whenever the circuit sends data. If it would have read:

```haskell
[4,5,6]
```

instead, this would have mean that the circuit would be stalled for _4_ cycles on its first valid data cycle, _5_ on the next, and _6_ on the next valid data cycle after that. Hedgehog only generated one member in our case, as it expects to sample just a single value too.

At this point we're forced to conclude that `forceAckWithLow` did not fix our woes and we should persue a proper fix.

## Connecting multiple circuits
**HEADS UP**: The following instructions only work on GHC 8.6.5. Due to internal GHC changes we can't easily port this to newer versions. This is in the works.

-----------------------

Check out [tests/Tests/Protocols/Plugin.hs](https://github.com/clash-lang/clash-protocols/blob/main/tests/Tests/Protocols/Plugin.hs) for examples on how to use `Protocols.Plugin` to wire up circuits using a convenient syntax.


# License
`clash-protocols` is licensed under BSD2. See [LICENSE](LICENSE).

# Project goals

- Include basic dataflow protocols (e.g., `Df`/`Dfs`) and industry supported ones (e.g., AMBA AXI)
- Include lots of basic operators on circuits and its protocols
- Export a consistent interface across protocols
- 100% documentation coverage, preferably with lots of examples

This project does not aim to:

- Provide board or vendor specific primitives

# Contributing
No formal guidelines yet, but feel free to open a PR!

# TODO

0.1

- [x] README
- [x] Add more convenient functions: `fanin`, `roundrobin`, ..
- [ ] Make `Dfs` base implementation instead of `Df` (performance / cleanliness)
- [x] Decide what to do with `Protocols.Ack`
- [ ] Check dead doc links on CI
- [x] Upstream all changes to `circuit-notation` (where possible)
- [x] Add examples on how to use DSL plugin
- [ ] Port and upstream examples `circuit-notation`
- [ ] Blogpost introducing explaining the _why_ of `clash-protocols`

0.2

- [ ] Make DSL plugin work with GHC 8.10
- [ ] AXI AMBA (in terms of `DfLike`!)
- [ ] Test framework for "chunked" designs
- [ ] Improve errors for multichannel tests
- [ ] Investigate whether we can use injective type families for `SimulateType` and `ExpectType`
