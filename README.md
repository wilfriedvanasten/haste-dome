# Haste-Dome

This library is a wrapper intended to be used in addition
to Haste.DOM. It aims to make a top-down approach to
constructing elements more comfortable. A top-down
approach is more natural especially if you consider
that elements are just a representation of the HTML
DOM Tree.

## A top-down approach to element building

Consider the following HTML snippet:

```html
<div class="hello">
  <h1>Hello World</h1>
</div>
```

Now I can illustrate what the top-down approach means:

 * We do not know where this snippet resides. Therefore
   we should make no assumptions about the external
   environment. Following this to the logical conclusion
   the `h1` should make no assumptions about the `div`

 * The `div` does not care nor know about the contents
   of the `h1`. This follows from the fact, that, in most cases,
   it just functions as a container.

 * The DOM is shaped as a tree. When constructing the
   elements in Haskell with a top-down approach, we
   can build our element building expression like a tree.

These are obviously just guidelines. Especially the second
rule is often broken. It does indicate that you should
take care when doing so. It greatly complicates the
relationship between the parent and the child's contents
and therefore the child.

## Why haste-dome

While it is possible to nest Haste.DOM monadic expressions in such
a way that the element is build in a top-down manner, you
need to really think and work in a very specific manner,
which is a burden. Think of it like writing folds
and maps repeatedly specialized in line. It's not hard,
but it is extra work (And a chance for errors) we want to avoid.

Consider the following sample of Haste.DOM that closely
approaches the top-down semantics this library strives
for:

```haskell
hello = do
  p <- newElem "div"
  set p [attr "class" =: "hello"]
  appendChild p =<< (do
      p <- newElem "h1"
      appendChild p =<< newTextElem "Hello World"
      pure p)
  pure p
```

While we have achieved the rules we deduce from the nature
of the HTML DOM tree, we have to construct the code in
a very specific way:


 * We need to shadow the `p` in the lower levels to prevent
   the lower elements from accessing the higher elements.
   This assures the lower elements make no assumptions
   about their containers

 * We immediately bind the childs into the append functions.
   This assures that, in principle, `p` just functions as a
   container for its children.

In addition we have to deal with capturing and passing
the element `p` at the current level.

Note that the sample is merely an illustration of the top-down
approach applied consistently in Haste.DOM. It is very
easy to break out of the pattern and loose the benefits it
brings.

These observations are the basis of this library. It relieves
both the need for the explicit `p` passing and makes the
scoping implicit. It does so by introducing the `Dome` type.
This is just an alias for `ReaderT Elem` that has the
current hierarchical element p as the environment.
The above snippet looks like this in haste-dome:

```haskell
hello =
  eBuild "div" $ do
    eSet [attr "class" =: "hello"]
    eAppendBuild "h1" $
      eAppendChildM $ newTextElem "Hello World"
```

That's it! We can clearly see the top-down structure
achieved, but we arrived at it in a more comfortable
way. No more need to keep track of our context
and pass it explicitly to (nearly) all functions.
We have also greatly simplified the rules to remember
and while you cannot see it, we also gained a signal:
If we need some element other than `p` in an operation
not related to `p`, we need to somehow lift or pass the
operation into the Dome expression. The context changes
explicitly and visibly. Also note that we no longer
need pure. This was necessary in the Haste.DOM approach,
but, thanks to `ReaderT`, we can ask for that in
`eBuild`.

Now there are likely situations in which it is unavoidable
to break with this pattern. And that's okay! This systematic
approach to building elements just increases the maintainability
at a (relatively) low cost. For the other (more tricky?) situations
you can use all the low-level (While this library is certainly not
on a much higher level) power Haste.DOM provides. I
render low level (ffi) canvas stuff and than append the result of the
computation to my element (Which is nested in anther element etc.) using
this pattern. It also means you can easily extract subexpressions
into reusable top level elements (Since you avoid dependening on a
specific context).

## Friendly advice

This library contains cautions
throughout the documentation. These are just friendly advice.
Following them or at least keeping them in mind assures
you get the most out of the top down approach. The library
also contains functions that should be avoided in a mostly
top-down approach, to make the most out of the environment and to
keep close coherence to the Haste.DOM API. While these functions
can easily cause maintenance problems, they are powerful
'primitives' (for lack of a better word). If you use them
smartly you can construct safer variants that ensure the
top-down approach's advantages are maintained.

## The e's! They do nothing!

You may have noticed that all 'Haste.Dome' functions are
prefixed with an e. This is just a consistent application
of simple name mangling to prevent name collision with
Haste.DOM. Since most of the functions in this library
also have a counterpart in Haste.DOM, this makes using
the library very easy especially if you are already
familiar with Haste.DOM. Just stick an e (for environment,
element or Dom**e** if you have trouble remembering) in
front of the relevant identifier and, in most cases, you
are good to go! And that also works the other way around!

### Why not use qualification?

Since users of this library need functions from both
Haste.DOM and this library, it would be more awkward
to have to use qualifiers for one and no qualifiers for the
other. This is especially true since the interfaces
are so similar. This consistent name mangling also
ensures that the code is universally understandable.
This is not guaranteed to happen when using qualification.
