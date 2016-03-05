{- |
This library is a wrapper intended to be used in addition
to "Haste.DOM". It aims to make a top-down approach to
constructing elements more comfortable. A top-down
approach is more natural especially if you consider
that elements are just a representation of the HTML
DOM Tree.
-}
module Haste.Dome where

import Haste.DOM
import Control.Monad.Reader

{- |
Dome is a simple reader transformer
with the element that is the current hierarchical
subject as the environment
-}
type Dome m a = ReaderT Elem m a

{- |
This function executes the given dome calculation
in a newly created element and afterwards returns
the environment (With all monadic side effects
applied)
-}
{-# INLINABLE eBuild #-}
eBuild :: MonadIO m => String -> Dome m a -> m Elem
eBuild s t = newElem s >>= runReaderT (t >> ask)

{- |
Runs the given dome calculation in the given element.
This function should be used with care. Both the
caller and the callee need to deal with the fact
that there are factors beyond their control. If
in doubt, it may be better to stick to eBuild and
appending the result (and only the result) into
the element.

Another useful usecase for this function is in event
handlers that have to update some part of the DOM or
raise some events. When you do so, try to avoid depending
to much on the actual contents of the element. That usually
results in having to also update the handler when updating
the contents and (possibly) the other way around.

A good rule of thumb is to only run eRunIn on elements on
which that is the only thing you intend to do with it. It
is generally much easier to just competely rerender something
than look for something specific to update.
-}
{-# INLINABLE eRunIn #-}
eRunIn :: (MonadIO m, IsElem e) => e -> Dome m a -> m ()
eRunIn e t = runReaderT (void t) (elemOf e)

{- |
Applies the passed function to the environment element.

Actually just an alias for @'ask' >>= ('lift' . f)@ that is slightly more
convenient to use. This is the bridge between the 'Dome' environment and
the Haste.DOM functions. Almost everything in this module
is defined as some call to eApply.
-}
{-# INLINABLE eApply #-}
eApply :: MonadIO m => (Elem -> m a) -> Dome m a
eApply = (ask >>=) . (lift .)

{- |
Takes the given element and appends it as a child to the environment element
-}
{-# INLINABLE eAppendChild #-}
eAppendChild :: (MonadIO m, IsElem e) => e -> Dome m ()
eAppendChild e = eApply (`Haste.DOM.appendChild` elemOf e)

{- |
Executes the given monadic action and appends the result as a child
to the environment element.
-}
{-# INLINABLE eAppendChildM #-}
eAppendChildM :: (MonadIO m, IsElem e) => m e -> Dome m ()
eAppendChildM e = lift e >>= eAppendChild

{- |
An alias for the commonly recurring

@
'eAppendChildM' ('eBuild' (..))
@

I strongly recommmend using this and not the above pattern
(or variants there of) whenever possible.
-}
{-# INLINABLE eAppendBuild #-}
eAppendBuild :: MonadIO m => String -> Dome m a -> Dome m ()
eAppendBuild s = eAppendChildM . eBuild s

{- |
Insert a new child before the given existing element that
must already be a child of the environment. Note that using
this function directly may be a cautionary signal since you may
leak the before element. I recommend using monadic
bind to directly feed the before element into this function.

An example:

@
'eGetFirstChild' >>= (``eInsertChildBefore`` (..))
@

This way you limit the scope of the before element
to this function, making the template more resiliant
to changes. Note also the use of the semantic 'eGetFirstChild'
rather than supplying an arbitrary element.
-}
{-# INLINABLE eInsertChildBefore #-}
eInsertChildBefore :: (MonadIO m, IsElem before, IsElem child) => before -> child -> Dome m ()
eInsertChildBefore b e = eApply (\p -> insertChildBefore p b e)

{- |
Executes the given action on the elements returned when the
given query selector is run within the environment element. The
results are gathered and returned as a list.

While this function is certainly useful, I again caution overusing
this. Always make sure that the queryselector is not to general
(Can't capture any other elements accidently). The best way
to approach this is to view the caller and the environment
as having a contract. The environment will make sure only elements
that the caller wants to match, match the query selector and you will limit
your action on the element to the agreed upon action.
-}
{-# INLINABLE eMapQS #-}
eMapQS :: MonadIO m => QuerySelector -> (Elem -> m a) -> Dome m [a]
eMapQS qs f = eApply (\p -> mapQS p qs f)

{- |
A version of 'eMapQS' that discards the results.
-}
{-# INLINABLE eMapQS_ #-}
eMapQS_ :: MonadIO m => QuerySelector -> (Elem -> m a) ->  Dome m ()
eMapQS_ qs f = eApply (\p -> mapQS_ p qs f)

{- |
Execute the given action on the list of elements returned
by the query selector. The same cautionary tale as with
'eMapQS' applies here
-}
{-# INLINABLE eWithElemsQS #-}
eWithElemsQS :: MonadIO m => QuerySelector -> ([Elem] -> m a) -> Dome m a
eWithElemsQS qs f = eApply (\p -> withElemsQS p qs f)

{- |
Sets the given list of attributes on the environment element.
-}
{-# INLINABLE eSet #-}
eSet :: MonadIO m => [Attribute] -> Dome m ()
eSet as = eApply (`Haste.DOM.set` as)

{- |
Clears the environment of all children.
-}
{-# INLINABLE eClearChildren #-}
eClearChildren :: MonadIO m => Dome m ()
eClearChildren = eApply clearChildren

{- |
Raises a click event on the environment. Granted this
is not really helpful while constructing elements, but
in some eRunIn calculations this could come in handy
-}
{-# INLINABLE eClick #-}
eClick :: MonadIO m => Dome m ()
eClick = eApply click

{- |
Raises a focus event on the environment. Granted this
is not really helpful while constructing elements, but
in some eRunIn calculations this could come in handy
-}
{-# INLINABLE eFocus #-}
eFocus :: MonadIO m => Dome m ()
eFocus = eApply focus

{- |
Raises a blur event on the environment. Granted this
is not really helpful while constructing elements, but
in some eRunIn calculations this could come in handy
-}
{-# INLINABLE eBlur #-}
eBlur :: MonadIO m => Dome m ()
eBlur = eApply blur

{- |
Retrieves the first child in the environment element
if any. This comes with all the caveats of changing
contexts. You should really just use the information
that it is the first element and not depend on or modify
the contents.
-}
{-# INLINABLE eGetFirstChild #-}
eGetFirstChild :: MonadIO m => Dome m (Maybe Elem)
eGetFirstChild = eApply getFirstChild

{- |
Retrieves the last child in the environment element
if any. This comes with all the caveats of changing
contexts. You should really just use the information
that it is the last element and not depend on or modify
the contents.
-}
{-# INLINABLE eGetLastChild #-}
eGetLastChild :: MonadIO m => Dome m (Maybe Elem)
eGetLastChild = eApply getLastChild

{- |
Retrieves all the children from the environment.
Use with EXTREME CAUTION. As the element grows and
changes this result will also change and you
do not know anything about any of the elements
other than that they are children.
-}
{-# INLINABLE eGetChildren #-}
eGetChildren :: MonadIO m => Dome m [Elem]
eGetChildren = eApply getChildren

{- |
Sets the given list of elements as the children
of the element. This kind of defeats the purpose
of a top-down approach, but if the elements
come from an external source this can be useful.
-}
{-# INLINABLE eSetChildren #-}
eSetChildren :: MonadIO m => [Elem] -> Dome m ()
eSetChildren es = eApply (`setChildren` es)

{- |
Removes the given child from the environment element.
-}
{-# INLINABLE eDeleteChild #-}
eDeleteChild :: MonadIO m => Elem -> Dome m ()
eDeleteChild e = eApply (`deleteChild` e)

{- |
Retrieves and binds the elements in the environment that
match the given query selector. The same cautionary tale
given with 'eMapQS' applies to this function. In addition
this monadicly binds (and may leak) the elements. Be careful.
-}
{-# INLINABLE eElemsByQS #-}
eElemsByQS :: MonadIO m => QuerySelector -> Dome m [Elem]
eElemsByQS qs = eApply (`elemsByQS` qs)

{- |
Gets the style property identified by the given id from
the environment. Note that you are generally better
of using CSS and classes instead of this and its counterpart
'eSetStyle'.
-}
{-# INLINABLE eGetStyle #-}
eGetStyle :: MonadIO m => PropID -> Dome m String
eGetStyle propID = eApply (`getStyle` propID)

{- |
Sets the style property identified by the given id from
the environment to the given value. Note that you are generally better
of using CSS and classes instead of this and its counterpart
'eGetStyle'.
-}
{-# INLINABLE eSetStyle #-}
eSetStyle :: MonadIO m => PropID -> String -> Dome m ()
eSetStyle propID v = eApply (\p -> setStyle p propID v)

{- |
Respectively sets or unsets the given class on the
environment
-}
{-# INLINABLE eSetClass #-}
eSetClass :: MonadIO m => String -> Bool -> Dome m ()
eSetClass n v = eApply (\p -> setClass p n v)

{- |
Toggles the given class on the environment
-}
{-# INLINABLE eToggleClass #-}
eToggleClass :: MonadIO m => String -> Dome m ()
eToggleClass n = eApply (`toggleClass` n)

{- |
Checks if the given class is currently set on the
environment
-}
{-# INLINABLE eHasClass #-}
eHasClass :: MonadIO m => String -> Dome m Bool
eHasClass n = eApply (`hasClass` n)
