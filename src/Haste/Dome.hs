{- |
This library is a wrapper around, but intended to be used in addition
to, "Haste.DOM". It aims to make a top-down approach to
constructing elements more comfortable. A top-down
approach is more natural especially if you consider
that elements are just a representation of the HTML
DOM Tree.
-}
module Haste.Dome (
-- Equivalent to Haste.Dome.JSString so reexport
J.Dome(..), J.eRunIn, J.eApply, J.eAppendChild,
J.eAppendChildM, J.eInsertChildBefore, J.eSet,
J.eClearChildren, J.eClick, J.eFocus, J.eBlur,
J.eGetFirstChild, J.eGetLastChild, J.eGetChildren,
J.eSetChildren, J.eDeleteChild,
-- Redefined here to use String instead of JSString
eBuild, eAppendBuild, eMapQS, eMapQS_,
eWithElemsQS, eElemsByQS, eGetStyle,
eSetStyle, eSetClass, eToggleClass, eHasClass
) where

import qualified Haste.Dome.JSString as J
import Control.Monad.Reader
import Haste.Dome.JSString (Dome)
import Haste.DOM
import Haste.Prim (fromJSStr, toJSStr)

{- |
This function executes the given dome calculation
in a newly created element and afterwards returns
the environment (With all monadic side effects
applied)
-}
{-# INLINABLE eBuild #-}
eBuild :: MonadIO m => String -> Dome m a -> m Elem
eBuild s = J.eBuild (toJSStr s)

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
eAppendBuild s = J.eAppendBuild (toJSStr s)

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
eMapQS qs = J.eMapQS (toJSStr qs)

{- |
A version of 'eMapQS' that discards the results.
-}
{-# INLINABLE eMapQS_ #-}
eMapQS_ :: MonadIO m => QuerySelector -> (Elem -> m a) ->  Dome m ()
eMapQS_ qs = J.eMapQS_ (toJSStr qs)

{- |
Execute the given action on the list of elements returned
by the query selector. The same cautionary tale as with
'eMapQS' applies here
-}
{-# INLINABLE eWithElemsQS #-}
eWithElemsQS :: MonadIO m => QuerySelector -> ([Elem] -> m a) -> Dome m a
eWithElemsQS qs = J.eWithElemsQS (toJSStr qs)

{- |
Retrieves and binds the elements in the environment that
match the given query selector. The same cautionary tale
given with 'eMapQS' applies to this function. In addition
this monadicly binds (and may leak) the elements. Be careful.
-}
{-# INLINABLE eElemsByQS #-}
eElemsByQS :: MonadIO m => QuerySelector -> Dome m [Elem]
eElemsByQS = J.eElemsByQS . toJSStr

{- |
Gets the style property identified by the given id from
the environment. Note that you are generally better
of using CSS and classes instead of this and its counterpart
'eSetStyle'.
-}
{-# INLINABLE eGetStyle #-}
eGetStyle :: MonadIO m => PropID -> Dome m String
eGetStyle = (fromJSStr <$>) . J.eGetStyle . toJSStr

{- |
Sets the style property identified by the given id from
the environment to the given value. Note that you are generally better
of using CSS and classes instead of this and its counterpart
'eGetStyle'.
-}
{-# INLINABLE eSetStyle #-}
eSetStyle :: MonadIO m => PropID -> String -> Dome m ()
eSetStyle propID v = J.eSetStyle (toJSStr propID) (toJSStr v)

{- |
Respectively sets or unsets the given class on the
environment
-}
{-# INLINABLE eSetClass #-}
eSetClass :: MonadIO m => String -> Bool -> Dome m ()
eSetClass n = J.eSetClass (toJSStr n)

{- |
Toggles the given class on the environment
-}
{-# INLINABLE eToggleClass #-}
eToggleClass :: MonadIO m => String -> Dome m ()
eToggleClass = J.eToggleClass . toJSStr

{- |
Checks if the given class is currently set on the
environment
-}
{-# INLINABLE eHasClass #-}
eHasClass :: MonadIO m => String -> Dome m Bool
eHasClass = J.eHasClass . toJSStr
