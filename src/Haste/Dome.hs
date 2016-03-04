module Haste.Dome where

import Haste.DOM
import Control.Monad.Reader

{-
My BlazeTemplate is a simple reader transformer
with the element we are rendering the template
into as the environment
-}
type Dome m a = ReaderT Elem m a

{-
This function constructs a given template into a new bare
element of the given type.
-}
{-# INLINABLE eBuild #-}
eBuild :: MonadIO m => String -> Dome m a -> m Elem
eBuild s t = newElem s >>= runReaderT (t >> ask)

{-
Builds a template with an initial environment. There
are no restrictions in place to prevent the template
from tampering with the environment, so it is safest
to consider the given element a part of the template.
Do also make sure that the given environment is
compatible with the expectations of the template.
This version of build does not return the environment,
since it would be returned to the source of the element
which means it is already there
-}
{-# INLINABLE eRunIn #-}
eRunIn :: (MonadIO m, IsElem e) => e -> Dome m a -> m ()
eRunIn e t = runReaderT (void t) (elemOf e)

{-
apply is a wrapper around (ask >>= (lift . f)) that is slightly more
convenient to use. This is the bridge between Dome and
and Haste.DOM
-}
{-# INLINABLE eApply #-}
eApply :: MonadIO m => (Elem -> m a) -> Dome m a
eApply f = ask >>= (lift . f)

{-
append takes the given element and appends it to the environment element,
The only reasonable way to do this is as a child
-}
{-# INLINABLE eAppendChild #-}
eAppendChild :: (MonadIO m, IsElem e) => e -> Dome m ()
eAppendChild e = eApply (`Haste.DOM.appendChild` elemOf e)

{-# INLINABLE eAppendChildM #-}
eAppendChildM :: (MonadIO m, IsElem e) => m e -> Dome m ()
eAppendChildM e = lift e >>= eAppendChild

{-# INLINABLE eAppendBuild #-}
eAppendBuild :: MonadIO m => String -> Dome m a -> Dome m ()
eAppendBuild s = eAppendChildM . eBuild s

{-
Insert before takes a function that gets a child element from the
environment and than inserts the given element before that element.
-}
{-# INLINABLE eInsertChildBefore #-}
eInsertChildBefore :: (MonadIO m, IsElem before, IsElem child) => before -> child -> Dome m ()
eInsertChildBefore b e = eApply (\p -> insertChildBefore p b e)

{-
withQS executes the given action on the elements returned when the
given query selector is run within the environment element. The
results are gathered and returned as a list. See also withQS_
-}
{-# INLINABLE eMapQS #-}
eMapQS :: MonadIO m => QuerySelector -> (Elem -> m a) -> Dome m [a]
eMapQS qs f = eApply (\p -> mapQS p qs f)

{-# INLINABLE eMapQS_ #-}
eMapQS_ :: MonadIO m => QuerySelector -> (Elem -> m a) ->  Dome m ()
eMapQS_ qs f = eApply (\p -> mapQS_ p qs f)

eWithElemsQS :: MonadIO m => QuerySelector -> ([Elem] -> m a) -> Dome m a
eWithElemsQS qs f = eApply (\p -> withElemsQS p qs f)

{-# INLINABLE eSet #-}
eSet :: MonadIO m => [Attribute] -> Dome m ()
eSet as = eApply (`Haste.DOM.set` as)

{-# INLINABLE eClearChildren #-}
eClearChildren :: MonadIO m => Dome m ()
eClearChildren = eApply clearChildren

{-# INLINABLE eClick #-}
eClick :: MonadIO m => Dome m ()
eClick = eApply click

{-# INLINABLE eFocus #-}
eFocus :: MonadIO m => Dome m ()
eFocus = eApply focus

{-# INLINABLE eBlur #-}
eBlur :: MonadIO m => Dome m ()
eBlur = eApply blur

{-# INLINABLE eGetFirstChild #-}
eGetFirstChild :: MonadIO m => Dome m (Maybe Elem)
eGetFirstChild = eApply getFirstChild

{-# INLINABLE eGetLastChild #-}
eGetLastChild :: MonadIO m => Dome m (Maybe Elem)
eGetLastChild = eApply getLastChild

{-# INLINABLE eGetChildren #-}
eGetChildren :: MonadIO m => Dome m [Elem]
eGetChildren = eApply getChildren

{-# INLINABLE eSetChildren #-}
eSetChildren :: MonadIO m => [Elem] -> Dome m ()
eSetChildren es = eApply (`setChildren` es)

{-# INLINABLE eDeleteChild #-}
eDeleteChild :: MonadIO m => Elem -> Dome m ()
eDeleteChild e = eApply (`deleteChild` e)

{-# INLINABLE eElemsByQS #-}
eElemsByQS :: MonadIO m => QuerySelector -> Dome m [Elem]
eElemsByQS qs = eApply (`elemsByQS` qs)

{-# INLINABLE eGetStyle #-}
eGetStyle :: MonadIO m => PropID -> Dome m String
eGetStyle propID = eApply (`getStyle` propID)

{-# INLINABLE eSetStyle #-}
eSetStyle :: MonadIO m => PropID -> String -> Dome m ()
eSetStyle propID v = eApply (\p -> setStyle p propID v)

{-# INLINABLE eSetClass #-}
eSetClass :: MonadIO m => String -> Bool -> Dome m ()
eSetClass n v = eApply (\p -> setClass p n v)

{-# INLINABLE eToggleClass #-}
eToggleClass :: MonadIO m => String -> Dome m ()
eToggleClass n = eApply (`toggleClass` n)

{-# INLINABLE eHasClass #-}
eHasClass :: MonadIO m => String -> Dome m Bool
eHasClass n = eApply (`hasClass` n)
