module Haste.Dome (
  eBuild,
  eRunIn,
  eApply,
  eAppendChild,
  eAppendChildM,
  eAppendBuild,
  eInsertChildBefore,
  eWithQS,
  eWithQS_,
  (=:),
  attr,
  eSet,
  newTextElem,
  newElem,
  eClearChildren
) where

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

eAppendBuild :: MonadIO m => String -> Dome m a -> Dome m ()
eAppendBuild s = eAppendChildM . eBuild s

{-
Insert before takes a function that gets a child element from the
environment and than inserts the given element before that element.
-}
{-# INLINABLE eInsertChildBefore #-}
eInsertChildBefore :: (MonadIO m, IsElem before, IsElem child) => (Elem -> m before) -> child -> Dome m ()
eInsertChildBefore f e = do
  b <- eApply f
  eApply (\p -> insertChildBefore p b e)

{-
withQS executes the given action on the elements returned when the
given query selector is run within the environment element. The
results are gathered and returned as a list. See also withQS_
-}
{-# INLINABLE eWithQS #-}
eWithQS :: MonadIO m => QuerySelector -> (Elem -> m a) -> Dome m [a]
eWithQS qs f = do
  es <- eApply (`elemsByQS` qs)
  lift $ mapM f es

{-# INLINABLE eWithQS_ #-}
eWithQS_ :: MonadIO m => QuerySelector -> (Elem -> m a) ->  Dome m ()
eWithQS_ qs f = do
  es <- eApply (`elemsByQS` qs)
  lift $ mapM_ f es

eSet :: MonadIO m => [Attribute] -> Dome m ()
eSet as = eApply (`Haste.DOM.set` as)

eClearChildren :: MonadIO m => Dome m ()
eClearChildren = eApply clearChildren
