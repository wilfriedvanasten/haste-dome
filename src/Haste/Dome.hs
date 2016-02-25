module Haste.Dome (
  buildNew,
  buildIn,
  apply,
  append,
  appendM,
  appendNew,
  insertBefore,
  withQS,
  withQS_,
  (=:),
  attr,
  Haste.Dome.set,
  newTextElem,
  newElem,
  clear
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
{-# INLINABLE buildNew #-}
buildNew :: MonadIO m => String -> Dome m a -> m Elem
buildNew s t = newElem s >>= runReaderT (t >> ask)

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
buildIn :: MonadIO m => Elem -> Dome m a -> m ()
buildIn e t = runReaderT (void t) e

{-
apply is a wrapper around (ask >>= (lift . f)) that is slightly more
convenient to use. This is the bridge between BlazeTemplate
and Haste.DOM
-}
{-# INLINABLE apply #-}
apply :: MonadIO m => (Elem -> m a) -> Dome m a
apply f = ask >>= (lift . f)

{-
append takes the given element and appends it to the environment element,
The only reasonable way to do this is as a child
-}
{-# INLINABLE append #-}
append :: MonadIO m => Elem -> Dome m ()
append e = apply (`Haste.DOM.appendChild` e)

{-# INLINABLE appendM #-}
appendM :: MonadIO m => m Elem -> Dome m ()
appendM e = lift e >>= append

appendNew :: MonadIO m => String -> Dome m a -> Dome m ()
appendNew s = appendM . buildNew s

{-
Insert before takes a function that gets a child element from the
environment and than inserts the given element before that element.
-}
{-# INLINABLE insertBefore #-}
insertBefore :: MonadIO m => (Elem -> m Elem) -> Elem -> Dome m ()
insertBefore f e = do
  b <- apply f
  apply (\p -> insertChildBefore p b e)

{-
withQS executes the given action on the elements returned when the
given query selector is run within the environment element. The
results are gathered and returned as a list. See also withQS_
-}
{-# INLINABLE withQS #-}
withQS :: MonadIO m => QuerySelector -> (Elem -> m a) -> Dome m [a]
withQS qs f = do
  es <- apply (`elemsByQS` qs)
  lift $ mapM f es

{-# INLINABLE withQS_ #-}
withQS_ :: MonadIO m => QuerySelector -> (Elem -> m a) ->  Dome m ()
withQS_ qs f = do
  es <- apply (`elemsByQS` qs)
  lift $ mapM_ f es

set :: MonadIO m => [Attribute] -> Dome m ()
set as = apply (`Haste.DOM.set` as)

clear :: MonadIO m => Dome m ()
clear = apply clearChildren
