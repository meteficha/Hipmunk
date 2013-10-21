-----------------------------------------------------------------------------
-- |
-- Module      :  Physics/Hipmunk/Hipmunk.hs
-- Copyright   :  (c) Felipe A. Lessa 2008-2010
-- License     :  MIT (see LICENSE)
--
-- Maintainer  :  felipe.lessa@gmail.com
-- Stability   :  provisional
-- Portability :  portable (needs FFI)
--
-- This module re-exports all other Hipmunk modules. It is
-- meant to be imported qualified such as
--
-- @
-- import qualified Physics.Hipmunk as H
-- @
--
-- however it doesn't clash with the 'Prelude'.
--
-----------------------------------------------------------------------------

module Physics.Hipmunk
    (-- * Modules re-exported
     module Physics.Hipmunk.Common,
     module Physics.Hipmunk.Body,
     module Physics.Hipmunk.Shape,
     module Physics.Hipmunk.Constraint,
     module Physics.Hipmunk.Space,
     module Physics.Hipmunk.Callbacks
    )
    where

import Physics.Hipmunk.Common
import Physics.Hipmunk.Callbacks
import Physics.Hipmunk.Body
import Physics.Hipmunk.Shape
import Physics.Hipmunk.Constraint
import Physics.Hipmunk.Space
