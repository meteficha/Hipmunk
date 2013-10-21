-----------------------------------------------------------------------------
-- |
-- Module      :  Physics/Hipmunk/Unsafe.hsc
-- Copyright   :  (c) 2008-2010 Felipe A. Lessa
-- License     :  MIT (see LICENSE)
--
-- Maintainer  :  felipe.lessa@gmail.com
-- Stability   :  provisional
-- Portability :  portable (needs FFI)
--
-- All functions on this module are /UNSAFE/ in the sense that
-- they may reduce the physical accuracy or numerical stability
-- of the simulation if you use them correctly, or may crash your
-- system if you are not careful enough.  Read their
-- documentation carefully and use them only if you really need
-- and know what you are doing.
--
-----------------------------------------------------------------------------

module Physics.Hipmunk.Unsafe
    (-- * Shapes
     unsafeShapeRedefine,
     -- * Constraints
     unsafeRememberC
    )
    where

import Foreign hiding (rotate, new)
import Foreign.C
#include "wrapper.h"

import Physics.Hipmunk.Common
import Physics.Hipmunk.Internal
import Physics.Hipmunk.Shape

-- | @unsafeShapeRedefine shape type off@ redefines @shape@ to
--   have new parameters described on @type@ and to be at offset
--   @off@.  Be careful, /you should not change the shape type/.
--   For example, it is unsafe to change a circle shape's radius,
--   but it is an error to try to change a circle into a segment
--   or a polygon.  Note also that these errors /are not/
--   /checked/, meaning /they will probably crash Chipmunk/.
unsafeShapeRedefine :: Shape -> ShapeType -> Position -> IO ()
unsafeShapeRedefine (S shape _) (Circle r) off =
  withForeignPtr shape $ \shape_ptr ->
  with off $ \off_ptr -> do
    cpCircleShapeSetRadius shape_ptr r
    wrCircleShapeSetOffset shape_ptr off_ptr

unsafeShapeRedefine (S shape _) (LineSegment p1 p2 r) off =
  withForeignPtr shape $ \shape_ptr ->
  with (p1+off) $ \p1off_ptr ->
  with (p2+off) $ \p2off_ptr -> do
    wrSegmentShapeSetEndpoints shape_ptr p1off_ptr p2off_ptr
    cpSegmentShapeSetRadius shape_ptr r

unsafeShapeRedefine (S shape _) (Polygon verts) off =
  withForeignPtr shape $ \shape_ptr ->
  with off $ \off_ptr ->
  withArrayLen verts $ \verts_len verts_ptr -> do
    let verts_len' = fromIntegral verts_len
    wrPolyShapeSetVerts shape_ptr verts_len' verts_ptr off_ptr

foreign import ccall unsafe "wrapper.h"
    cpCircleShapeSetRadius :: ShapePtr -> CpFloat -> IO ()
foreign import ccall unsafe "wrapper.h"
    wrCircleShapeSetOffset :: ShapePtr -> VectorPtr -> IO ()
foreign import ccall unsafe "wrapper.h"
    wrSegmentShapeSetEndpoints :: ShapePtr -> VectorPtr -> VectorPtr -> IO ()
foreign import ccall unsafe "wrapper.h"
    cpSegmentShapeSetRadius :: ShapePtr -> CpFloat -> IO ()
foreign import ccall unsafe "wrapper.h"
    wrPolyShapeSetVerts :: ShapePtr -> CInt -> VectorPtr -> VectorPtr -> IO ()




-- | Unsafe function that changes the constraint type to
--   anything. It is unsafe because you should call 'redefine'
--   only on the same kind of constraint you created, and this
--   function allows you to bypass the type system checks.  Note
--   also that, unlike Chipmunk, we don't check at run-time that
--   'redefine' is being called on the right type!
unsafeRememberC :: ConstraintType a => Constraint Unknown -> Constraint a
unsafeRememberC (C c b1 b2) = C c b1 b2
{-# INLINE unsafeRememberC #-}