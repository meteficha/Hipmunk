-----------------------------------------------------------------------------
-- |
-- Module      :  Physics/Hipmunk/Constraint.hsc
-- Copyright   :  (c) 2008-2010 Felipe A. Lessa
-- License     :  MIT (see LICENSE)
--
-- Maintainer  :  felipe.lessa@gmail.com
-- Stability   :  provisional
-- Portability :  portable (needs FFI)
--
-- Constraints that restrict the bodies' movement.
--
-----------------------------------------------------------------------------

module Physics.Hipmunk.Constraint
    (-- * Common interface
     Constraint,
     newConstraint,
     redefineC,
     setBiasCoefC,
     setMaxBias,
     setMaxForce,
     -- ** Forgetting the phantom type
     -- $phantom
     Unknown,
     forgetC,

     -- * Constraint types
     -- $constraintTypes

     -- ** Pin joint
     Pin(..),
     -- ** Slide joint
     Slide(..),
     -- ** Pivot joint
     Pivot(..),
     -- ** Groove joint
     Groove(..),
     -- ** Gear joint
     Gear(..),
     -- ** Damped spring
     DampedSpring(..),
     -- ** Damped rotary spring
     DampedRotarySpring(..),
     -- ** Ratchet joint
     Ratchet(..),
     -- ** Rotary limit
     RotaryLimit(..),
     -- ** Simple motor
     SimpleMotor(..),
    )
    where

import Foreign
#include "wrapper.h"

import Physics.Hipmunk.Common
import Physics.Hipmunk.Internal
import Physics.Hipmunk.Body (worldToLocal)


-- | @newConstraint b1 b2 type_@ connects the two bodies @b1@ and @b2@
--   with a constraint of the given type. Note that you should
--   add the 'Constraint' to a space.
--
--   The 'ConstraintType' type class is implemented by all
--   constraint types to allow them to be manipulated by the same
--   framework while retaining type-safety, consequently it isn't
--   exported.
newConstraint :: ConstraintType a => Body -> Body -> a -> IO (Constraint a)
newConstraint body1@(B b1) body2@(B b2) type_ =
  withForeignPtr b1 $ \b1_ptr ->
  withForeignPtr b2 $ \b2_ptr ->
  mallocForeignPtrBytes (size type_) >>= \constraint ->
  withForeignPtr constraint $ \constraint_ptr -> do
    init_ type_ constraint_ptr b1_ptr b2_ptr
    return (C constraint body1 body2)
{-# SPECIALISE newConstraint :: Body -> Body -> Pin -> IO (Constraint Pin) #-}
{-# SPECIALISE newConstraint :: Body -> Body -> Slide -> IO (Constraint Slide) #-}
{-# SPECIALISE newConstraint :: Body -> Body -> Pivot -> IO (Constraint Pivot) #-}
{-# SPECIALISE newConstraint :: Body -> Body -> Groove -> IO (Constraint Groove) #-}
{-# SPECIALISE newConstraint :: Body -> Body -> Gear -> IO (Constraint Gear) #-}
{-# SPECIALISE newConstraint :: Body -> Body -> DampedSpring -> IO (Constraint DampedSpring) #-}
{-# SPECIALISE newConstraint :: Body -> Body -> DampedRotarySpring -> IO (Constraint DampedRotarySpring) #-}
{-# SPECIALISE newConstraint :: Body -> Body -> Ratchet -> IO (Constraint Ratchet) #-}
{-# SPECIALISE newConstraint :: Body -> Body -> RotaryLimit -> IO (Constraint RotaryLimit) #-}
{-# SPECIALISE newConstraint :: Body -> Body -> SimpleMotor -> IO (Constraint SimpleMotor) #-}

-- | @redefine constr type_@ redefines @constr@'s parameters
--   on-the-fly, allowing you to dynamically change the
--   constraint's behaviour.
redefineC :: ConstraintType a => Constraint a -> a -> IO ()
redefineC (C c b1 b2) t = withForeignPtr c $ \c_ptr -> redef c_ptr b1 b2 t
{-# SPECIALISE redefineC :: Constraint Pin -> Pin -> IO () #-}
{-# SPECIALISE redefineC :: Constraint Slide -> Slide -> IO () #-}
{-# SPECIALISE redefineC :: Constraint Pivot -> Pivot -> IO () #-}
{-# SPECIALISE redefineC :: Constraint Groove -> Groove -> IO () #-}
{-# SPECIALISE redefineC :: Constraint Gear -> Gear -> IO () #-}
{-# SPECIALISE redefineC :: Constraint DampedSpring -> DampedSpring -> IO () #-}
{-# SPECIALISE redefineC :: Constraint DampedRotarySpring -> DampedRotarySpring -> IO () #-}
{-# SPECIALISE redefineC :: Constraint Ratchet -> Ratchet -> IO () #-}
{-# SPECIALISE redefineC :: Constraint RotaryLimit -> RotaryLimit -> IO () #-}
{-# SPECIALISE redefineC :: Constraint SimpleMotor -> SimpleMotor -> IO () #-}

-- | Sets the constraint's bias coefficient.  By default it is
--   equal to the last value set globally with
--   'setConstraintBiasCoef', which initially is @0.1@
setBiasCoefC :: BiasCoef -> Constraint a -> IO ()
setBiasCoefC b (C c _ _) = withForeignPtr c $ flip #{poke cpConstraint, errorBias} b

setMaxBias :: CpFloat -> Constraint a -> IO ()
setMaxBias b (C c _ _) = withForeignPtr c $ flip #{poke cpConstraint, maxBias} b

setMaxForce :: CpFloat -> Constraint a -> IO ()
setMaxForce b (C c _ _) = withForeignPtr c $ flip #{poke cpConstraint, maxForce} b

-- $phantom
--   These functions discard the phantom type of the constraint.
--   They're useful, for example, if you want to put different
--   constraints in a homogeneous data structure (such as a
--   list).

-- | Completely safe function that discards the constraint type
--   (which is a phantom type).  You can \"remember\" it again by
--   using @unsafeRemember@ from the @Unsafe@ module.
forgetC :: Constraint a -> Constraint Unknown
forgetC (C c b1 b2) = C c b1 b2
{-# INLINE forgetC #-}



-- $constraintTypes
--   There are currently nine types of constraints. When
--   appending a number to a property, we hint that it refer to
--   one of the bodies that the constraint is intercting with
--   (e.g. \"Second anchor\" is the position of the anchor on the
--   second body in its coordinates).

-- | A pin joint connects the bodies with a solid pin.
--   The anchor points are kept at a fixed distance.
data Pin = Pin {pinAnchor1 :: !Position {-^ First anchor. -}
               ,pinAnchor2 :: !Position {-^ Second anchor. -}}
    deriving (Eq, Ord, Show)

instance ConstraintType Pin where
  size _ = #{size cpPinJoint}
  init_ (Pin a1 a2) = with2 a1 a2 $ wrPinJointInit
  redef ptr _ _ (Pin a1 a2) = do
      #{poke cpPinJoint, anchr1} ptr a1
      #{poke cpPinJoint, anchr2} ptr a2

-- | A slide joint is similar to a pin joint, however
--   it has a minimum and a maximum distance.
data Slide = Slide {slideAnchor1 :: !Position {-^ First anchor. -}
                   ,slideAnchor2 :: !Position {-^ Second anchor. -}
                   ,slideMinDist :: !Distance {-^ Minimum distance. -}
                   ,slideMaxDist :: !Distance {-^ Maximum distance. -}}
    deriving (Eq, Ord, Show)

instance ConstraintType Slide where
  size _ = #{size cpSlideJoint}
  init_ (Slide a1 a2 mn mx) = with2 a1 a2 $ wrSlideJointInit mn mx
  redef ptr _ _ (Slide a1 a2 mn mx) = do
      #{poke cpSlideJoint, anchr1} ptr a1
      #{poke cpSlideJoint, anchr2} ptr a2
      #{poke cpSlideJoint, min} ptr mn
      #{poke cpSlideJoint, max} ptr mx

-- | A pivot joint allows the bodies to pivot around
--   a single point.
data Pivot =
    -- | You may specify the pivot point in world's coordinates
    --   (so both bodies should be already in place).
    Pivot1 {pivotPos :: !Position {-^ Pivot point in world's coordinates. -}}
    -- | Or you may specify the joint as two anchors (on each
    --   body's coordinates), removing the need having the bodies
    --   already in place.
  | Pivot2 {pivotAnchor1 :: !Position {-^ First anchor. -}
           ,pivotAnchor2 :: !Position {-^ Second anchor. -}}
    deriving (Eq, Ord, Show)

instance ConstraintType Pivot where
  size _ = #{size cpPivotJoint}
  init_ (Pivot1 pos)   = with1 pos   $ wrPivot1JointInit
  init_ (Pivot2 a1 a2) = with2 a1 a2 $ wrPivot2JointInit
  redef ptr b1 b2 (Pivot1 pos) = do
      worldToLocal b1 pos >>= #{poke cpPivotJoint, anchr1} ptr
      worldToLocal b2 pos >>= #{poke cpPivotJoint, anchr2} ptr
  redef ptr _ _ (Pivot2 a1 a2) = do
      #{poke cpPivotJoint, anchr1} ptr a1
      #{poke cpPivotJoint, anchr2} ptr a2

-- | A groove joint attaches a point on the second body
--   to a groove in the first one.
data Groove = Groove {
      groovePoints :: !(Position,Position) {-^ Groove, in first body's coordinates. -}
     ,groovePivot  :: !Position            {-^ Pivot, in second body's coordinates. -}}
    deriving (Eq, Ord, Show)

instance ConstraintType Groove where
  size _ = #{size cpGrooveJoint}
  init_ (Groove (g1,g2) anchor) = with3 g1 g2 anchor $ wrGrooveJointInit
  redef ptr _ _ (Groove (g1,g2) anchor) = do
      #{poke cpGrooveJoint, grv_a} ptr g1
      #{poke cpGrooveJoint, grv_b} ptr g2
      #{poke cpGrooveJoint, grv_n} ptr $ perp $ normalize $ g1 - g2
      #{poke cpGrooveJoint, anchr2} ptr anchor

-- | A gear joint restricts the bodies movement to be
--   coordinated as if they were attached like dented gears.
data Gear = Gear {gearPhase :: !Angle   {-^ Phase of the movement. -}
                 ,gearRatio :: !CpFloat {-^ Ratio between the gears. -}}
    deriving (Eq, Ord, Show)

instance ConstraintType Gear where
  size _ = #{size cpGearJoint}
  init_ (Gear p r) = wrGearJointInit p r
  redef ptr _ _ (Gear p r) = do
      #{poke cpGearJoint, phase} ptr p
      #{poke cpGearJoint, ratio} ptr r
      #{poke cpGearJoint, ratio_inv} ptr (1/r)

-- | A simple damped spring.  Generally this constraint
--   should be used instead of @applyDampedSpring@.
data DampedSpring = DampedSpring {
      dampedAnchor1    :: !Position {-^ First anchor. -}
     ,dampedAnchor2    :: !Position {-^ Second anchor. -}
     ,dampedRestLength :: !Distance {-^ Rest length. -}
     ,dampedStiffness  :: !CpFloat  {-^ Stiffness. -}
     ,dampedDamping    :: !Damping  {-^ Damping. -}}
    deriving (Eq, Ord, Show)

instance ConstraintType DampedSpring where
  size _ = #{size cpDampedSpring}
  init_ (DampedSpring a1 a2 r s d) = with2 a1 a2 $ wrDampedSpringInit r s d
  redef ptr _ _ (DampedSpring a1 a2 r s d) = do
      #{poke cpDampedSpring, anchr1} ptr a1
      #{poke cpDampedSpring, anchr2} ptr a2
      #{poke cpDampedSpring, restLength} ptr r
      #{poke cpDampedSpring, stiffness} ptr s
      #{poke cpDampedSpring, damping} ptr d

-- | A damped rotary spring constraint.
data DampedRotarySpring = DampedRotarySpring {
      dampedRotRestAngle :: !Angle   {-^ Rest angle. -}
     ,dampedRotStiffness :: !CpFloat {-^ Stiffness. -}
     ,dampedRotDamping   :: !Damping {-^ Damping. -}}
    deriving (Eq, Ord, Show)

instance ConstraintType DampedRotarySpring where
  size _ = #{size cpDampedRotarySpring}
  init_ (DampedRotarySpring r s d) = wrDampedRotarySpringInit r s d
  redef ptr _ _ (DampedRotarySpring r s d) = do
      #{poke cpDampedRotarySpring, restAngle} ptr r
      #{poke cpDampedRotarySpring, stiffness} ptr s
      #{poke cpDampedRotarySpring, damping} ptr d

-- | A ratchet constraint.
data Ratchet = Ratchet {
      ratchetPhase :: !CpFloat {-^ Phase. -}
     ,ratchet      :: !CpFloat {-^ Ratchet. -}}
    deriving (Eq, Ord, Show)

instance ConstraintType Ratchet where
  size _ = #{size cpRatchetJoint}
  init_ (Ratchet p r) = wrRatchetJointInit p r
  redef ptr _ _ (Ratchet p r) = do
    #{poke cpRatchetJoint, phase} ptr p
    #{poke cpRatchetJoint, ratchet} ptr r

-- | A rotary limit constraints the difference of angle
--   between two bodies.
data RotaryLimit = RotaryLimit {
      rotaryMinDist :: Distance {-^ Minimum distance. -}
     ,rotaryMaxDist :: Distance {-^ Maximum distance. -}}
    deriving (Eq, Ord, Show)

instance ConstraintType RotaryLimit where
  size _ = #{size cpRotaryLimitJoint}
  init_ (RotaryLimit mn mx) = wrRotaryLimitJointInit mn mx
  redef ptr _ _ (RotaryLimit mn mx) = do
      #{poke cpRotaryLimitJoint, min} ptr mn
      #{poke cpRotaryLimitJoint, max} ptr mx

-- | A simple motor that applies opposite impulses to each
--   body.  The rate is used to compute the torque.
data SimpleMotor = SimpleMotor {
      simpleMotorRate :: CpFloat {-^ Rate. -}}
    deriving (Eq, Ord, Show)

instance ConstraintType SimpleMotor where
  size _ = #{size cpSimpleMotor}
  init_ (SimpleMotor r) = wrSimpleMotorInit r
  redef ptr _ _ (SimpleMotor r) = do
      #{poke cpSimpleMotor, rate} ptr r






-- | Helper functions similar to 'with'.
with1 :: (Storable a) => a -> (Ptr a -> ConstraintInit) -> ConstraintInit
with1 x f c b1 b2 =
    with x $ \x_ptr ->
    f x_ptr c b1 b2
with2 :: (Storable a, Storable b) => a -> b
      -> (Ptr a -> Ptr b -> ConstraintInit) -> ConstraintInit
with2 x y f c b1 b2 =
    with x $ \x_ptr ->
    with y $ \y_ptr ->
    f x_ptr y_ptr c b1 b2
with3 :: (Storable a, Storable b, Storable c) => a -> b -> c
      -> (Ptr a -> Ptr b -> Ptr c -> ConstraintInit) -> ConstraintInit
with3 x y z f c b1 b2 =
    with x $ \x_ptr ->
    with y $ \y_ptr ->
    with z $ \z_ptr ->
    f x_ptr y_ptr z_ptr c b1 b2

-- Boring imports
foreign import ccall unsafe "wrapper.h"
    wrPinJointInit :: VectorPtr -> VectorPtr -> ConstraintInit
foreign import ccall unsafe "wrapper.h"
    wrSlideJointInit :: CpFloat -> CpFloat -> VectorPtr -> VectorPtr -> ConstraintInit
foreign import ccall unsafe "wrapper.h"
    wrPivot1JointInit :: VectorPtr -> ConstraintInit
foreign import ccall unsafe "wrapper.h"
    wrPivot2JointInit :: VectorPtr -> VectorPtr -> ConstraintInit
foreign import ccall unsafe "wrapper.h"
    wrGearJointInit :: CpFloat -> CpFloat -> ConstraintInit
foreign import ccall unsafe "wrapper.h"
    wrGrooveJointInit :: VectorPtr -> VectorPtr -> VectorPtr -> ConstraintInit
foreign import ccall unsafe "wrapper.h"
    wrDampedSpringInit :: CpFloat -> CpFloat -> CpFloat -> VectorPtr -> VectorPtr -> ConstraintInit
foreign import ccall unsafe "wrapper.h"
    wrDampedRotarySpringInit :: CpFloat -> CpFloat -> CpFloat -> ConstraintInit
foreign import ccall unsafe "wrapper.h"
    wrRatchetJointInit :: CpFloat -> CpFloat -> ConstraintInit
foreign import ccall unsafe "wrapper.h"
    wrRotaryLimitJointInit :: CpFloat -> CpFloat -> ConstraintInit
foreign import ccall unsafe "wrapper.h"
    wrSimpleMotorInit :: CpFloat -> ConstraintInit
