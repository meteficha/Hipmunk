-----------------------------------------------------------------------------
-- |
-- Module      :  Physics/Hipmunk/Body.hsc
-- Copyright   :  (c) 2008-2010 Felipe A. Lessa
-- License     :  MIT (see LICENSE)
--
-- Maintainer  :  felipe.lessa@gmail.com
-- Stability   :  provisional
-- Portability :  portable (needs FFI)
--
-- Rigid bodies and their properties.
--
-----------------------------------------------------------------------------

module Physics.Hipmunk.Body
    (-- * Creating
     Body,
     newBody,

     -- * Static properties
     -- ** Basic
     -- *** Mass
     Mass,
     mass,
     -- *** Moment of inertia
     Moment,
     moment,

     -- ** Linear components of motion
     -- *** Position
     position,
     -- *** Velocity
     Velocity,
     velocity,
     maxVelocity,
     -- *** Force
     Force,
     force,

     -- ** Angular components of motion
     -- *** Angle
     angle,
     -- *** Angular velocity
     AngVel,
     angVel,
     maxAngVel,
     -- *** Torque
     Torque,
     torque,

     -- * Dynamic properties
     slew,
     updateVelocity,
     updatePosition,
     resetForces,
     applyForce,
     applyOnlyForce,
     applyImpulse,
     applyDampedSpring,

     -- * Utilities
     localToWorld,
     worldToLocal
    )
    where

import Data.StateVar
import Foreign hiding (rotate, new)
#include "wrapper.h"

import Physics.Hipmunk.Common
import Physics.Hipmunk.Internal

-- | @newBody mass inertia@ creates a new 'Body' with
--   the given mass and moment of inertia.
--
--   It is recommended to call 'setPosition' afterwards.
newBody :: Mass -> Moment -> IO Body
newBody mass_ inertia = do
  b <- mallocForeignPtrBytes #{size cpBody}
  withForeignPtr b $ \ptr -> do
    cpBodyInit ptr mass_ inertia
  return (B b)

foreign import ccall unsafe "wrapper.h"
    cpBodyInit :: BodyPtr -> CpFloat -> CpFloat -> IO ()



type Mass = CpFloat

mass :: Body -> StateVar Mass
mass (B b) = makeStateVar getter setter
    where
      getter = withForeignPtr b #{peek cpBody, m}
      setter = withForeignPtr b . flip cpBodySetMass

foreign import ccall unsafe "wrapper.h"
    cpBodySetMass :: BodyPtr -> Mass -> IO ()


type Moment = CpFloat

moment :: Body -> StateVar Moment
moment (B b) = makeStateVar getter setter
    where
      getter = withForeignPtr b #{peek cpBody, i}
      setter = withForeignPtr b . flip cpBodySetMoment

foreign import ccall unsafe "wrapper.h"
    cpBodySetMoment :: BodyPtr -> CpFloat -> IO ()



angle :: Body -> StateVar Angle
angle (B b) = makeStateVar getter setter
    where
      getter = withForeignPtr b #{peek cpBody, a}
      setter = withForeignPtr b . flip cpBodySetAngle

foreign import ccall unsafe "wrapper.h"
    cpBodySetAngle :: BodyPtr -> CpFloat -> IO ()



-- | Note that using this function to change the position
--   on every step is not recommended as it may leave
--   the velocity out of sync.
position :: Body -> StateVar Position
position (B b) = makeStateVar getter setter
    where
      getter = withForeignPtr b #{peek cpBody, p}
      setter = withForeignPtr b . flip #{poke cpBody, p}


type Velocity = Vector

velocity :: Body -> StateVar Velocity
velocity (B b) = makeStateVar getter setter
    where
      getter = withForeignPtr b #{peek cpBody, v}
      setter = withForeignPtr b . flip #{poke cpBody, v}

-- | Maximum linear velocity after integrating, defaults to infinity.
maxVelocity :: Body -> StateVar CpFloat
maxVelocity (B b) = makeStateVar getter setter
    where
      getter = withForeignPtr b #{peek cpBody, v_limit}
      setter = withForeignPtr b . flip #{poke cpBody, v_limit}



type Force = Vector

force :: Body -> StateVar Force
force (B b) = makeStateVar getter setter
    where
      getter = withForeignPtr b #{peek cpBody, f}
      setter = withForeignPtr b . flip #{poke cpBody, f}


type AngVel = CpFloat

angVel :: Body -> StateVar AngVel
angVel (B b) = makeStateVar getter setter
    where
      getter = withForeignPtr b #{peek cpBody, w}
      setter = withForeignPtr b . flip #{poke cpBody, w}

-- | Maximum angular velocity after integrating, defaults to infinity.
maxAngVel :: Body -> StateVar CpFloat
maxAngVel (B b) = makeStateVar getter setter
    where
      getter = withForeignPtr b #{peek cpBody, w_limit}
      setter = withForeignPtr b . flip #{poke cpBody, w_limit}



type Torque = CpFloat

torque :: Body -> StateVar Torque
torque (B b) = makeStateVar getter setter
    where
      getter = withForeignPtr b #{peek cpBody, t}
      setter = withForeignPtr b . flip #{poke cpBody, t}


-- | @slew b newpos dt@ changes the body @b@'s velocity
--   so that it reaches @newpos@ in @dt@ time.
--
--   It is usually used to change the position of a
--   static body in the world. In that case, remember
--   to reset the velocity to zero afterwards!
slew :: Body -> Position -> Time -> IO ()
slew (B b) newpos dt = do
  withForeignPtr b $ \ptr -> do
    p <- #{peek cpBody, p} ptr
    #{poke cpBody, v} ptr $ (newpos - p) `scale` (recip dt)


-- | @updateVelocity b gravity damping dt@ redefines body @b@'s
--   linear and angular velocity to account for the force\/torque
--   being applied to it, the gravity and a damping factor
--   during @dt@ time using Euler integration.
--
--   Note that this function only needs to be called if you
--   are not adding the body to a space.
updateVelocity :: Body -> Vector -> Damping -> Time -> IO ()
updateVelocity (B b) g d dt =
  withForeignPtr b $ \b_ptr ->
  with g $ \g_ptr -> do
    wrBodyUpdateVelocity b_ptr g_ptr d dt

foreign import ccall unsafe "wrapper.h"
    wrBodyUpdateVelocity :: BodyPtr -> VectorPtr
                         -> CpFloat -> Time -> IO ()


-- | @updatePosition b dt@ redefines the body position like
--   'updateVelocity' (and it also shouldn't be called if you
--   are adding this body to a space).
updatePosition :: Body -> Time -> IO ()
updatePosition (B b) dt = do
  withForeignPtr b $ \ptr -> do
    cpBodyUpdatePosition ptr dt

foreign import ccall unsafe "wrapper.h"
    cpBodyUpdatePosition :: BodyPtr -> Time -> IO ()


-- | @resetForces b@ redefines as zero all forces and torque
--   acting on body @b@.
resetForces :: Body -> IO ()
resetForces b = do
  force  b $= 0
  torque b $= 0


-- | @applyForce b f r@ applies to the body @b@ the force
--   @f@ with offset @r@, both vectors in world coordinates.
--   This is the most stable way to change a body's velocity.
--
--   Note that the force is accumulated in the body, so you
--   may need to call 'applyOnlyForce'.
applyForce :: Body -> Vector -> Position -> IO ()
applyForce (B b) f p =
  withForeignPtr b $ \b_ptr ->
  with f $ \f_ptr ->
  with p $ \p_ptr -> do
    wrBodyApplyForce b_ptr f_ptr p_ptr

foreign import ccall unsafe "wrapper.h"
    wrBodyApplyForce :: BodyPtr -> VectorPtr -> VectorPtr -> IO ()


-- | @applyOnlyForce b f r@ applies a force like 'applyForce',
--   but calling 'resetForces' before. Note that using this
--   function is preferable as it is optimized over this common
--   case.
applyOnlyForce :: Body -> Vector -> Position -> IO ()
applyOnlyForce b f p = do
  force  b $= f
  torque b $= p `cross` f


-- | @applyImpulse b j r@ applies to the body @b@ the impulse
--   @j@ with offset @r@, both vectors in world coordinates.
applyImpulse :: Body -> Vector -> Position -> IO ()
applyImpulse (B b) j r =
  withForeignPtr b $ \b_ptr ->
  with j $ \j_ptr ->
  with r $ \r_ptr -> do
    wrBodyApplyImpulse b_ptr j_ptr r_ptr

foreign import ccall unsafe "wrapper.h"
    wrBodyApplyImpulse :: BodyPtr -> VectorPtr -> VectorPtr -> IO ()


-- | @dampedSpring (b1,a1) (b2,a2) rlen k dmp dt@ applies a damped
--   spring force between bodies @b1@ and @b2@ at anchors
--   @a1@ and @a2@, respectively. @k@ is the spring constant
--   (force\/distance), @rlen@ is the rest length of the spring,
--   @dmp@ is the damping constant (force\/velocity), and @dt@
--   is the time step to apply the force over. Both anchors are
--   in body coordinates.
--
--   Note: large damping values can be unstable, you should use
--   the damped spring constraint instead.
applyDampedSpring :: (Body,Position) -> (Body,Position) -> Distance
                  -> CpFloat -> Damping -> Time -> IO ()
applyDampedSpring (B b1,a1) (B b2, a2) rlen k dmp dt =
  withForeignPtr b1 $ \b1_ptr ->
  withForeignPtr b2 $ \b2_ptr ->
  with a1 $ \a1_ptr ->
  with a2 $ \a2_ptr -> do
    wrApplyDampedSpring b1_ptr b2_ptr a1_ptr a2_ptr rlen k dmp dt

foreign import ccall unsafe "wrapper.h"
    wrApplyDampedSpring :: BodyPtr -> BodyPtr -> VectorPtr -> VectorPtr
                        -> CpFloat -> CpFloat -> CpFloat -> Time -> IO ()


-- | For a vector @p@ in body @b@'s coordinates,
--   @localToWorld b p@ returns the corresponding vector
--   in world coordinates.
localToWorld :: Body -> Position -> IO Position
localToWorld (B b) p =
  withForeignPtr b $ \b_ptr ->
  with p $ \p_ptr -> do
    wrBodyLocal2World b_ptr p_ptr
    peek p_ptr

foreign import ccall unsafe "wrapper.h"
    wrBodyLocal2World :: BodyPtr -> VectorPtr -> IO ()


-- | For a vector @p@ in world coordinates,
--   @worldToLocal b p@ returns the corresponding vector
--   in body @b@'s coordinates.
worldToLocal :: Body -> Position -> IO Position
worldToLocal (B b) p =
  withForeignPtr b $ \b_ptr ->
  with p $ \p_ptr -> do
    wrBodyWorld2Local b_ptr p_ptr
    peek p_ptr

foreign import ccall unsafe "wrapper.h"
    wrBodyWorld2Local :: BodyPtr -> VectorPtr -> IO ()
