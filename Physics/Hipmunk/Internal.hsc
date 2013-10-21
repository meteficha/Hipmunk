-----------------------------------------------------------------------------
-- |
-- Module      :  Physics/Hipmunk/Internal.hsc
-- Copyright   :  (c) 2008-2010 Felipe A. Lessa
-- License     :  MIT (see LICENSE)
--
-- Maintainer  :  felipe.lessa@gmail.com
-- Stability   :  provisional
-- Portability :  portable (needs FFI)
--
-----------------------------------------------------------------------------

module Physics.Hipmunk.Internal
    (VectorPtr,

     BodyPtr,
     Body(..),
     unB,

     ShapePtr,
     Shape(..),
     unS,

     ConstraintPtr,
     Constraint(..),
     unC,
     Unknown(..),
     ConstraintInit,
     ConstraintType(..),

     SpacePtr,
     Space(..),
     Callbacks(..),
     HandlerFunPtrs,
     unP,
     retriveShape,
     freeHandlerFunPtrs,

     Entity(..),

     ArbiterPtr,

     Contact(..),
     ContactPtr
    )
    where

import qualified Data.Map as M
import Control.Monad (when)
import Data.IORef
import Data.Map (Map)
import Foreign
#include "wrapper.h"

import Physics.Hipmunk.Common


type VectorPtr = Ptr Vector



-- | A rigid body representing the physical properties of an
--   object, but without a shape. It may help to think of it as a
--   particle that is able to rotate.
newtype Body = B (ForeignPtr Body)
type BodyPtr = Ptr Body

unB :: Body -> ForeignPtr Body
unB (B b) = b

instance Eq Body where
    B b1 == B b2 = b1 == b2

instance Ord Body where
    B b1 `compare` B b2 = b1 `compare` b2



-- | A collision shape is attached to a 'Body' to define its
--   shape. Multiple shapes may be attached, including
--   overlapping ones (shapes of a body don't generate collisions
--   with each other).
--
--   Note that to have any effect, a 'Shape' must also be
--   added to a 'Space', even if the body was already added.
data Shape = S !(ForeignPtr Shape) !Body
type ShapePtr = Ptr Shape

-- Note also that we have to maintain a reference to the
-- 'Body' to avoid garbage collection in the case that
-- the user doesn't add the body to a space and don't keep
-- a reference (common when adding bodies with infinite mass).
--
-- However, the body doesn't need to keep references to
-- the attached shapes because cpBody do not reference them,
-- so it wouldn't notice at all if they disappeared =).
-- A space would notice, but then the space will keep its
-- own reference the the shape.

unS :: Shape -> ForeignPtr Shape
unS (S s _) = s

instance Eq Shape where
    S s1 _ == S s2 _ = s1 == s2

instance Ord Shape where
    S s1 _ `compare` S s2 _ = s1 `compare` s2



-- | Represents a constraint between two bodies. Don't forget to
--   add the bodies and the constraint itself to the space.
--   The phantom type indicates the type of the constraint.
data Constraint a = C !(ForeignPtr (Constraint ())) !Body !Body
type ConstraintPtr = Ptr (Constraint ())

unC :: Constraint a -> ForeignPtr (Constraint ())
unC (C j _ _) = j

instance Eq (Constraint a) where
    C j1 _ _ == C j2 _ _ = j1 == j2

instance Ord (Constraint a) where
    C j1 _ _ `compare` C j2 _ _ = j1 `compare` j2

-- | An unknown constraint \"type\".  Note that this isn't a
--   'ConstraintType' because you can't create a constraint of
--   @Unknown@ type.
data Unknown = Unknown

-- | Type of generic constraint initializar.
type ConstraintInit = ConstraintPtr -> BodyPtr -> BodyPtr -> IO ()

-- | Internal.  Class implemented by all constraint types.
class ConstraintType a where
  size  :: a -> Int
  init_ :: a -> ConstraintInit
  redef :: ConstraintPtr -> Body -> Body -> a -> IO ()



-- | A space is where the simulation really occurs. You add
--   bodies, shapes and constraints to a space and then @step@ it
--   to update it as whole.
data Space = P !(ForeignPtr Space)
               !(IORef Entities)   -- Active and static entities
               !(IORef Callbacks)  -- Added callbacks
type SpacePtr  = Ptr Space
type Entities  = Map (Ptr ()) (Either (ForeignPtr ()) Shape)
data Callbacks = CBs {cbsDefault  :: HandlerFunPtrs
                     ,cbsHandlers :: Map (CollisionType_, CollisionType_) HandlerFunPtrs
                     ,cbsPostStep :: [FunPtr ()]}
type HandlerFunPtrs = (FunPtr (), FunPtr (), FunPtr (), FunPtr ())
type CollisionType_ = #{type cpCollisionType}
-- Duplicated to avoid bringing the documentation from Shape module.

unP :: Space -> ForeignPtr Space
unP (P sp _ _) = sp

instance Eq Space where
    P s1 _ _ == P s2 _ _ = s1 == s2

instance Ord Space where
    P s1 _ _ `compare` P s2 _ _ = s1 `compare` s2

-- | Internal. Retrive a 'Shape' from a 'ShapePtr' and a 'Space'.
retriveShape :: Space -> ShapePtr -> IO Shape
retriveShape (P _ entities _) ptr = do
  ent <- readIORef entities
  let Just (Right shape) = M.lookup (castPtr ptr) ent
  return shape

-- | Internal.  Free all function pointers of this handler.
freeHandlerFunPtrs :: HandlerFunPtrs -> IO ()
freeHandlerFunPtrs (p1,p2,p3,p4) = f p1 >> f p2 >> f p3 >> f p4
    where f p = when (p /= nullFunPtr) (freeHaskellFunPtr p)




-- | Type class implemented by entities that can be
--   added to a space.
class Entity a where
    -- | Add an entity to a 'Space'. Don't add the same
    --   entity twice to a space.
    spaceAdd :: Space -> a -> IO ()
    -- | Remove an entity from a 'Space'. Don't remove
    --   an entity that wasn't added.
    spaceRemove :: Space -> a -> IO ()
    -- | Internal function.  Retrive the pointer of this entity.
    entityPtr :: a -> ForeignPtr a


-- | Arbiters are used within callbacks.  We don't expose them to
-- the user.
data Arbiter
type ArbiterPtr = Ptr Arbiter



-- 'Contact's are an exception to the pattern we've been following
-- as we're going to use StorableArray with them, so we need
-- them to be Storable (like Vector).

-- | A 'Contact' contains information about a collision.
--   It is passed to 'Physics.Hipmunk.Space.Full'.
--
--   The fields 'ctJnAcc' and 'ctJtAcc' do not have any meaningfull
--   value until 'Physics.Hipmunk.Space.step' has returned
--   (i.e. during a call to a callback this information
--   contains garbage), and by extension you can only know
--   the impulse sum after @step@ returns as well.
--
--   /IMPORTANT:/ You may maintain a reference to an array of
--   @Contact@s that was passed to a callback to do any other
--   processing later. However, /a new call to/ @step@ /will/
--   /invalidate any of those arrays!/ Be careful.
data Contact = Contact {
      ctPos    :: Position,
      -- ^ Position of the collision in world's coordinates.

      ctNormal :: Vector,
      -- ^ Normal of the collision.

      ctDist   :: CpFloat,
      -- ^ Penetration distance of the collision.

      ctJnAcc  :: CpFloat,
      -- ^ Normal component of final impulse applied.
      --   (Valid only after @step@ finishes.)

      ctJtAcc  :: CpFloat
      -- ^ Tangential component of final impulse applied.
      --   (Valid only after @step@ finishes.)
    }
               deriving (Eq, Ord, Show)

type ContactPtr = Ptr Contact

instance Storable Contact where
    sizeOf _    = #{size cpContact}
    alignment _ = alignment (undefined :: Vector)
    peek ptr    = do
      p     <- #{peek cpContact, p} ptr
      n     <- #{peek cpContact, n} ptr
      dist  <- #{peek cpContact, dist} ptr
      jnAcc <- #{peek cpContact, jnAcc} ptr
      jtAcc <- #{peek cpContact, jtAcc} ptr
      return $ Contact {ctPos    = p
                       ,ctNormal = n
                       ,ctDist   = dist
                       ,ctJnAcc  = jnAcc
                       ,ctJtAcc  = jtAcc}
    poke ptr c = do
      #{poke cpContact, p} ptr (ctPos c)
      #{poke cpContact, n} ptr (ctNormal c)
      #{poke cpContact, dist} ptr (ctDist c)
      #{poke cpContact, jnAcc} ptr (ctJnAcc c)
      #{poke cpContact, jtAcc} ptr (ctJtAcc c)
