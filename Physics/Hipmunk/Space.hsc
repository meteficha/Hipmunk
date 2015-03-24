-----------------------------------------------------------------------------
-- |
-- Module      :  Physics/Hipmunk/Space.hsc
-- Copyright   :  (c) 2008-2010 Felipe A. Lessa
-- License     :  MIT (see LICENSE)
--
-- Maintainer  :  felipe.lessa@gmail.com
-- Stability   :  provisional
-- Portability :  portable (needs FFI)
--
-- The space, where the simulation happens and the various entities
-- interact.
--
-----------------------------------------------------------------------------

module Physics.Hipmunk.Space
    (-- * Callbacks problem
     -- $callbacksProblem

     -- * Creating spaces and adding entities
     Space,
     newSpace,
     freeSpace,
     Entity(spaceAdd, spaceRemove),
     StaticShape(..),

     -- * Properties
     -- ** Iterations
     Iterations,
     iterations,
     -- ** Gravity
     Gravity,
     gravity,
     -- ** Damping
     damping,
     -- ** Time stamp
     TimeStamp,
     timeStamp,
     -- ** Collision Slop
     CollisionSlop,
     collisionSlop,
     -- ** Collision Bias
     CollisionBias,
     collisionBias,
     -- ** Collision Persistence
     collsionPersistence,

     -- * Spatial hashes
     useSpatialHash,

     -- * Reindexing
     reindexStatic,

     -- ** Point query
     -- $point_query
     spaceQuery,
     spaceQueryList,

     -- * Iterate objects in space
     spaceEachBody,
     spaceEachShape,
     spaceEachConstraint,

     -- * Stepping
     step
    )
    where

import qualified Data.Foldable as F
import qualified Data.Map as M
import Control.Exception (bracket)
import Control.Monad (when)
import Data.IORef
import Data.StateVar
import Foreign hiding (new)
#if MIN_VERSION_base(4,7,0)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
#endif
import Foreign.C.Types (CInt(..))
#include "wrapper.h"

import Physics.Hipmunk.Common
import Physics.Hipmunk.Internal
import Physics.Hipmunk.Shape
import Physics.Hipmunk.Constraint


-- $callbacksProblem
--   We have a huge problem for callbacks: we *have* to call
--   'freeHaskellFunPtr' to every Haskell function that was
--   passed via FFI to C code after we don't need them.
--   However, the 'ForeignPtr' that the 'Space' has can
--   portably have finalizers only in the FFI, never in the
--   Haskell land, so we can't run the Haskell function
--   'freeHaskellFunPtr' from a 'ForeignPtr' finalizer.
--
--   There are two options:
--
--     1. Use "Foreign.Concurrent" to add a Haskell finalizer.
--        Under GHC this is great and adds no overhead (maybe there's
--        even less overhead than calling a C function).
--        However "Foreign.Concurrent" is not portable and
--        works only under GHC.
--
--     2. Require that users of the library (you) call
--        a finalizer function when they plan to stop using
--        the space. This adds some burden to the programmer
--        and somehow defeats the purpose of the GC, however
--        it works everywhere.
--
--   As this is a library that intends to be as portable as
--   possible (like Chipmunk itself), of course I chose
--   to follow the second path. This means that your code will
--   run unchanged on every Haskell environment supporting
--   FFI with C99, but also that you have to take care to
--   avoid memory leaks. You've been warned! :)
--
--   Note: callbacks are implemented in
--   "Physics.Hipmunk.Callbacks" module.


-- | Creates a new, empty space.
--   Some of the memory resources associated with the space
--   must be manually freed through 'freeSpace' when the
--   'Space' is no longer necessary.
newSpace :: IO Space
newSpace =
  mallocForeignPtrBytes #{size cpSpace} >>= \sp ->
  withForeignPtr sp $ \sp_ptr -> do
    cpSpaceInit sp_ptr
    let n = nullFunPtr
    entities  <- newIORef M.empty
    callbacks <- newIORef $ CBs (n,n,n,n) M.empty []
    return (P sp entities callbacks)

foreign import ccall unsafe "wrapper.h"
    cpSpaceInit :: SpacePtr -> IO ()

-- | @freeSpace sp@ frees some memory resources that can't
--   be automatically deallocated in a portable way.
--   The space @sp@ then becomes invalid and should
--   not be used (passing @sp@ to any other function,
--   including 'freeSpace', results in undefined behavior).
freeSpace :: Space -> IO ()
freeSpace (P sp entities callbacks) = do
  -- We could use a finalizer to call cpSpaceDestroy,
  -- but it is easier to just destroy it here.  The user
  -- needs to call 'freeSpace' anyways.
  --
  -- It is only safe to free all FunPtr's if either:
  --
  --  a) We remove all reference to them from the Hipmunk side.
  --     This entails one foreign call per callback.
  --
  --  b) The space has been destroyed.
  --
  -- By destroying the space here we guarantee that the callbacks
  -- can't be called from C land anymore.
  withForeignPtr sp cpSpaceDestroy

  -- The only things we *have* to free are the callbacks,
  -- but we'll release all the IORef contents as well.
  let err :: a
      err = error "Physics.Hipmunk.Space: freeSpace already called here."
  writeIORef entities err
  CBs def cbs post <- readIORef callbacks
  writeIORef callbacks err
  freeHandlerFunPtrs def
  freeAll freeHandlerFunPtrs cbs
  freeAll freeHaskellFunPtr  post

freeAll :: F.Foldable t => (a -> IO ()) -> t a -> IO ()
freeAll f = F.foldr ((>>) . f) (return ())

foreign import ccall unsafe "wrapper.h"
    cpSpaceDestroy :: SpacePtr -> IO ()



-- Entity class is imported from Internal.

spaceAddHelper :: (a -> ForeignPtr b)
               -> (SpacePtr -> Ptr b -> IO ())
               -> (a -> Retrievable)
               -> (Space -> a -> IO ())
spaceAddHelper get_ add toRetrievable =
    \(P sp entities _) new_c ->
        let new  = get_ new_c
            key  = unsafeForeignPtrToPtr $ castForeignPtr new
            val  = toRetrievable new_c
        in withForeignPtr sp $ \sp_ptr ->
           withForeignPtr new $ \new_ptr -> do
             add sp_ptr new_ptr
             modifyIORefStrict entities (M.insert key val)

spaceRemoveHelper :: (a -> ForeignPtr b)
                  -> (SpacePtr -> Ptr b -> IO ())
                  -> (Space -> a -> IO ())
spaceRemoveHelper get_ remove =
    \(P sp entities _) old_c -> do
      let old  = get_ old_c
          key  = unsafeForeignPtrToPtr $ castForeignPtr old
      modifyIORefStrict entities (M.delete key)
      withForeignPtr sp $ \sp_ptr ->
        withForeignPtr old $ \old_ptr ->
          remove sp_ptr old_ptr

-- | Strict version of modifyIORef (otherwise the thunks
--   will keep referencing removed entities).
modifyIORefStrict :: IORef a -> (a -> a) -> IO ()
modifyIORefStrict var f = do
  old <- readIORef var
  let new = f old
  new `seq` writeIORef var new

instance Entity Body where
    spaceAdd    = spaceAddHelper    unB cpSpaceAddBody ReB
    spaceRemove = spaceRemoveHelper unB cpSpaceRemoveBody
    entityPtr   = unB
foreign import ccall unsafe "wrapper.h"
    cpSpaceAddBody :: SpacePtr -> BodyPtr -> IO ()
foreign import ccall unsafe "wrapper.h"
    cpSpaceRemoveBody :: SpacePtr -> BodyPtr -> IO ()

instance Entity Shape where
    spaceAdd    = spaceAddHelper    unS cpSpaceAddShape ReS
    spaceRemove = spaceRemoveHelper unS cpSpaceRemoveShape
    entityPtr   = unS
foreign import ccall unsafe "wrapper.h"
    cpSpaceAddShape :: SpacePtr -> ShapePtr -> IO ()
foreign import ccall {- !!! -} safe {- !!! -} "wrapper.h"
    cpSpaceRemoveShape :: SpacePtr -> ShapePtr -> IO ()
    -- may call the 'separate' handler.

instance Entity (Constraint a) where
    spaceAdd    = spaceAddHelper    unC cpSpaceAddConstraint (ReC . forgetC)
    spaceRemove = spaceRemoveHelper unC cpSpaceRemoveConstraint
    entityPtr   = castForeignPtr . unC
foreign import ccall unsafe "wrapper.h"
    cpSpaceAddConstraint :: SpacePtr -> ConstraintPtr -> IO ()
foreign import ccall unsafe "wrapper.h"
    cpSpaceRemoveConstraint :: SpacePtr -> ConstraintPtr -> IO ()


-- | A 'StaticShape' is a 'Shape' container that, when added
--   to a space via 'spaceAdd', is added to the static
--   list of shapes.
--
--   A static shape is one assumed not to move. If you move
--   a static shape after adding it, then you need to 'rehashStatic'.
--
--   You should not add the same shape as active and static,
--   nor should you add as active and try to remove as
--   static or vice versa.
newtype StaticShape = Static {unStatic :: Shape}

instance Entity StaticShape where
    spaceAdd    = spaceAddHelper    (unS . unStatic) cpSpaceAddStaticShape (ReS . unStatic)
    spaceRemove = spaceRemoveHelper (unS . unStatic) cpSpaceRemoveStaticShape
    entityPtr   = castForeignPtr . unS . unStatic
foreign import ccall unsafe "wrapper.h"
    cpSpaceAddStaticShape :: SpacePtr -> ShapePtr -> IO ()
foreign import ccall {- !!! -} safe {- !!! -} "wrapper.h"
    cpSpaceRemoveStaticShape :: SpacePtr -> ShapePtr -> IO ()
    -- may call the 'separate' handler.

-- | The number of iterations to use when solving constraints.
--   (default is 10).
type Iterations = CInt
iterations :: Space -> StateVar Iterations
iterations (P sp _ _) = makeStateVar getter setter
    where
      getter = withForeignPtr sp #{peek cpSpace, iterations}
      setter = withForeignPtr sp . flip #{poke cpSpace, iterations}

-- | The gravity applied to the system. (default is 0)
type Gravity = Vector
gravity :: Space -> StateVar Gravity
gravity (P sp _ _) = makeStateVar getter setter
    where
      getter = withForeignPtr sp #{peek cpSpace, gravity}
      setter = withForeignPtr sp . flip #{poke cpSpace, gravity}

-- | The amount of viscous damping applied to the system.
--   (default is 1)
damping :: Space -> StateVar Damping
damping (P sp _ _) = makeStateVar getter setter
    where
      getter = withForeignPtr sp #{peek cpSpace, damping}
      setter = withForeignPtr sp . flip #{poke cpSpace, damping}

-- | The time stamp of the simulation, increased in 1
--   every time 'step' is called.
type TimeStamp = CInt
timeStamp :: Space -> GettableStateVar TimeStamp
timeStamp (P sp _ _) = makeGettableStateVar $
                       withForeignPtr sp #{peek cpSpace, stamp}

-- | Amount of encouraged penetration between colliding shapes.
--   Used to reduce oscillating contacts and keep the collision cache warm.
--   Defaults to 0.1. If you have poor simulation quality,
--   increase this number as much as possible without allowing visible amounts of overlap.
type CollisionSlop = CpFloat
collisionSlop :: Space -> StateVar CollisionSlop
collisionSlop (P sp _ _) = makeStateVar getter setter
    where
      getter = withForeignPtr sp #{peek cpSpace, collisionSlop}
      setter = withForeignPtr sp . flip #{poke cpSpace, collisionSlop}


-- | Determines how fast overlapping shapes are pushed apart.
--   Expressed as a fraction of the error remaining after each second.
--   Defaults to pow(1.0 - 0.1, 60.0) meaning that Chipmunk fixes 10% of overlap each frame at 60Hz.
type CollisionBias = CpFloat
collisionBias :: Space -> StateVar CollisionBias
collisionBias (P sp _ _) = makeStateVar getter setter
    where
      getter = withForeignPtr sp #{peek cpSpace, collisionBias}
      setter = withForeignPtr sp . flip #{poke cpSpace, collisionBias}

-- | Number of frames that contact information should persist.
--   Defaults to 3. There is probably never a reason to change this value.
collsionPersistence :: Space -> StateVar TimeStamp
collsionPersistence (P sp _ _) = makeStateVar getter setter
    where
      getter = withForeignPtr sp #{peek cpSpace, collisionPersistence}
      setter = withForeignPtr sp . flip #{poke cpSpace, collisionPersistence}

-- | Switch the space to use a spatial has as it's spatial index.
useSpatialHash :: Space -> CpFloat -> CInt -> IO ()
useSpatialHash (P sp _ _) dim count =
    withForeignPtr sp $ \sp_ptr -> do
      cpSpaceUseSpatialHash sp_ptr dim count

foreign import ccall unsafe "wrapper.h"
  cpSpaceUseSpatialHash :: SpacePtr -> CpFloat
                        -> CInt -> IO ()

-- | Update the collision detection info for the static shapes in the space.
reindexStatic :: Space -> IO ()
reindexStatic (P sp _ _) =
    withForeignPtr sp cpSpaceReindexStatic

foreign import ccall unsafe "wrapper.h"
    cpSpaceReindexStatic :: SpacePtr -> IO ()


-- $point_query
--   Point querying uses the spatial hashes to find out
--   in what shapes a point is contained. It is useful,
--   for example, to know if a shape was clicked by
--   the user.

-- | @spaceQuery sp pos l g cb@ will call @cb@ for every
--   shape that
--
--   * Contains point @pos@ (in world's coordinates).
--
--   * Isn't of the same group as @g@.
--
--   * Shares at least one layer with @l@.
--
--   The order in which the callback is called is unspecified.
--   However it is guaranteed that it will be called once,
--   and only once, for each of the shapes described above
--   (and never for those who aren't).
spaceQuery :: Space -> Position -> Layers -> Group -> (Shape -> IO ()) -> IO ()
spaceQuery spce@(P sp _ _) pos layers_ group_ callback =
  withForeignPtr sp $ \sp_ptr ->
  bracket (makePointQueryFunc cb) freeHaskellFunPtr $ \cb_ptr ->
  with pos $ \pos_ptr ->
    wrSpacePointQuery sp_ptr pos_ptr layers_ group_ cb_ptr
 where
   cb shape_ptr _ = do maybeShape <- retrieveShape spce shape_ptr
                       case maybeShape of
                         Just s -> callback s
                         _ -> fail $ "Physics.Hipmunk.Space: the impossible happened!" ++ 
                                     " spaceQuery callback received a pointer expected to be a shape," ++ 
                                     " but got something else!"

type PointQueryFunc = ShapePtr -> Ptr () -> IO ()
type PointQueryFuncPtr = FunPtr PointQueryFunc
foreign import ccall "wrapper"
    makePointQueryFunc :: PointQueryFunc -> IO PointQueryFuncPtr
foreign import ccall safe "wrapper.h"
    wrSpacePointQuery :: SpacePtr -> VectorPtr -> Layers -> Group
                      -> PointQueryFuncPtr -> IO ()


-- | @spaceQueryList sp pos l g@ acts like 'spaceQuery' but
--   returns a list of 'Shape's instead of calling a callback.
--   This is just a convenience function.
spaceQueryList :: Space -> Position -> Layers -> Group -> IO [Shape]
spaceQueryList spce pos layers_ group_ = do
  var <- newIORef []
  spaceQuery spce pos layers_ group_ $ modifyIORef var . (:)
  readIORef var

-- | Call @callback for each body in the space.
type BodyIteratorFunc = BodyPtr -> Ptr () -> IO ()
type BodyIteratorFuncPtr = FunPtr BodyIteratorFunc
spaceEachBody :: Space -> (Body -> IO ()) -> IO ()
spaceEachBody spce@(P sp _ _) callback =
  withForeignPtr sp $ \sp_ptr ->
  bracket (makeBodyIteratorFunc cb) freeHaskellFunPtr $ \cb_ptr ->
    wrSpaceEachBody sp_ptr cb_ptr
  where
    cb body_ptr _ = do maybeBody <- retrieveBody spce body_ptr
                       case maybeBody of
                         Just b -> callback b
                         _ -> fail $ "Physics.Hipmunk.Space: the impossible happened!" ++ 
                                     " spaceEachBody callback received a pointer expected to be a body," ++ 
                                     " but got something else!"

foreign import ccall "wrapper"
  makeBodyIteratorFunc :: BodyIteratorFunc -> IO BodyIteratorFuncPtr
foreign import ccall safe "wrapper.h"
  wrSpaceEachBody :: SpacePtr -> BodyIteratorFuncPtr -> IO ()

-- | Call @callback for each shape in the space.
type ShapeIteratorFunc = ShapePtr -> Ptr () -> IO ()
type ShapeIteratorFuncPtr = FunPtr ShapeIteratorFunc
spaceEachShape :: Space -> (Shape -> IO ()) -> IO ()
spaceEachShape spce@(P sp _ _) callback =
  withForeignPtr sp $ \sp_ptr ->
  bracket (makeShapeIteratorFunc cb) freeHaskellFunPtr $ \cb_ptr ->
    wrSpaceEachShape sp_ptr cb_ptr
  where 
    cb shape_ptr _ = do maybeShape <- retrieveShape spce shape_ptr
                        case maybeShape of
                          Just s -> callback s
                          _ -> fail $ "Physics.Hipmunk.Space: the impossible happened!" ++ 
                                      " spaceEachShape callback received a pointer expected to be a shape," ++ 
                                      " but got something else!"

foreign import ccall "wrapper"
  makeShapeIteratorFunc :: ShapeIteratorFunc -> IO ShapeIteratorFuncPtr
foreign import ccall safe "wrapper.h"
  wrSpaceEachShape :: SpacePtr -> ShapeIteratorFuncPtr -> IO ()

-- | Call @callback for each constraint in the space.
type ConstraintIteratorFunc = ConstraintPtr -> Ptr () -> IO ()
type ConstraintIteratorFuncPtr = FunPtr ConstraintIteratorFunc
spaceEachConstraint :: Space -> (Constraint Unknown -> IO ()) -> IO ()
spaceEachConstraint spce@(P sp _ _) callback =
  withForeignPtr sp $ \sp_ptr ->
  bracket (makeConstraintIteratorFunc cb) freeHaskellFunPtr $ \cb_ptr ->
    wrSpaceEachConstraint sp_ptr cb_ptr
  where 
    cb constraint_ptr _ = do maybeConstraint <- retrieveConstraint spce constraint_ptr
                             case maybeConstraint of
                               Just c -> callback c
                               _ -> fail $ "Physics.Hipmunk.Space: the impossible happened!" ++ 
                                           " spaceEachConstraint callback received a pointer expected to be a constraint," ++ 
                                           " but got something else!"

foreign import ccall "wrapper"
  makeConstraintIteratorFunc :: ConstraintIteratorFunc -> IO ConstraintIteratorFuncPtr
foreign import ccall safe "wrapper.h"
  wrSpaceEachConstraint :: SpacePtr -> ConstraintIteratorFuncPtr -> IO ()

-- | @step sp dt@ will update the space @sp@ for a @dt@ time
--   step.
--
--   It is highly recommended to use a fixed @dt@ to increase
--   the efficiency of contact persistence. Some tips may be
--   found in <http://www.gaffer.org/game-physics/fix-your-timestep>.
step :: Space -> Time -> IO ()
step (P sp _ callbacks) dt = do
  withForeignPtr sp $ \sp_ptr -> do
    cpSpaceStep sp_ptr dt
  cbs@(CBs {cbsPostStep = post}) <- readIORef callbacks
  when (not $ null post) $ do
    freeAll freeHaskellFunPtr post
    writeIORef callbacks (cbs {cbsPostStep = []})


-- IMPORTANT! This call can (and probably will) callback into Haskell.
foreign import ccall {- !!! -} safe {- !!! -}
    cpSpaceStep :: SpacePtr -> Time -> IO ()
