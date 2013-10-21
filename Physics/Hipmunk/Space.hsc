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
     -- ** Elastic iterations
     ElasticIterations,
     elasticIterations,
     -- ** Gravity
     Gravity,
     gravity,
     -- ** Damping
     damping,
     -- ** Time stamp
     TimeStamp,
     timeStamp,

     -- * Spatial hashes
     -- $resizing
     resizeStaticHash,
     resizeActiveHash,
     rehashStatic,
     -- ** Point query
     -- $point_query
     spaceQuery,
     spaceQueryList,

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
import Foreign.C.Types (CInt(..))
#include "wrapper.h"

import Physics.Hipmunk.Common
import Physics.Hipmunk.Internal
import Physics.Hipmunk.Shape


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
               -> (a -> Maybe Shape)
               -> (Space -> a -> IO ())
spaceAddHelper get_ add toShape =
    \(P sp entities _) new_c ->
        let new  = get_ new_c
            key  = unsafeForeignPtrToPtr $ castForeignPtr new
            val  = case toShape new_c of
                     Just shape -> Right shape
                     Nothing    -> Left (castForeignPtr new)
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
    spaceAdd    = spaceAddHelper    unB cpSpaceAddBody (const Nothing)
    spaceRemove = spaceRemoveHelper unB cpSpaceRemoveBody
    entityPtr   = unB
foreign import ccall unsafe "wrapper.h"
    cpSpaceAddBody :: SpacePtr -> BodyPtr -> IO ()
foreign import ccall unsafe "wrapper.h"
    cpSpaceRemoveBody :: SpacePtr -> BodyPtr -> IO ()

instance Entity Shape where
    spaceAdd    = spaceAddHelper    unS cpSpaceAddShape Just
    spaceRemove = spaceRemoveHelper unS cpSpaceRemoveShape
    entityPtr   = unS
foreign import ccall unsafe "wrapper.h"
    cpSpaceAddShape :: SpacePtr -> ShapePtr -> IO ()
foreign import ccall {- !!! -} safe {- !!! -} "wrapper.h"
    cpSpaceRemoveShape :: SpacePtr -> ShapePtr -> IO ()
    -- may call the 'separate' handler.

instance Entity (Constraint a) where
    spaceAdd    = spaceAddHelper    unC cpSpaceAddConstraint (const Nothing)
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
    spaceAdd    = spaceAddHelper    (unS . unStatic) cpSpaceAddStaticShape (Just . unStatic)
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

-- | The number of elastic iterations to use when solving
--   constraints.  If @0@, then old-style elastic code is used.
--   (default is 0).
--
--   This property is deprecated.  You should no longer need to
--   set any value other than the default.
type ElasticIterations = CInt
{-# DEPRECATED elasticIterations "Elastic iterations should no longer be needed" #-}
elasticIterations :: Space -> StateVar ElasticIterations
elasticIterations (P sp _ _) = makeStateVar getter setter
    where
      getter = withForeignPtr sp #{peek cpSpace, elasticIterations}
      setter = withForeignPtr sp . flip #{poke cpSpace, elasticIterations}

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





-- $resizing
--   @'resizeStaticHash' sp dim count@ resizes the static
--   hash of space @sp@ to have hash cells of size @dim@
--   and suggested minimum number of cells @count@.
--   @'resizeActiveHash' sp dim count@ works the same way
--   but modifying the active hash of the space.
--
--   Chipmunk's performance is highly sensitive to both
--   parameters, which should be hand-tuned to maximize
--   performance. It is in general recommended to set @dim@ as
--   the average object size and @count@ around 10 times the
--   number of objects in the hash. Usually bigger numbers are
--   better to @count@, but only to a certain point. By default
--   dim is @100.0@ and count is @1000@.
--
--   Note that in the case of the static hash you may try
--   larger numbers as the static hash is only rehashed
--   when requested by 'rehashStatic', however that will
--   use more memory.

resizeStaticHash :: Space -> Distance -> CInt -> IO ()
resizeStaticHash (P sp _ _) dim count =
    withForeignPtr sp $ \sp_ptr -> do
      cpSpaceResizeStaticHash sp_ptr dim count

foreign import ccall unsafe "wrapper.h"
    cpSpaceResizeStaticHash :: SpacePtr -> CpFloat
                            -> CInt -> IO ()

resizeActiveHash :: Space -> Distance -> CInt -> IO ()
resizeActiveHash (P sp _ _) dim count =
  withForeignPtr sp $ \sp_ptr -> do
    cpSpaceResizeActiveHash sp_ptr dim count

foreign import ccall unsafe "wrapper.h"
    cpSpaceResizeActiveHash :: SpacePtr -> CpFloat
                            -> CInt -> IO ()

-- | Rehashes the shapes in the static spatial hash.
--   You only need to call this if you move one of the
--   static shapes.
rehashStatic :: Space -> IO ()
rehashStatic (P sp _ _) =
    withForeignPtr sp cpSpaceRehashStatic

foreign import ccall unsafe "wrapper.h"
    cpSpaceRehashStatic :: SpacePtr -> IO ()




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
   cb shape_ptr _ = retriveShape spce shape_ptr >>= callback

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

