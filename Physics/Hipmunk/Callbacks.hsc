-----------------------------------------------------------------------------
-- |
-- Module      :  Physics/Hipmunk/Callbacks.hsc
-- Copyright   :  (c) 2008-2010 Felipe A. Lessa
-- License     :  MIT (see LICENSE)
--
-- Maintainer  :  felipe.lessa@gmail.com
-- Stability   :  provisional
-- Portability :  portable (needs FFI)
--
-- Callbacks are functions that are called whenever certain
-- events happen.  For example, you may use a callback to know
-- when a player bumps into an enemy.  Or when a bullet hits its
-- target.  Or how strong was a collision.
-----------------------------------------------------------------------------

module Physics.Hipmunk.Callbacks
    (-- * Collision handlers

     -- $collisionHandlers

     -- * Callback types
     Begin,
     PreSolve,
     PostSolve,
     Separate,
     PostStep,
     NotSeparate,
     NotPostStep,

     -- * Callback monad
     Callback,

     -- * Callback functions
     -- | Functions that may be called within a callback.  We
     -- divide them in groups according to the kinds of callbacks
     -- allowed to use them.

     -- ** General
     -- | These functions can be used in any kind of callback.
     shapes,
     isFirstContact,

     -- ** Only when colliding
     -- | These functions make sense only if the shapes @a@ and
     -- @b@ are colliding, i.e., outside a @Separate@ event.
     normal,
     points,

     -- ** Only in @PostStep@
     -- | These functions can be used only in @PostStep@
     -- events.  Use the 'postStep' function to add a
     -- @PostStep@ callback.
     totalImpulse,
     totalImpulseWithFriction,
     currentSpaceAdd,
     currentSpaceRemove,

     -- * Adding post-step callbacks
     postStep,
     unsafePostStep,

     -- * Adding collision handlers
     CollisionHandler(..),
     setDefaultCollisionHandler,
     addCollisionHandler,
     removeCollisionHandler
    )
    where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Foreign hiding (new)
import Foreign.C.Types (CInt(..))
#include "wrapper.h"

import Physics.Hipmunk.Common
import Physics.Hipmunk.Internal
import Physics.Hipmunk.Shape


-- $collisionHandlers
--   Collision handlers ('CollisionHandler') are tuples of 4
--   callback functions.  Each function is called when a
--   different kind of collision events happens.  Most of the
--   time they are triggered inside a
--   'Physics.Hipmunk.Space.step' function call, however they can
--   also be called when a shape is removed.
--
--   Collision events always happen between two shapes, say @a@
--   and @b@.  Possible events are:
--
--     ['Begin'] Shapes @a@ and @b@ started touching for the
--     first time on this step (that is, they were not touching
--     on the last step).  May return @False@ to ignore the
--     collision entirely or @True@ to process it normally.  If
--     the collision is ignored, then you /will not/ receive
--     @PreSolve@ or @PostSolve@ events, however you /will/
--     receive a @Separate@ event when they stop overlapping.
--
--     ['PreSolve'] Shapes @a@ and @b@ are touching during this
--     step.  May return @False@ to ignore the collision for this
--     step or @True@ to process it normally.  You may also use
--     this step to give custom friction or elasticity values.
--
--     ['PostSolve'] Shapes @a@ and @b@ are touching and their
--     collision response has been processed.  You can retrive
--     the collision force (e.g. to calculate sound volumes or
--     damage amounts).
--
--     ['Separate'] Shapes @a@ and @b@ stopped touching (or
--     overlapping, if the collision was ignored on @Begin@)
--     for the first time on this step (that is, on the last step
--     they were touching or overlapping).
--
--   You may have many different kinds of collision handlers.
--   Each collision handler is associated with two
--   'CollisionType's.  Whenever shapes @a@ and @b@ collide, if
--   there was a callback associated with @a@'s and @b@'s
--   collision types, then it is called.  Otherwise the default
--   callback is called.  The default callback is also
--   overrideable.
--
--   The callbacks themselves may execute arbitrary operations
--   with a simple exception: /callbacks cannot add or remove/
--   /entities from the space/.  To that end, there exist another
--   kind of callback:
--
--     ['PostStep'] Called on the end of the 'step' function.
--     This is the only callback where you may remove entities
--     from the space, using 'currentSpaceAdd' or
--     'currentSpaceRemove'.
--
--   Post-step callbacks are not collision handlers, because they
--   aren't called for each collision.  Instead, inside a
--   collision handler you may use 'postStep' to add a post-step
--   callback that will be called when the 'step' ends.  Each
--   post-step callback is associated with an entity, and there
--   can be only one post-step callback per entity.


-- | Phantom type used in @Begin@ collision events.
data Begin

-- | Phantom type used in @PreSolve@ collision events.
data PreSolve

-- | Phantom type used in @PostSolve@ collision events.
data PostSolve

-- | Phantom type used in @Separate@ collision events.
data Separate

-- | Phantom type used in @PostStep@ callbacks.
--
--  The phantom type @t@ inside this @PostStep@ phantom type is
--  the collision event that originated this @PostStep@ callback.
--  For example, if you add a @PostStep@ from a @Begin@ handler,
--  then it will have type @PostStep Begin@.  It is used by the
--  @PostStep@'s instance of 'NotSeparate'.
data PostStep t

-- | Class of collision events other than @Separate@.  That is,
-- collision events where the shapes are touching or overlapping.
class NotSeparate t where
instance NotSeparate Begin where
instance NotSeparate PreSolve where
instance NotSeparate PostSolve where
instance NotSeparate t => NotSeparate (PostStep t) where

-- | Class of callbacks called from collision events. That is,
-- everything other than 'PostStep'.
class NotPostStep t where
instance NotPostStep Begin where
instance NotPostStep PreSolve where
instance NotPostStep PostSolve where
instance NotPostStep Separate where

-- | Monad where callbacks are run.  Within this monad you have
-- access to functions describing the collision.  You can also
-- run any IO actions using 'liftIO' from @transformers@ package.
-- However, remember not to call 'spaceAdd' or 'spaceRemove'
-- outside a @PostStep@ callback -- use 'postStep' instead, for example:
--
-- @
-- postStep entity (currentSpaceRemove entity)
-- @
--
-- The phantom type @t@ describes the type of callback, which can be
--
--   ['Begin'] When the collision first occurs.
--
--   ['PreSolve'] Before the collision is processed.
--
--   ['PostSolve'] After the collision is processed.
--
--   ['Separate'] When the collision ends.
--
--   ['PostStep'] After the 'step' finishes.
--
-- This phantom type is used to disallow invalid operations.  For
-- example, you can't calculate the normal of a collision if you
-- are in a @Separate@ event, as there is no collision inside
-- this event.  And you can't add a new post-step callback inside
-- a post-step callback.
newtype Callback t a = CB {unCB :: ReaderT CallbackEnv IO a}
data CallbackEnv = CE {ceSpace   :: !Space,
                       ceArbiter :: {-# UNPACK #-} !ArbiterPtr}

instance Functor (Callback t) where
    fmap f (CB m) = CB (fmap f m)

instance Monad (Callback t) where
    return       = CB . return
    (CB m) >>= n = CB (m >>= unCB . n)
    (CB m) >>  n = CB (m >>  unCB   n)
    fail         = CB . fail

instance Applicative (Callback t) where
    pure              = CB . pure
    (CB m) <*> (CB n) = CB (m <*> n)
    (CB m)  *> (CB n) = CB (m  *> n)
    (CB m) <*  (CB n) = CB (m <*  n)

instance MonadIO (Callback t) where
    liftIO = CB . liftIO

env :: Callback t CallbackEnv
env = CB ask

arbiterPtr :: Callback t ArbiterPtr
arbiterPtr = ceArbiter <$> env


-- | Shapes involved in this collision.
shapes :: Callback t (Shape, Shape)
shapes = do
  arb_ptr <- arbiterPtr
  spce    <- space
  liftIO $ do
    shA_ptr <- #{peek cpArbiter, a} arb_ptr
    shB_ptr <- #{peek cpArbiter, b} arb_ptr
    swapped <- #{peek cpArbiter, swappedColl} arb_ptr
    maybeshapeA  <- retrieveShape spce shA_ptr
    maybeshapeB  <- retrieveShape spce shB_ptr
    case (maybeshapeA, maybeshapeB) of
      (Just shapeA, Just shapeB) -> if swapped
                                       then return (shapeB, shapeA)
                                       else return (shapeA, shapeB)
      (_, _) -> fail "Physics.Hipmunk.Callbacks: expected shapes but got nothing!"

-- | Space from where these shapes come from.
space :: Callback t Space
space = ceSpace <$> env

-- | @True@ iff this is the first step that the shapes touched.
isFirstContact :: Callback t Bool
isFirstContact = do
  arb_ptr <- arbiterPtr
  liftIO $ do
    arbState <- #{peek cpArbiter, state} arb_ptr :: IO CInt
    return (arbState == #{const cpArbiterStateFirstColl})

-- | The normal vector of the collision.
normal :: NotSeparate t => Callback t Vector
normal = arbVecFunc wrArbiterGetNormal

arbVecFunc :: (ArbiterPtr -> VectorPtr -> IO ()) -> Callback t Vector
arbVecFunc func = do
  arb_ptr <- arbiterPtr
  liftIO $ alloca $ \v_ptr -> do
    func arb_ptr v_ptr
    peek v_ptr

foreign import ccall unsafe "wrapper.h"
    wrArbiterGetNormal :: ArbiterPtr -> VectorPtr -> IO ()

-- | Points where the collision occured.
points :: NotSeparate t => Callback t [Position]
points = do
  let go :: [Position] -> Int -> ContactPtr -> IO [Position]
      go acc 0 _ = return acc
      go acc i p = do v <- #{peek cpContact, p} p
                      go (v:acc) (i-1) (p `advancePtr` negate 1)
  ptr <- arbiterPtr
  numContacts  <- liftIO $ #{peek cpArbiter, numContacts} ptr
  contacts_ptr <- liftIO $ #{peek cpArbiter, contacts} ptr
  -- XXX: Why should numContacts ever get garbage?
  if numContacts <= 0 || numContacts > #{const CP_MAX_CONTACTS_PER_ARBITER}
    then return []
    else liftIO $ go [] numContacts (contacts_ptr `advancePtr` (numContacts-1))

-- | The total impulse that was applied to resolve the collision.
-- Returns incorrect results if elastic iterations are being used.
totalImpulse :: NotSeparate t => Callback (PostStep t) Vector
totalImpulse = arbVecFunc wrArbiterTotalImpulse

foreign import ccall unsafe "wrapper.h"
    wrArbiterTotalImpulse :: ArbiterPtr -> VectorPtr -> IO ()

-- | The total impulse with friction that was applied to resolve
-- the collision.  Returns incorrect results if elastic
-- iterations are being used.
totalImpulseWithFriction :: NotSeparate t => Callback (PostStep t) Vector
totalImpulseWithFriction = arbVecFunc wrArbiterTotalImpulseWithFriction

foreign import ccall unsafe "wrapper.h"
    wrArbiterTotalImpulseWithFriction :: ArbiterPtr -> VectorPtr -> IO ()

-- | Add an entity to the current 'Space' from where this
-- callback was called.  Don't add the same entity twice to a
-- space.
--
-- You can add entities only in 'PostStep' callbacks.  You should
-- not use @liftIO@ and @spaceAdd@.
currentSpaceAdd :: Entity a => a -> Callback (PostStep t) ()
currentSpaceAdd ent = do
  spce <- space
  liftIO (spaceAdd spce ent)

-- | Remove an entity from the current 'Space' from where this
-- callback was called.  Don't remove an entity that wasn't
-- added.
--
-- You can remove entities only in 'PostStep' callbacks.  You
-- should not use @liftIO@ and @spaceRemove@.
currentSpaceRemove :: Entity a => a -> Callback (PostStep t) ()
currentSpaceRemove ent = do
  spce <- space
  liftIO (spaceRemove spce ent)

-- | @postStep e cb@ registers a callback @cb@ for the 'PostStep'
-- phase on a given entity @e@.  @PostStep@ callbacks are called
-- once when the 'step' call finishes (and only on the current
-- time step).  This is the only kind of callbacks that may call
-- 'currentSpaceAdd' and 'currentSpaceRemove'.
--
-- Each entity may have /at most one/ callback registered on it.
-- If a second callback @cb2@ gets registered on the same entity
-- @e@, then callback @cb@ /will not/ be called, only @cb2@.
-- This is not a bug, but a feature.  This allows you to say, for
-- example, @postStep shape (currentSpaceRemove shape)@ every
-- time @shape@ collides.  Even if @shape@ collided many times in
-- a single time step, only the last callback would be called and
-- @shape@ would be removed just once.
--
-- Note that this function registers a callback from within
-- another callback, as this is the motivation of using
-- @PostStep@ callbacks.
postStep :: (Entity a, NotPostStep t) => a -> Callback (PostStep t) () -> Callback t ()
postStep e cb = do
  ce <- env
  liftIO $ unsafePostStep (ceSpace ce) e $ runReaderT (unCB cb) ce

-- | As 'postStep', registers a @PostStep@ callback.  Unlike
-- 'postStep', this function allows you to register a @PostStep@
-- callback from anywhere.  Also, from this callback you won't be
-- in 'Callback' monad.  It is therefore unsafe and should not be
-- used unless you really know what you are doing.
unsafePostStep :: Entity a => Space -> a -> IO () -> IO ()
unsafePostStep (P sp _ callbacks) e cb = do
  let f _ _ _ = cb -- discard arguments that we don't need
  cb_ptr <- makeChipmunkPostStepCB f
  withForeignPtr sp $ \sp_ptr ->
    withForeignPtr (entityPtr e) $ \e_ptr ->
      cpSpaceAddPostStepCallback sp_ptr cb_ptr (castPtr e_ptr) nullPtr
  let cb_ptr' = castFunPtr cb_ptr
  modifyIORef callbacks $ \cbs -> cbs {cbsPostStep = cb_ptr' : cbsPostStep cbs}

type ChipmunkPostStepCB = SpacePtr -> Ptr () -> Ptr () -> IO ()
type ChipmunkPostStepCBPtr = FunPtr ChipmunkPostStepCB
foreign import ccall "wrapper"
    makeChipmunkPostStepCB :: ChipmunkPostStepCB -> IO ChipmunkPostStepCBPtr
foreign import ccall unsafe "wrapper.h"
    cpSpaceAddPostStepCallback :: SpacePtr -> ChipmunkPostStepCBPtr -> Ptr () -> Ptr () -> IO ()




-- | A 4-tuple of callbacks, one for each kind of collision
-- event.
--
-- @beginHandler@ and @preSolveHandler@ should return a @Bool@
-- stating @True@ if the collision should be processed or @False@
-- if the collision should be ignored.  If @beginHandler@ returns
-- @False@, the collision will be completely ignored.  If
-- @preSolveHandler@ returns @False@, then the collision will be
-- ignored only for this time step.
--
-- You may also use @Nothing@ to use the default handlers.  The
-- default is to process all collisions.  That is, @Handler
-- Nothing Nothing Nothing Nothing@ is the same as
--
-- @
-- Handler {beginHandler     = Just (return True)
--         ,preSolveHandler  = Just (return True)
--         ,postSolveHandler = Just (return ())
--         ,separateHandler  = Just (return ())}
-- @
--
-- however using @Nothing@ is more efficient (the Chipmunk
-- library won't need to call a Haskell function).
--
-- Note that assigning @Nothing@ /does not/ mean that the default
-- set with 'setDefaultCollisionHandler' will be called.  That
-- default is called only if there isn't a registered handler for
-- the given collision types.
data CollisionHandler =
    Handler {beginHandler     :: Maybe (Callback Begin Bool)
            ,preSolveHandler  :: Maybe (Callback PreSolve Bool)
            ,postSolveHandler :: Maybe (Callback PostSolve ())
            ,separateHandler  :: Maybe (Callback Separate ())}

-- | Internal. Type of callback used by Chipmunk.
type ChipmunkCB a = ArbiterPtr -> SpacePtr -> Ptr () -> IO a
type ChipmunkCBPtr a = FunPtr (ChipmunkCB a)

-- | Internal. Create a new 'FunPtr' for a given callback.
adaptCallback :: MakeChipmunkCB a => Space
              -> Maybe (Callback t a) -> IO (ChipmunkCBPtr a)
adaptCallback _    Nothing  = return nullFunPtr
adaptCallback spce (Just c) = makeChipmunkCB f
    where
      f arb_ptr _ _ =
          let ce = CE {ceSpace   = spce
                      ,ceArbiter = arb_ptr}
          in runReaderT (unCB c) ce

-- | Internal. Transform callbacks of 'Bool's to 'CInt's.
asCInt :: Maybe (Callback t Bool) -> Maybe (Callback t CInt)
asCInt Nothing  = Nothing
asCInt (Just c) = Just (f <$> c) where f b = if b then 1 else 0

-- | Internal. Class of data types than can be given back as responses of
-- callbacks.
class MakeChipmunkCB a where
    makeChipmunkCB :: ChipmunkCB a -> IO (ChipmunkCBPtr a)

instance MakeChipmunkCB CInt where
    makeChipmunkCB = makeChipmunkCB_CInt
foreign import ccall "wrapper"
    makeChipmunkCB_CInt :: ChipmunkCB CInt -> IO (ChipmunkCBPtr CInt)

instance MakeChipmunkCB () where
    makeChipmunkCB = makeChipmunkCB_Void
foreign import ccall "wrapper"
    makeChipmunkCB_Void :: ChipmunkCB () -> IO (ChipmunkCBPtr ())

-- | Internal.  Create the 'FunPtr's and give them to C land.
addHandler :: Space -> (SpacePtr -> HandlerAdder)
           -> CollisionHandler -> IO HandlerFunPtrs
addHandler spce@(P sp _ _) handlerAdder handler = do
  beginCB     <- adaptCallback spce $ asCInt $ beginHandler handler
  preSolveCB  <- adaptCallback spce $ asCInt $ preSolveHandler handler
  postSolveCB <- adaptCallback spce $ postSolveHandler handler
  separateCB  <- adaptCallback spce $ separateHandler handler
  withForeignPtr sp $ \sp_ptr -> do
    handlerAdder sp_ptr beginCB preSolveCB postSolveCB separateCB nullPtr
  return (castFunPtr beginCB,     castFunPtr preSolveCB,
          castFunPtr postSolveCB, castFunPtr separateCB)

type HandlerAdder =  ChipmunkCBPtr CInt
                  -> ChipmunkCBPtr CInt
                  -> ChipmunkCBPtr ()
                  -> ChipmunkCBPtr ()
                  -> Ptr ()
                  -> IO ()

-- | Defines a new default collision handler.  This handler is
-- used whenever two shapes @a@ and @b@ collide such that no
-- other collision pair function was defined to @a@'s and @b@'s
-- collision types. The default is @Handler Nothing Nothing
-- Nothing Nothing@.
setDefaultCollisionHandler :: Space -> CollisionHandler -> IO ()
setDefaultCollisionHandler spce@(P _ _ callbacks) handler = do
  ptrs <- addHandler spce cpSpaceSetDefaultCollisionHandler handler
  cbs <- readIORef callbacks
  freeHandlerFunPtrs (cbsDefault cbs)
  writeIORef callbacks $ cbs {cbsDefault = ptrs}

foreign import ccall unsafe "wrapper.h"
    cpSpaceSetDefaultCollisionHandler :: SpacePtr -> HandlerAdder

-- | @addCollisionHandler sp (cta,ctb) handler@ defines @handler@
-- as the handler to be used whenever a collision occurs between
-- a shape of collision type @cta@ and another of collision type
-- @ctb@ (and vice versa).  Any other callback already registered
-- to handle @(cta,ctb)@ will be removed.
--
-- Note that you should /not/ add handlers to both combinations
-- of @(cta,ctb)@ and @(ctb,cta)@.  Doing so results in undefined
-- behaviour.  A good rule of thumb is to always use @cta <=
-- ctb@, although this is not necessary.
addCollisionHandler :: Space -> CollisionType -> CollisionType -> CollisionHandler -> IO ()
addCollisionHandler spce@(P _ _ callbacks) cta ctb handler = do
  -- Add the new handler, overriding anything that was already there.
  let handlerAdder p = cpSpaceAddCollisionHandler p cta ctb
  ptrs <- addHandler spce handlerAdder handler

  -- Free the previous one and record the new one.
  cbs <- readIORef callbacks
  let handlers = cbsHandlers cbs
      old = M.lookup (cta,ctb) handlers
      handlers' =
            if ptrs == (nullFunPtr, nullFunPtr, nullFunPtr, nullFunPtr)
                -- no need to gc nullFunPtrs
            then handlers
            else M.insert (cta,ctb) ptrs handlers
  maybe (return ()) freeHandlerFunPtrs old
  writeIORef callbacks $ cbs {cbsHandlers = handlers'}

foreign import ccall unsafe "wrapper.h"
    cpSpaceAddCollisionHandler :: SpacePtr -> CollisionType -> CollisionType -> HandlerAdder

-- | @removeCollisionHandler sp (cta,ctb)@ removes the handler
-- that was registered to handle @(cta,ctb)@, if any (see
-- 'addCollisionHandler').  Any collisions that would be handled
-- by the removed handler will be handled by the default one (see
-- 'setDefaultCollisionHandler').
--
-- Note that you should /always/ use the same order that was
-- passed to 'addCollisionHandler'. In other words, after
-- @addCollisionHandler sp (cta,ctb) handler@ you should use
-- @removeCollisionHandler sp (cta,ctb)@, and /never/
-- @removeCollisionHandler sp (ctb,cta)@ (note the swapped
-- tuple).
--
-- Although pointless, it is harmless to remove a callback that
-- was not added.
removeCollisionHandler :: Space -> CollisionType -> CollisionType -> IO ()
removeCollisionHandler (P sp _ callbacks) cta ctb = do
  cbs <- readIORef callbacks
  let handlers = cbsHandlers cbs
      (old,handlers') = M.updateLookupWithKey (\_ _ -> Nothing) (cta,ctb) handlers
  case old of
    Nothing   -> return () -- no need to free, no need to remove
    Just ptrs -> do freeHandlerFunPtrs ptrs
                    writeIORef callbacks $ cbs {cbsHandlers = handlers'}
                    withForeignPtr sp $ \sp_ptr -> do
                      cpSpaceRemoveCollisionHandler sp_ptr cta ctb

foreign import ccall unsafe "wrapper.h"
    cpSpaceRemoveCollisionHandler :: SpacePtr -> CollisionType -> CollisionType -> IO ()
