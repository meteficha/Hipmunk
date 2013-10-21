-----------------------------------------------------------------------------
-- |
-- Module      :  Physics/Hipmunk/Shape.hsc
-- Copyright   :  (c) 2008-2010 Felipe A. Lessa
-- License     :  MIT (see LICENSE)
--
-- Maintainer  :  felipe.lessa@gmail.com
-- Stability   :  provisional
-- Portability :  portable (needs FFI)
--
-- Shapes used for collisions, their properties and some useful
-- polygon functions.
--
-----------------------------------------------------------------------------

module Physics.Hipmunk.Shape
    (-- * Shapes
     Shape,
     ShapeType(..),
     newShape,

     -- * Properties
     -- ** Collision type
     CollisionType,
     collisionType,
     -- ** Group
     Group,
     group,
     -- ** Layers
     Layers,
     layers,
     -- ** Elasticity
     Elasticity,
     elasticity,
     -- ** Friction
     Friction,
     friction,
     -- ** Surface velocity
     SurfaceVel,
     surfaceVel,

     -- * Utilities
     body,
     momentForShape,
     momentForCircle,
     momentForSegment,
     momentForPoly,
     shapePointQuery,
     shapeSegmentQuery,

     -- ** For polygons
     -- $polygon_util
     Segment,
     Intersection(..),
     epsilon,
     (.==.),
     isLeft,
     isClockwise,
     isConvex,
     intersects,
     polyReduce,
     polyCenter,
     convexHull
    )
    where

import Data.List (foldl', sortBy)
import Data.StateVar
import Foreign hiding (rotate, new)
import Foreign.C
#include "wrapper.h"

import Physics.Hipmunk.Common
import Physics.Hipmunk.Internal
import Physics.Hipmunk.Body (Mass, Moment)

-- | There are three types of shapes that can be attached
--   to bodies:
data ShapeType =
    -- | A circle is the fastest collision type. It also
    --   rolls smoothly.
    Circle {radius :: !Distance}

    -- | A line segment is meant to be used as a static
    --   shape. (It can be used with moving bodies, however
    --   two line segments never generate collisions between
    --   each other.)
  | LineSegment {start     :: !Position,
                 end       :: !Position,
                 thickness :: !Distance}

    -- | Polygons are the slowest of all shapes but
    --   the most flexible. The list of vertices must form
    --   a convex hull with clockwise winding.
    --   Note that if you want a non-convex polygon you may
    --   add several convex polygons to the body.
  | Polygon {vertices :: ![Position]}
    deriving (Eq, Ord, Show)


-- | @newShape b type off@ creates a new shape attached to
--   body @b@ at offset @off@. Note that you have to
--   add the shape to a space otherwise it won't generate
--   collisions.
newShape :: Body -> ShapeType -> Position -> IO Shape
newShape body_@(B b) (Circle r) off =
  withForeignPtr b $ \b_ptr ->
  with off $ \off_ptr ->
  mallocForeignPtrBytes #{size cpCircleShape} >>= \shape ->
  withForeignPtr shape $ \shape_ptr -> do
    wrCircleShapeInit shape_ptr b_ptr off_ptr r
    return (S shape body_)

newShape body_@(B b) (LineSegment p1 p2 r) off =
  withForeignPtr b $ \b_ptr ->
  with (p1+off) $ \p1off_ptr ->
  with (p2+off) $ \p2off_ptr ->
  mallocForeignPtrBytes #{size cpSegmentShape} >>= \shape ->
  withForeignPtr shape $ \shape_ptr -> do
    wrSegmentShapeInit shape_ptr b_ptr p1off_ptr p2off_ptr r
    return (S shape body_)

newShape body_@(B b) (Polygon verts) off =
  withForeignPtr b $ \b_ptr ->
  with off $ \off_ptr ->
  withArrayLen verts $ \verts_len verts_ptr ->
  mallocForeignPtrBytes #{size cpPolyShape} >>= \shape ->
  withForeignPtr shape $ \shape_ptr -> do
    let verts_len' = fromIntegral verts_len
    wrPolyShapeInit shape_ptr b_ptr verts_len' verts_ptr off_ptr
    addForeignPtrFinalizer cpShapeDestroy shape
    return (S shape body_)

foreign import ccall unsafe "wrapper.h"
    wrCircleShapeInit :: ShapePtr -> BodyPtr -> VectorPtr
                      -> CpFloat -> IO ()
foreign import ccall unsafe "wrapper.h"
    wrSegmentShapeInit :: ShapePtr -> BodyPtr -> VectorPtr
                       -> VectorPtr -> CpFloat -> IO ()
foreign import ccall unsafe "wrapper.h"
    wrPolyShapeInit :: ShapePtr -> BodyPtr -> CInt -> VectorPtr
                    -> VectorPtr -> IO ()
foreign import ccall unsafe "wrapper.h &cpShapeDestroy"
    cpShapeDestroy :: FunPtr (ShapePtr -> IO ())


-- | @body s@ is the body that this shape is associated
--   to. Useful especially in a space callback.
body :: Shape -> Body
body (S _ b) = b


-- | The collision type is used to determine which collision
--   callback will be called. Its actual value doesn't have a
--   meaning for Chipmunk other than the correspondence between
--   shapes and the collision pair functions you add. (default is
--   zero)

type CollisionType = #{type cpCollisionType}
collisionType :: Shape -> StateVar CollisionType
collisionType (S shape _) = makeStateVar getter setter
    where
      getter = withForeignPtr shape #{peek cpShape, collision_type}
      setter = withForeignPtr shape . flip #{poke cpShape, collision_type}

-- | Groups are used to filter collisions between shapes. If
--   the group is zero, then it imposes no restriction
--   to the collisions. However, if the group is non-zero then
--   the shape will not collide with other shapes in the same
--   non-zero group. (default is zero)
--
--   This is primarely used to create multi-body, multi-shape
--   objects such as ragdolls. It may be thought as a lightweight
--   alternative to creating a callback that filters the
--   collisions.
type Group = #{type cpGroup}
group :: Shape -> StateVar Group
group (S shape _) = makeStateVar getter setter
    where
      getter = withForeignPtr shape #{peek cpShape, group}
      setter = withForeignPtr shape . flip #{poke cpShape, group}

-- | Layers are similar to groups, but use a bitmask. For a collision
--   to occur, two shapes must have at least one layer in common.
--   In other words, @layer1 .&. layer2@ should be non-zero.
--   (default is @-1@, meaning all bits set)
--
--   Note that although this type may have more than 32 bits,
--   for portability you should only rely on the lower 32 bits.
type Layers = #{type cpLayers}
layers :: Shape -> StateVar Layers
layers (S shape _) = makeStateVar getter setter
    where
      getter = withForeignPtr shape #{peek cpShape, layers}
      setter = withForeignPtr shape . flip #{poke cpShape, layers}

-- | The elasticity of the shape is such that @0.0@ gives no bounce
--   while @1.0@ give a \"perfect\" bounce. Note that due to
--   inaccuracies using @1.0@ or greater is not recommended.
--
--   The amount of elasticity applied during a collision is
--   calculated by multiplying the elasticity of both shapes.
--   (default is zero)
--
--   By default old-style elastic iterations are done when the
--   space @step@s.  This used to result in a not-so-good
--   simulation, but now this is the recommended setting.
type Elasticity = CpFloat
elasticity :: Shape -> StateVar Elasticity
elasticity (S shape _) = makeStateVar getter setter
    where
      getter = withForeignPtr shape #{peek cpShape, e}
      setter = withForeignPtr shape . flip #{poke cpShape, e}

-- | The friction coefficient of the shape according
--   to Coulumb friction model (i.e. @0.0@ is frictionless,
--   iron on iron is around @1.0@, and it could be greater
--   then @1.0@).
--
--   The amount of friction applied during a collision is
--   determined by multiplying the friction coefficient
--   of both shapes. (default is zero)
type Friction = CpFloat
friction :: Shape -> StateVar Friction
friction (S shape _) = makeStateVar getter setter
    where
      getter = withForeignPtr shape #{peek cpShape, u}
      setter = withForeignPtr shape . flip #{poke cpShape, u}

-- | The surface velocity of the shape. Useful to create
--   conveyor belts and players that move around. This
--   value is only used when calculating friction, not
--   collision. (default is zero)
type SurfaceVel = Vector
surfaceVel :: Shape -> StateVar SurfaceVel
surfaceVel (S shape _) = makeStateVar getter setter
    where
      getter = withForeignPtr shape #{peek cpShape, surface_v}
      setter = withForeignPtr shape . flip #{poke cpShape, surface_v}





-- | @momentForShape m s off@ is a convenience function that calculates
--   the moment of inertia for shape @s@ with mass @m@ and at a
--   offset @off@ of the body's center.  Uses 'momentForCircle',
--   'momentForSegment' and 'momentForPoly' internally.
momentForShape :: Mass -> ShapeType -> Position -> Moment
momentForShape m (Circle r)            off = m*(r*r + (off `dot` off))
momentForShape m (LineSegment p1 p2 _) off = momentForSegment m (p1+off) (p2+off)
momentForShape m (Polygon verts)       off = momentForPoly m verts off

-- | @momentForCircle m (ri,ro) off@ is the moment of inertia
--   of a circle of @m@ mass, inner radius of @ri@, outer radius
--   of @ro@ and at an offset @off@ from the center of the body.
momentForCircle :: Mass -> (Distance, Distance) -> Position -> Moment
momentForCircle m (ri,ro) off = (m/2)*(ri*ri + ro*ro) + m*(off `dot` off)
-- We recoded the C function to avoid FFI and unsafePerformIO
-- on this simple function.


-- | @momentForSegment m p1 p2@ is the moment of inertia of a
--   segment of mass @m@ going from point @p1@ to point @p2@.
momentForSegment :: Mass -> Position -> Position -> Moment
momentForSegment m p1 p2 =
    let len' = len (p2 - p1)
        offset = scale (p1 + p2) (recip 2)
    in m * len' * len' / 12  +  m * offset `dot` offset
-- We recoded the C function to avoid FFI and unsafePerformIO
-- on this simple function.


-- | @momentForPoly m verts off@ is the moment of inertia of a
--   polygon of @m@ mass, at offset @off@ from the center of
--   the body and comprised of @verts@ vertices. This is similar
--   to 'Polygon' (and the same restrictions for the vertices
--   apply as well).
momentForPoly :: Mass -> [Position] -> Position -> Moment
momentForPoly m verts off = (m*sum1)/(6*sum2)
  where
    verts' = if off /= 0 then map (+off) verts else verts
    (sum1,sum2) = calc (pairs (,) verts') 0 0

    calc a b c | a `seq` b `seq` c `seq` False = undefined
    calc []           acc1 acc2 = (acc1, acc2)
    calc ((v1,v2):vs) acc1 acc2 =
      let a = v2 `cross` v1
          b = v1 `dot` v1 + v1 `dot` v2 + v2 `dot` v2
      in calc vs (acc1 + a*b) (acc2 + a)
-- We recoded the C function to avoid FFI, unsafePerformIO
-- and a bunch of malloc + poke. Is it worth?

-- | Internal. For @l = [x1,x2,...,xn]@, @pairs f l@ is
--   @[f x1 x2, f x2 x3, ...,f xn x1]@.
pairs :: (a -> a -> b) -> [a] -> [b]
pairs f l = zipWith f l (tail $ cycle l)

-- | @shapePointQuery shape p@ returns @True@ iff the point
--   in position @p@ (in world's coordinates) lies within the
--   shape @shape@.
shapePointQuery :: Shape -> Position -> IO Bool
shapePointQuery (S shape _) p =
  withForeignPtr shape $ \shape_ptr ->
  with p $ \p_ptr -> do
    i <- wrShapePointQuery shape_ptr p_ptr
    return (i /= 0)

foreign import ccall unsafe "wrapper.h"
    wrShapePointQuery :: ShapePtr -> VectorPtr -> IO CInt

-- | @shapeSegmentQuery shape p1 p2@ returns @Just (t,n)@ iff the
--   segment from @p1@ to @p2@ (in world's coordinates)
--   intersects with the shape @shape@.  In that case, @0 <= t <=
--   1@ indicates that one of the intersections is at point @p1 +
--   (p2 - p1) \`scale\` t@ with normal @n@.
shapeSegmentQuery :: Shape -> Position -> Position
                  -> IO (Maybe (CpFloat, Vector))
shapeSegmentQuery (S shape _) p1 p2 =
    withForeignPtr shape $ \shape_ptr ->
    with p1 $ \p1_ptr ->
    with p2 $ \p2_ptr ->
    allocaBytes #{size cpSegmentQueryInfo} $ \info_ptr -> do
      i <- wrShapeSegmentQuery shape_ptr p1_ptr p2_ptr info_ptr
      if (i == 0) then return Nothing else do
        t <- #{peek cpSegmentQueryInfo, t} info_ptr
        n <- #{peek cpSegmentQueryInfo, n} info_ptr
        return $ Just (t, n)

foreign import ccall unsafe "wrapper.h"
    wrShapeSegmentQuery :: ShapePtr -> VectorPtr -> VectorPtr
                        -> Ptr () -> IO CInt



-- $polygon_util
--   This section is inspired by @pymunk.util@,
--   a Python module made from <http://code.google.com/p/pymunk/>,
--   although implementations are quite different.
--
--   Also, unless noted otherwise all polygons are
--   assumed to be simple (i.e. no overlapping edges).

-- | The epsilon used in the algorithms below when necessary
--   to compare floats for \"equality\".
epsilon :: CpFloat
epsilon = 1e-25

-- | \"Equality\" under 'epsilon'. That is, @a .==. b@
--   if @abs (a - b) <= epsilon@.
(.==.) :: CpFloat -> CpFloat -> Bool
a .==. b = abs (a - b) <= epsilon

-- | A line segment.
type Segment = (Position, Position)

-- | /O(n)/. @isClockwise verts@ is @True@ iff @verts@ form
--   a clockwise polygon.
isClockwise :: [Position] -> Bool
isClockwise = (<= 0) . foldl' (+) 0 . pairs cross

-- | @isLeft (p1,p2) vert@ is
--
--    * @LT@ if @vert@ is at the left of the line defined by @(p1,p2)@.
--
--    * @EQ@ if @vert@ is at the line @(p1,p2)@.
--
--    * @GT@ otherwise.
isLeft :: (Position, Position) -> Position -> Ordering
isLeft (p1,p2) vert = compare 0 $ (p1 - vert) `cross` (p2 - vert)

-- | /O(n)/. @isConvex verts@ is @True@ iff @vers@ form a convex
--   polygon.
isConvex :: [Position] -> Bool
isConvex = foldl1 (==) . map (0 <) . filter (0 /=) . pairs cross . pairs (-)
-- From http://apocalisp.wordpress.com/category/programming/haskell/page/2/

-- | /O(1)/. @intersects seg1 seg2@ is the intersection between
--   the two segments @seg1@ and @seg2@. See 'Intersection'.
intersects :: Segment -> Segment -> Intersection
intersects (a0,a1) (b0,b1) =
    let u                = a1 - a0
        v@(Vector vx vy) = b1 - b0
        w@(Vector wx wy) = a0 - b0
        d = u `cross` v
        parallel = d .==. 0

        -- Parallel case
        collinear = all (.==. 0) [u `cross` w, v `cross` w]
        a_is_point = u `dot` u .==. 0
        b_is_point = v `dot` v .==. 0
        (Vector w2x w2y) = a1 - b0
        (a_in_b, a_in_b') = if vx .==. 0
                             then swap (wy/vy, w2y/vy)
                             else swap (wx/vx, w2x/vx)
            where swap t@(x,y) | x < y     = t
                               | otherwise = (y,x)

        -- Non-parallel case
        sI = v `cross` w / d
        tI = u `cross` w / d

        -- Auxiliary functions
        inSegment p (c0,c1)
            | vertical  = test (gy p) (gy c0, gy c1)
            | otherwise = test (gx p) (gx c0, gx c1)
            where
              vertical = gx c0 .==. gx c1
              (gx, gy) = (\(Vector x _) -> x, \(Vector _ y) -> y)
              test q (d0,d1) = any (inside q) [(d0,d1), (d1,d0)]
        inside n (l,r) = l <= n && n <= r

    in if parallel
       then case (collinear, a_is_point, b_is_point) of
             (False, _, _) ->
                 -- Parallel and non-collinear
                 IntNowhere

             (_, False, False) ->
                 -- Both are parallel, collinear segments
                 case (a_in_b > 1 || a_in_b' < 0,
                       max a_in_b 0, min a_in_b' 1) of
                   (True, _, _) -> IntNowhere
                   (_, i0, i1)
                       | i0 .==. i1 -> IntPoint p0
                       | otherwise  -> IntSegmt (p0,p1)
                       where p0 = b0 + v `scale` i0
                             p1 = b0 + v `scale` i1

             (_, True, True) ->
                 -- Both are points
                 if len (b0-a0) .==. 0
                 then IntPoint a0 else IntNowhere

             _ ->
                 -- One is a point, another is a segment
                 let (point,segment)
                         | a_is_point = (a0, (b0,b1))
                         | otherwise  = (b0, (a0,a1))
                 in if inSegment point segment
                    then IntPoint point else IntNowhere

       else if all (\x -> inside x (0,1)) [sI, tI]
            then IntPoint (a0 + u `scale` sI) else IntNowhere

-- | A possible intersection between two segments.
data Intersection = IntNowhere         -- ^ Don't intercept.
                  | IntPoint !Position -- ^ Intercept in a point.
                  | IntSegmt !Segment  -- ^ Share a segment.
                    deriving (Eq, Ord, Show)


-- | /O(n)/. @polyReduce delta verts@ removes from @verts@ all
--   points that have less than @delta@ distance
--   in relation to the one preceding it.
--
--   Note that a very small polygon may be completely \"eaten\"
--   if all its vertices are within a @delta@ radius from the
--   first.
polyReduce :: Distance -> [Position] -> [Position]
polyReduce delta = go
    where
      go (p1:p2:ps) | len (p2-p1) < delta = go (p1:ps)
                    | otherwise           = p1 : go (p2:ps)
      go other = other

-- | /O(n)/. @polyCenter verts@ is the position in the center
--   of the polygon formed by @verts@.
polyCenter :: [Position] -> Position
polyCenter verts = foldl' (+) 0 verts `scale` s
    where s = recip $ toEnum $ length verts


-- | /O(n log n)/. @convexHull verts@ is the convex hull of the
--   polygon defined by @verts@. The vertices of the convex
--   hulls are given in clockwise winding. The polygon
--   doesn't have to be simple.
--
--   Implemented using Graham scan, see
--   <http://cgm.cs.mcgill.ca/~beezer/cs507/3coins.html>.
convexHull :: [Position] -> [Position]
convexHull verts =
  let (p0,ps) = takeMinimum verts
      (_:p1:points) = p0 : sortBy (isLeft . (,) p0) ps

      -- points is going counterclockwise now.
      -- In go we use 'hull' with the last added
      -- vertex as the head, so our result is clockwise.

      -- Remove right turns
      go hull@(h1:h2:hs) (q1:qs) =
          case (isLeft (h2,h1) q1, hs) of
            (LT,_) -> go (q1:hull) qs    -- Left turn
            (_,[]) -> go (q1:hull) qs    -- Maintain at least 2 points
            _      -> go (h2:hs) (q1:qs) -- Right turn or straight
      go hull [] = hull
      go _ _     = error "Physics.Hipmunk.Shape.convexHull: never get here"

  in go [p1,p0] points


-- | Internal. Works like minimum but also returns the
--   list without it. The order of the list may be changed.
--   We have @fst (takeMinimum xs) == minimum xs@ and
--   @sort (uncurry (:) $ takeMinimum xs) == sort xs@
takeMinimum :: Ord a => [a] -> (a, [a])
takeMinimum [] = error "Physics.Hipmunk.Shape.takeMinimum: empty list"
takeMinimum (x:xs) = go x [] xs
    where
      go min_ acc (y:ys) | y < min_  = go y (min_:acc) ys
                         | otherwise = go min_ (y:acc) ys
      go min_ acc [] = (min_, acc)

