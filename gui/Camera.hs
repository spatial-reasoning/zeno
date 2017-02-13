{-# OPTIONS_GHC -Wall #-}

module Camera
    ( Camera(..)
    , newCamera
    , newCamera2D
--    , newCameraEuler
--    , newCameraEgo
--    , setCamera
    , eyePosition
    , lookAtMatrix
    , moveCamera
    )
where

import Linear

                         --         distance  x       y
data Camera = Camera2D { camCoords :: (Double, Double, Double) }
            | Camera   { camRadius :: Double
                       , camCenter :: V3 Double
                       , camFront  :: V3 Double
                       , camUp     :: V3 Double
                       , camRight  :: V3 Double
                       , camPos    :: V3 Double
                       } deriving (Eq, Read, Show)
--            | CameraEuler
--                  --                rho      theta      phi     rotation
--                { coordinates :: (GLdouble, GLdouble, GLdouble, GLdouble)
--                , origin      :: V3 GLdouble
--                }
--            | CameraEulerOnXYPlane
--                  --                rho      theta      phi
--                { coords :: (GLdouble, GLdouble, GLdouble)
--                , origin      :: V3 GLdouble
--                }
--            | CameraEgo { position  :: V3 GLdouble
--                        --                 Front         Right         Up
--                        , direction :: (V3 GLdouble, V3 GLdouble, V3 GLdouble)
--                        }

newCamera :: Camera
newCamera = newCamera3D

newCamera2D :: Camera
newCamera2D = Camera2D { camCoords = (7, 0, 0) }

newCamera3D :: Camera
newCamera3D = Camera { camRadius = 15
                     , camCenter = V3   0  0.5   0
                     , camFront  = V3 (-1)   0   0
                     , camUp     = V3   0    1   0
                     , camRight  = V3   0    0 (-1)
                     , camPos    = V3  15  0.5   0 }


eyePosition :: Camera -> V3 Double
eyePosition Camera{ camRadius = r, camCenter = c, camFront = f } = c - r *^ f

moveCamera :: Bool -> Bool -> Bool
           -> Camera
           -> V2 Double -> V2 Double ->V2 Double
           -> Camera
moveCamera lmb mmb rmb cam oldXY newXY (V2 scrollX scrollY) =
    cam { camRadius = nextRadius
        , camCenter = nextCenter
        , camFront  = nextFront
        , camUp     = nextUp
        , camRight  = nextRight
        , camPos    = nextPos
        }
  where
    Camera { camRadius = rho
           , camCenter = c, camFront = f, camUp = u, camRight = r } = cam
    V2 dx dy = (oldXY - newXY) / 100
    nextU = normalize $ rotate (axisAngle r dy) u
    nextR' = rotate (axisAngle nextU dx) r
    nextR = normalize $ nextR' - (dot nextR' nextU *^ nextU)
    nextF = cross nextU nextR
    [nextFront, nextUp, nextRight] = if lmb
        then map normalize [nextF, nextU, nextR]
        else [f, u, r]
    nextCenter = if rmb
        then c + 0.2 * rho *^ (r ^* dx - u ^* dy)
        else c
    nextPos = nextCenter - nextRadius *^ nextFront
    nextRadius = rho - 0.1 * scrollY * max rho 1


lookAtMatrix :: (Fractional a) => Camera -> M44 a
lookAtMatrix cam = fmap (fmap realToFrac) $
    -- improve: maybe we should split the rotation and translation matrices for
    -- better readability
    V4 (trans xaxis) (trans yaxis) (trans zaxis) (V4 0 0 0 1)
  where
    -- the axises have to be normalized
    zaxis = - camFront cam
    xaxis = camRight cam
--    yaxis = normalize $ cross up zaxis
    yaxis = camUp cam
--    yaxis = normalize $ cross zaxis xaxis
    trans axis@(V3 x y z) = V4 x y z (dot axis $ - camPos cam)


--setCamera :: Camera -> IO ()
--setCamera camera = lookAt (toVertex3 eyePosition) keepEyeOn upVector
--  where
--    eyePosition = center camera - radius camera *^ front camera
--    keepEyeOn   = toVertex3 $ eyePosition + front camera
--    upVector    = toVector3 $ up camera

-- todo: Define callbacks depending on camera type.

