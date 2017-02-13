module Picking where

import Data.Maybe

import Camera

-- See: http://schabby.de/picking-opengl-ray-tracing
pick :: Double -> Double -> Int -> Int -> Camera -> [Object] -> Just Object
pick posx posy width height cam objs = foldr
    (\ objWithDist -> maybe (Just object objWithDist) closest
    ) Nothing $ mapMaybe (intersects pos dir) objs
  where
    closest owd1 owd2 | distance owd1 < distance owd2 = owd1
                      | otherwise = owd2
    view = camFront cam
    h    = hLength *^ camRight cam
    v    = vLength *^ camUp    cam
    vLength = tan(fovy / 2) * 0.01
    hLength = vLength * (width / height)
    fovy = deg2rad 30
    -- translate mouse coordinates so that the origin lies in the center of the
    -- view port and scale mouse coordinates so that half the view port width
    -- and height becomes 1
    x = (posx - width / 2) / (height / 2)
    y = (posy - height / 2) / (width / 2)
    -- linear combination to compute intersection of picking ray with view port
    -- plane
    pos = cameraPos + view*nearClippingPlaneDistance + h*x + v*y
    -- compute direction of picking ray by subtracting intersection point with
    -- camera position
    dir = pos - camPos cam

