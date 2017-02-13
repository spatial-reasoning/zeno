{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators #-}
module Geometry where
import Control.Applicative
import Data.Foldable (fold, foldMap)
import Data.Vinyl
import Graphics.GLUtil hiding (generateMipmap')
import Graphics.Rendering.OpenGL hiding (normal, normalize, light, Normal, Color)
import Linear
import Graphics.VinylGL
import qualified System.FilePath as FP

import Shader

type Pos    = "vertexPos"    ::: V3 GLfloat
type Normal = "vertexNormal" ::: V3 GLfloat
type Color  = "vertexColor"  ::: V3 GLfloat

pos :: Pos
pos = Field

normal :: Normal
normal = Field

col :: Color
col = Field

-- The 2D corners of a square.
square :: [V2 GLfloat]
square = V2 <$> [-1,1] <*> [1,-1]

-- The 3D faces of a cube.
front,back,left,right,top,bottom :: [V3 GLfloat]
front  = map (\(V2 x y) -> V3 x y 1) square
back   = map (\(V2 x y) -> V3 (-x) y (-1)) square
left   = map (\(V2 z y) -> V3 (-1) y z) square
right  = map (\(V2 z y) -> V3 1 y (-z)) square
top    = map (\(V2 x z) -> V3 x 1 (-z)) square
bottom = map (\(V2 x z) -> V3 x (-1) z) square

-- Cube face vertices paired with normal vectors.
ptsCube :: [PlainRec [Pos,Normal]]
ptsCube = fold [ map (setNorm $ -z)    front
               , map (setNorm $ -z) back
               , map (setNorm $ -x) left
               , map (setNorm x)    right
               , map (setNorm y)    top
               , map (setNorm $ -y) bottom ]
  where [x,y,z] = basis
        setNorm v p = (pos =: p <+> normal =: v)

-- Cube face vertices paired with normal vectors.
ptsCube2 :: [PlainRec [Pos,Normal]]
ptsCube2 = fold
    [ map (setNorm $ -x) [V3 0 0 0, V3 0 0 1, V3 0 1 0]
    , map (setNorm $ -x) [V3 0 0 0, V3 0 0 1, V3 0 1 1]
    , map (setNorm $ -y) [V3 0 0 0, V3 1 0 0, V3 0 0 1]
    , map (setNorm $ -z) [V3 0 0 0, V3 0 1 0, V3 1 0 0]
    , map (setNorm $ normalize $ (x+y+z)) [V3 0 0 1, V3 1 0 0, V3 0 1 0]
    ]
  where [x,y,z] = basis
        setNorm v p = (pos =: p <+> normal =: v)

-- Tetrahedron face vertices paired with normal vectors.
pts :: [PlainRec [Pos,Normal]]
pts = fold [ map (setNorm $ -x) [V3 0 0 0, V3 0 0 1, V3 0 1 0]
           , map (setNorm $ -y) [V3 0 0 0, V3 1 0 0, V3 0 0 1]
           , map (setNorm $ -z) [V3 0 0 0, V3 0 1 0, V3 1 0 0]
           , map (setNorm $ normalize $ (x+y+z)) [V3 0 0 1, V3 1 0 0, V3 0 1 0]
           ]
  where [x,y,z] = basis
        setNorm v p = (pos =: p <+> normal =: v)

pts2 :: [PlainRec [Pos,Normal]]
pts2 = fold [ map (setNorm $ -x) [V3 3 0 0, V3 3 0 1, V3 3 1 0]
            , map (setNorm $ -y) [V3 3 0 0, V3 4 0 0, V3 3 0 1]
            , map (setNorm $ -z) [V3 3 0 0, V3 3 1 0, V3 4 0 0]
            , map (setNorm $ normalize $ (x+y+z)) [V3 3 0 1, V3 4 0 0, V3 3 1 0]
            ]
  where [x,y,z] = basis
        setNorm v p = (pos =: p <+> normal =: v)

-- Color the front vertices a dark blue, the back a light beige.
colorize :: PlainRec [Pos,Normal] -> PlainRec [Pos,Normal,Color]
--colorize pt = pt <+> col =: c
colorize pt = pt <+> col =: rGet pos pt
--  where
--    c | view (rLens pos._z) pt > 0 = V3 0 0 1
--    c | False = V3 0 0 1
--      | otherwise = V3 0 1 0

-- Indices into the vertex array for each face.
indsCube :: [Word32]
indsCube = take 36 $ foldMap (flip map faceInds . (+)) [0,4..]
  where faceInds = [0,1,2,2,1,3]

-- Indices into the vertex array for each face.
inds :: [Word32]
inds = [0..11]
--inds = [0..23]
--inds = [11,10..0]
--inds = [0,1,2,2,1,3,4,5,6,6,5,7]

-- For rendering a cube, we'll need a ModelView matrix, and a
-- ProjectionModelView matrix.
type CamInfo = PlainRec ["cam" ::: M44 GLfloat, "projcam" ::: M44 GLfloat]

cube :: (i <: CamInfo) => IO (i -> IO ())
cube = do
--    sh <- simpleShaderProgramBS polyVertexShaderBS polyFragmentShaderBS
--    sh <- simpleShaderProgramBS phongVertexShaderBS phongFragmentShaderBS
    sh <- simpleShaderProgram (FP.normalise "/home/andre/Dokumente/logoSpace/zeno/zeno2/etc/pixelShader.vert")
                              (FP.normalise "/home/andre/Dokumente/logoSpace/zeno/zeno2/etc/pixelShader.frag")
    vb <- bufferVertices (map colorize pts)
    eb <- makeBuffer ElementArrayBuffer inds
    vao <- makeVAO $ do
             currentProgram $= Just (program sh)
--             setUniforms sh (light =: normalize (V3 0 0 1))
             enableVertices' sh vb
             bindVertices vb
             bindBuffer ElementArrayBuffer $= Just eb
    let ssh = setUniforms sh
    return $ \appInfo -> withVAO vao $ do
        currentProgram $= Just (program sh)
        ssh (cast appInfo :: CamInfo)
        drawIndexedTris 4
--  where
--    light :: "lightDir" ::: V3 GLfloat
--    light = Field

-- We don't use normal vectors with the ground, so we just need a
-- single composite projection matrix.
type ProjInfo = PlainRec '["projcam" ::: M44 GLfloat]

-- Ground texture from:
-- http://www.texturehd.com/data/media/21/Wood_floor_boards.jpg
ground :: (i <: ProjInfo) => IO (i -> IO ())
ground = do
    Right t <- readTexture $ FP.normalise "/home/andre/Dokumente/logoSpace/zeno/zeno2/art/Wood_floor_boards.png"
    generateMipmap' Texture2D
--    sh <- simpleShaderProgramBS groundVertexShaderBS groundFragmentShaderBS
    sh <- simpleShaderProgram (FP.normalise "/home/andre/Dokumente/logoSpace/zeno/zeno2/etc/ground.vert")
                              (FP.normalise "/home/andre/Dokumente/logoSpace/zeno/zeno2/etc/ground.frag")
    vb <- bufferVertices . map ((pos =:) . scale3D) $
          V2 <$> [-1,1] <*> [-1,1]
    vao <- makeVAO $ do
               enableVertices' sh vb
               bindVertices vb
--               setUniforms sh (tex =: 0)
               textureBinding Texture2D $= Just t
               textureFilter Texture2D $= 
                   ((Linear', Just Linear'), Linear')
               texture2DWrap $= (Repeated, Repeat)
    return $ \appInfo -> withVAO vao $ do
        currentProgram $= Just (program sh)
        setUniforms sh (cast appInfo :: ProjInfo)
        withTextures2D [t] $ drawArrays TriangleStrip 0 4
  where
    scale3D :: V2 GLfloat -> V3 GLfloat
    scale3D = (\(V2 x z) -> V3 x (-2.01) z) . (3*^)
    tex :: "tex" ::: GLint
    tex = Field

