{-# LANGUAGE DataKinds, TypeOperators, FlexibleContexts #-}
import Control.Applicative
--import Control.Lens ((^.), contains)
import Control.Monad (when, forever, liftM)
import Data.IORef
import Data.Maybe
import qualified Data.Set as S
import Data.Set (Set)
import Data.Time.Clock
import qualified Data.Vector.Storable as V
import Data.Vinyl
import Graphics.GLUtil
import Graphics.GLUtil.Camera3D (deg2rad, projectionMatrix)
import Graphics.Rendering.OpenGL hiding (normalize, rotate, back)
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.GL.StateVar ( get )
import Graphics.Rendering.OpenGL.GL.StringQueries
import Graphics.UI.GLFW
import Graphics.VinylGL
import Linear
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import System.Exit ( exitWith, ExitCode(..) )

import Camera
import Geometry hiding (pos)

-- Note that the field name, "vertexCoord", matches the attribute name
-- in the vertex shader.
pos :: "vertexCoord" ::: v GLfloat
pos = Field

tex :: "tex" ::: GLint
tex = Field

logo :: IO (IO ())
logo = do Right t <- readTexture ("art"</>"Haskell-Logo.png")
          sh <- simpleShaderProgram ("etc"</>"logo.vert") ("etc"</>"logo.frag")
          vb <- bufferVertices $ map (pos =:) [0, V2 0.25 0, 0.25, V2 0 0.25]
          vao <- makeVAO $
                 do currentProgram $= Just (program sh)
                    enableVertices' sh vb
                    bindVertices vb
                    textureBinding Texture2D $= Just t
                    textureFilter Texture2D $= 
                      ((Nearest, Nothing), Nearest)
                    texture2DWrap $= (Mirrored, ClampToEdge)
                    setUniforms sh (tex =: 0)
          return . withVAO vao $ 
            do currentProgram $= Just (program sh)
               withTextures2D [t] (drawArrays TriangleFan 0 4)

main :: IO ()
main = do
    usage
    -- open GL window
    success <- Graphics.UI.GLFW.init
    when (not success) $ do
        terminate
        error "GLFW couldn't initialize."

    Just mon <- getPrimaryMonitor
    Just ( vm@VideoMode { videoModeWidth=monx, videoModeHeight=mony }
         ) <- getVideoMode mon
    let sizex = monx `div` 2
    let sizey = mony `div` 2
    let posx  = monx `div` 4
    let posy  = mony `div` 4
    windowHint $ WindowHint'ContextVersionMajor 3
    windowHint $ WindowHint'ContextVersionMinor 1
    windowHint $ WindowHint'OpenGLForwardCompat True
    Just win <- createWindow 640 480 "Zeno" Nothing Nothing
    setWindowSize win sizex sizey
    setWindowPos  win posx  posy
    makeContextCurrent (Just win)

    -- show details about GL version and machine
    showInfo
--    putStrLn . (("Extensions: ") ++) . concat =<< get gluExtensions

    -- register callbacks
    kbState <- newIORef S.empty
    mbState <- newIORef S.empty
    cpState <- newIORef Nothing
    wsState <- getWindowSize win >>= newIORef . uncurry V2
    camState <- newIORef newCamera
    setMouseButtonCallback win (Just $ mbCallback mbState)
    setScrollCallback win (Just $ scrollCallback camState)
    setCursorPosCallback win (Just $ cpCallback cpState camState)
    setWindowSizeCallback win (Just $ wsCallback wsState)
    setWindowCloseCallback win (Just wcCallback)

    -- GL settings
    clearColor $= Color4 0.2 0.2 0.2 1
    depthFunc $= Just Lequal
    cullFace $= Just GL.Back
--    cullFace $= Nothing

    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

    groundView <- ground
--    subView    <- logo
--    let vPos@(V2 px py) = V2 160 120
    let vPos@(V2 px py) = V2 0 0
        vp = withViewport (Position px py) . (\(V2 w h) -> Size w h)
             . (subtract vPos) . rGet (Field::Viewport)

    -- drawing stuff
    pixelShader <- simpleShaderProgram ("etc"</>"pixelShader.vert")
                                       ("etc"</>"pixelShader.frag")
    -- improve: initialize vb and eb earlier with empty params and fill them
    -- later.
    vb <- bufferVertices (map colorize pts)
    eb <- makeBuffer ElementArrayBuffer inds
    setKeyCallback win (Just $ keyCallback kbState vb)
    vao <- makeVAO $ do
--             setUniforms sh (light =: normalize (V3 0 0 1))
             enableVertices' pixelShader vb
             bindVertices vb
             bindBuffer ElementArrayBuffer $= Just eb

    lastTick <- getCurrentTime >>= newIORef
    forever $ do
        camera     <- readIORef camState
        windowSize <- readIORef wsState
--        let V2 ww wh = fromIntegral <$> (windowSize - V2 160 120)
        let V2 ww wh = fromIntegral <$> windowSize
            mProj = projectionMatrix (deg2rad 30) (ww / wh) 0.01 100
            mCam = lookAtMatrix camera
            info = ( Field =: mCam
                 <+> Field =: (mProj !*! mCam)
                 <+> Field =: (fromIntegral <$> windowSize) ) :: AppInfo
--        draw info
        clear [ColorBuffer, DepthBuffer]
--        subView
        vp info $ (groundView info >>) $ withVAO vao $ do
--        vp info $ withVAO vao $ do
--        (groundView info >>) $ withVAO vao $ do
            currentProgram $= Just (program pixelShader)
            setUniforms pixelShader (cast info :: CamInfo)
            drawIndexedTris 4
        swapBuffers win
        pollEvents
        t <- getCurrentTime
        timeStep <- realToFrac . diffUTCTime t <$> readIORef lastTick
        writeIORef lastTick t
        putStr $ "\r" ++ show (recip $ timeStep)
        hFlush stdout


usage :: IO ()
usage = putStrLn "Arrow keys to translate, shift+arrow to rotate, esc to exit!"

showInfo :: IO ()
showInfo = do
    let proclaim message var = putStrLn . ((message ++ ": ") ++) =<< get var
    proclaim "Vendor" vendor
    proclaim "Renderer" renderer
    proclaim "Version" glVersion
    proclaim "GLSL" shadingLanguageVersion


type Viewport = "viewport" ::: V2 GLsizei

type AppInfo = PlainRec [ "cam"     ::: M44 GLfloat
                        , "projcam" ::: M44 GLfloat
                        , Viewport ]

--setup :: IO (AppInfo -> IO ())
--setup = do
--    clearColor $= Color4 0.2 0.2 0.2 1
--    depthFunc $= Just Lequal
--    cullFace $= Just Back
--
--    blend $= Enabled
--    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
--
--    mainView <- ((sequence_ .) . sequence) <$> sequence [ground, cube]
--    subView <- logo
--    return $ \x -> subView >> vp x (mainView x)
--  where
--    vp = withViewport (Position px py)
--         . (\(V2 w h) -> Size w h) . (subtract vPos) . rGet (Field::Viewport)
--    vPos@(V2 px py) = V2 160 120

--loop :: IO UI -> IO ()
--loop tick = setup >>= go cam0
--  where 
--    go :: Camera GLfloat -> (AppInfo -> IO ()) -> IO ()
--    go c draw = do
--        ui <- tick
--        clear [ColorBuffer, DepthBuffer]
--        let V2 ww wh = fromIntegral <$> (windowSize ui - V2 160 120)
--            mProj = projectionMatrix (deg2rad 30) (ww / wh) 0.01 100
--            mCam = camMatrix c
--            info =  Field =: mCam
--                <+> Field =: (mProj !*! mCam)
--                <+> Field =: (fromIntegral <$> windowSize ui)
--        draw info
----        putStr $ "\r" ++ show (recip $ timeStep ui)
--        hFlush stdout
--        go c draw
----    cam0 = tilt 17 $ pan 225 $ dolly (V3 (-2) (-1) (-2)) fpsCamera
--    cam0 = dolly (V3 (1) (1) (5)) fpsCamera



keyCallback :: IORef (Set Key)
            -> BufferedVertices [Pos, Geometry.Normal, Geometry.Color]
            -> KeyCallback
keyCallback _ _ win Key'Escape _ KeyState'Pressed _ = wcCallback win
keyCallback _ vb win Key'P _ KeyState'Pressed _ = do
    _ <- fromVector ElementArrayBuffer $ V.fromList ([0..23] :: [Word32])
    reloadVertices vb $ V.fromList (map colorize $ pts ++ pts2)
--    return ()
keyCallback _ vb win Key'P _ KeyState'Released _ = do
    _ <- fromVector ElementArrayBuffer $ V.fromList ([0..11] :: [Word32])
    reloadVertices vb $ V.fromList (map colorize pts)
--    return ()
keyCallback keys _ _ k _ KeyState'Pressed   _ = modifyIORef' keys $ S.insert k
keyCallback keys _ _ k _ KeyState'Repeating _ = return ()
keyCallback keys _ _ k _ KeyState'Released  _ = modifyIORef' keys $ S.delete k

mbCallback :: IORef (Set MouseButton) -> MouseButtonCallback
mbCallback mbs _ b MouseButtonState'Pressed  _ = modifyIORef' mbs $ S.insert b
mbCallback mbs _ b MouseButtonState'Released _ = modifyIORef' mbs $ S.delete b

scrollCallback :: IORef Camera -> ScrollCallback
scrollCallback camState _ scrollX scrollY = atomicModifyIORef' camState
    (\ cam@Camera{ camRadius = r } ->
        ( moveCamera False False False cam
                     undefined undefined (V2 scrollX scrollY)
        , () )
    )

cpCallback :: IORef (Maybe (V2 Double)) -> IORef Camera -> CursorPosCallback
cpCallback cpState camState win newx newy = do
    lmb <- isPressed MouseButton'1
    rmb <- isPressed MouseButton'2
    mmb <- isPressed MouseButton'3
    oldCP <- atomicModifyIORef' cpState $ \ cp ->
        ( Just $ V2 newx newy, cp )
    when (isJust oldCP) $ atomicModifyIORef' camState $ \ cam ->
        ( moveCamera lmb mmb rmb cam (fromJust oldCP) (V2 newx newy) (V2 0 0)
        , () )
  where
    isPressed = liftM (== MouseButtonState'Pressed) . getMouseButton win


wsCallback :: IORef (V2 Int) -> WindowSizeCallback
wsCallback ws _ w h = writeIORef ws (V2 w h)

wcCallback :: WindowCloseCallback
wcCallback win = do
    destroyWindow win
    terminate
    _ <- exitWith ExitSuccess
    return ()


