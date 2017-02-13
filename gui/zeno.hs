{-# LANGUAGE DataKinds, DeriveDataTypeable, FlexibleContexts, RecordWildCards, TypeOperators #-}
{-
   Author :  Andre' van Delden 2015

   Other credit to ...

   This program does ...
-}

-- ghci -package wx -package OpenGL

module Main
where

import Control.Applicative
import Control.Monad (when, forever, liftM)
import Data.IORef
import Data.Maybe
import qualified Data.Set as S
import Data.Set (Set)
import Data.Time.Clock
import qualified Data.Vector.Storable as V
import Data.Vinyl
import Graphics.UI.WX hiding (when)
import qualified Graphics.UI.WX as WX
import Graphics.UI.WXCore hiding (when)
import Graphics.UI.WXCore.WxcTypes
import Graphics.GLUtil
import Graphics.GLUtil.Camera3D (deg2rad, projectionMatrix)
import Graphics.Rendering.OpenGL hiding (normalize, rotate, back, get, Point, Size)
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.GL.StringQueries
import qualified Graphics.UI.GLUT as GLUT
import Graphics.VinylGL
import Linear hiding (column)
import System.Console.CmdArgs hiding ((:=), help, program)
import qualified System.Console.CmdArgs as CA
import System.Environment (getArgs)
import qualified System.FilePath as FP
import System.IO (BufferMode(..), hSetBuffering, hFlush, FilePath, stdout)
import System.Exit ( exitWith, ExitCode(..) )

import Camera
import Geometry hiding (pos)


aboutZeno :: String
aboutZeno = unlines $ aboutZenoLines

aboutZenoLines :: [String]
aboutZenoLines =
    [ "Zeno does something"
    , "quite useful"
    ]


-- begin commandline option handling ------------------------------------------

data Options = Options { files :: [FilePath]
--                       , filename   :: FilePath
--                       , optScenario   :: Int
                       } deriving (Show, Data, Typeable)

defaultOptions = Options
    { files = def &= args
                  &= typFile
--    , filename = "testi.feng"
--        &= opt ("" :: FilePath)
--        &= explicit
--        &= name "f"
--        &= name "filename"
--        &= typ "FILENAME"
--        &= CA.help "Start with this Zeno file"
--    , optScenario = 0
--        &= opt (0 :: Int)
--        &= explicit
--        &= name "S"
--        &= name "scenario"
--        &= help "1 = Only generate atomic networks.     2 = Only generate scenarios.          Any other number = Generate general networks. (Default = 0)"
    } &= CA.help aboutZeno
      &= helpArg [ explicit, name "h", name "help"
                 , CA.help "Show this message."]
      &= versionArg [CA.help "Show version information."]
      &= CA.program "zeno"
      &= summary "Zeno version 14.05.27, (K) André van Delden"
      &= details aboutZenoLines
--      &= verbosity &=
--      &= versionArg [ignore] &=


-- end commandline option handling --------------------------------------------

main :: IO()
main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
--    opts <- (if null args then withArgs ["--help"] else id) $ cmdArgs defaultOptions
    opts <- cmdArgs defaultOptions
    start $ gui opts

type Viewport = "viewport" ::: V2 GLsizei

type ViewInfo = PlainRec [ "cam"     ::: M44 GLfloat
                         , "projcam" ::: M44 GLfloat
                         , Viewport ]

-- improve: move this somewhere else
screenSize :: Num a => IO (Int, Int)
screenSize = do
    GLUT.initialize "" []
    [width, height] <- (\ (GLUT.Size w h) -> fromIntegral <$> [w, h]) <$>
                       GLUT.get GLUT.screenSize
    GLUT.exit
    return (width, height)


gui opts@Options{..} = do
    (monWidth, monHeight) <- screenSize
    let initWinSize = sz (2 * div monWidth 3) (2 * div monHeight 3)
    let initWinPos  = pt (div monWidth 6) (div monHeight 6)
    -- fixme: define a data type that holds the basic application state, such
    -- as filename and stuff.
    let filename = if null files then "" else head files
    f <- frame [ text := "Zeno" ++ if null files
                                   then ""
                                   else ("   –   " ++ filename)
               , clientSize := initWinSize
               , WX.position := initWinPos
               , picture := (FP.normalise "/home/andre/Dokumente/logoSpace/zeno/zeno2/art/logo2.png")
               ]


--    glCanvas <- glCanvasCreateEx f 0 (Rect 0 0 100 100) 0 "GLCanvas" [GL_RGBA, GL_DOUBLEBUFFER] nullPalette
    glCanvas <- glCanvasCreateDefault f 0 "GLCanvas" [GL_RGBA, GL_DOUBLEBUFFER, GL_DEPTH_SIZE 1]
    glContext <- glContextCreateFromNull glCanvas
    glCanvasSetCurrent glCanvas glContext
    let glWidgetLayout = fill $ minsize (Size 100 100) $ widget glCanvas

    myInit
    csState  <- get glCanvas size >>= (\ (Size x y) -> newIORef $ V2 x y)
    camState <- newIORef newCamera
    cpState  <- newIORef Nothing
    groundView <- ground
--    subView    <- logo
--    let vPos@(V2 px py) = V2 160 120
    let vPos@(V2 px py) = V2 0 0
        vp = withViewport (Position px py) . (\(V2 w h) -> GL.Size w h)
             . (subtract vPos) . rGet (Field::Viewport)

    -- drawing stuff
    pixelShader <- simpleShaderProgram (FP.normalise "/home/andre/Dokumente/logoSpace/zeno/zeno2/etc/pixelShader.vert")
                                       (FP.normalise "/home/andre/Dokumente/logoSpace/zeno/zeno2/etc/pixelShader.frag")
    -- improve: initialize vb and eb earlier with empty params and fill them
    -- later.
    --vb <- bufferVertices (map colorize pts)
    --eb <- makeBuffer ElementArrayBuffer inds
    vb <- bufferVertices (map colorize ptsCube)
    eb <- makeBuffer ElementArrayBuffer indsCube
    vao <- makeVAO $ do
             enableVertices' pixelShader vb
             bindVertices vb
             bindBuffer ElementArrayBuffer $= Just eb

    numState <- newIORef 12 :: IO (IORef CInt)
    lastTick <- getCurrentTime >>= newIORef
    let draw = do
        camera     <- readIORef camState
        canvasSize <- readIORef csState
        let V2 ww wh = fromIntegral <$> canvasSize
            mProj = projectionMatrix (deg2rad 30) (ww / wh) 0.01 500
            mCam = lookAtMatrix camera
            info = ( Field =: mCam
                 <+> Field =: (mProj !*! mCam)
                 <+> Field =: (fromIntegral <$> canvasSize) ) :: ViewInfo
        clear [ColorBuffer, DepthBuffer]
        vp info $ (groundView info >>) $ withVAO vao $ do
            currentProgram $= Just (program pixelShader)
            setUniforms pixelShader (cast info :: CamInfo)
            num <- readIORef numState
            -- improve next: search or implement an opengl 3.1 draw function
            -- for various objects
            drawIndexedTris num
        glCanvasSwapBuffers glCanvas
--        return ()
--        flush
        currentTime <- getCurrentTime
        timeStep <- realToFrac . diffUTCTime currentTime <$> readIORef lastTick
        writeIORef lastTick currentTime
        putStr $ "\r" ++ show (recip $ timeStep)
        hFlush stdout

    WX.set glCanvas [ on mouse := mouseHandler draw cpState camState f ]

    p <- panel f [ on keyboard := keyHandler draw vb numState
--                 [ on (charKey 'p') := keyHandler draw vb numState
--                 , on (charKey 'p') := putStrLn "bla"
--                 , on anyKey := \ _ -> infoDialog f "About Zeno" aboutZeno
                 ]

    -- create toolbar
--    zToolbar <- toolBar f []
--    toolMenu zToolbar mResume "Resume" ("art/resume.png") []
--    toolMenu zToolbar mPause  "Pause"  ("art/pause.png" ) []

    -- create statusbar field:
    zStatus <- statusField [text := "Welcome to Zeno" ++
                            if null files then "" else ". Running..."]

    -- labels
--    zLabel <- staticText p [text := "Controls", fontWeight := WeightBold]

    -- spin control
    t <- timer f
        [interval := 46, on command := do
            atomicModifyIORef' camState $ \ cam ->
                ( moveCamera True False False cam
                     (fromIntegral <$> V2 0 0) (fromIntegral <$> V2 1 0)
                     (V2 0 0)
                , () )
            draw
        ]
    -- Pause/Resume button
    btnPauseResume <- button p [ text := "Pause" ]

    let togglePauseResume = do
            txt <- get btnPauseResume text
            if txt == "Pause" then togglePause else toggleResume
        togglePause = do
            set btnPauseResume [text := "Resume"]
            set zStatus        [text := "Paused" ]
            timerStop t
        toggleResume = do
            set btnPauseResume [text := "Pause" ]
            set zStatus        [text := "Running..."]
            val <- get t interval
            timerStart t val False >> return ()

    set btnPauseResume [on command := togglePauseResume]
    sec     <- spinCtrl p      0 47 [ outerSize := sz 35 20, selection := 2 ]
    zSlider <- hslider  p True 0 47 [ selection := 2 ]
--    g    <- hgauge  p 100 [selection := 50]
    set zSlider [ on command := get zSlider selection >>= \ val -> if val == 0
                    then do set sec [selection := val]
                            timerStop t >> set t [interval := 48]
                    else do set sec [selection := val]
                            notRun <- get btnPauseResume text
                            if notRun == "Pause"
                            then timerStart t (48 - val) False >> return ()
                            else set t [interval := 48 - val]
                ]
    set sec [ on select := get sec selection >>= \ val -> if val == 0
                    then do set zSlider [selection := val]
                            timerStop t >> set t [interval := 48]
                    else do set zSlider [selection := val]
                            notRun <- get btnPauseResume text
                            if notRun == "Pause"
                            then timerStart t (48 - val) False >> return ()
                            else set t [interval := 48 - val]
            ]
--    makeScrollable sec
--    makeScrollable zSlider

    -- create menu
    zMenu <- menuPane [text := "&Zeno"]
    mOpen <- menuItem zMenu
        [ text := "&Open"
        , on command := do
            filename <- fileOpenDialog f True True "Open Spatial Constraints"
                [ ("Zeno File"   , ["*.zen"])
                , ("Haskell File", ["*.hs" ])
                , ("Any file"    , ["*.*"  ])
                ] "" ""
            when (isJust filename) $ set f [ text := ("Zeno   –   " ++ fromJust filename) ]
        ]
    menuLine zMenu
    mResume <- menuItem zMenu
        [ text := "&Resume"
        , on command := toggleResume ]
    mPause  <- menuItem zMenu
        [ text := "&Pause"
        , on command := togglePause  ]
    menuLine zMenu
    mQuit   <- menuQuit zMenu [help := "Quit", on command := close f]
    -- help menu
    zHelp  <- menuHelp []
    zAbout <- menuAbout zHelp
        [ text := "About Zeno"
--        , help := "About Zeno"
        , on command := infoDialog f "About Zeno" aboutZeno ]

    set f [ menuBar   := [zMenu,zHelp]
          , statusBar := [zStatus]
          , layout := fill $ row 1
              [ vfill $ container p $ column 1 $
                  [ marginTop $ centre $ widget btnPauseResume
                  , vglue
                  , centre $ hfill $ widget zSlider
                  , centre $ widget sec
                  ]
              , glWidgetLayout
              ]
 -- you have to use the paintRaw event. Otherwise the OpenGL window won't
 -- show anything!
          , on keyboard := \ _ -> infoDialog f "Test" "TesttestTest"
          , on paintRaw := paintGL csState draw glCanvas
          , clientSize := initWinSize
          ]



-- This paint function gets the current glCanvas for knowing where to draw in.
-- It is possible to have multiple GL windows in your application.
paintGL :: IORef (V2 Int) -> IO () -> GLCanvas a -> DC() -> WX.Rect -> [WX.Rect] -> IO ()
paintGL ws draw glWindow _ _ _ = reshape ws glWindow >> draw


myInit :: IO ()
myInit = do
    -- GL settings
    clearColor $= Color4 0.2 0.2 0.2 1
    depthFunc $= Just Lequal
    cullFace $= Just GL.Back
--    cullFace $= Nothing

    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)


reshape ws glWindow = get glWindow size >>=
   (\ (WX.Size x y) -> writeIORef ws $ fmap fromIntegral $ V2 x y)

mouseHandler draw cpState camState f mouseEvent = do
  let Point x y = mousePos mouseEvent
  oldCP <- atomicModifyIORef' cpState $ \ cp ->
      ( Just $ fmap fromIntegral $ V2 x y, cp )
  case mouseEvent of
    MouseMotion _ _ -> atomicModifyIORef' cpState $ \ cp ->
        ( Just $ fmap fromIntegral $ V2 x y
        , () )
    MouseLeftDrag _ _ -> when (isJust oldCP) $ do
        atomicModifyIORef' camState $ \ cam ->
            ( moveCamera True False False cam
                         (fromJust oldCP) (fromIntegral <$> V2 x y) (V2 0 0)
            , () )
        draw
    MouseRightDrag _ _ -> when (isJust oldCP) $ do
        atomicModifyIORef' camState $ \ cam ->
            ( moveCamera False False True cam
                         (fromJust oldCP) (fromIntegral <$> V2 x y) (V2 0 0)
            , () )
        draw
    MouseWheel downward _ _ -> do
        let (scrollX, scrollY) = if downward
            then (0,-1) else (0,1) :: (Int, Int)
        atomicModifyIORef' camState $ \ cam ->
            ( moveCamera False False False cam undefined undefined
                         (fromIntegral <$> V2 scrollX scrollY)
            , () )
        draw
    otherwise -> return ()

keyHandler draw vb numState keyEvent = case keyKey keyEvent of
    KeyChar 'p' -> do
        replaceBuffer ElementArrayBuffer ([0..23] :: [Word32])
        reloadVertices vb $ V.fromList (map colorize $ pts ++ pts2)
        writeIORef numState 8
        draw
    KeyChar 'q' -> do
        replaceBuffer ElementArrayBuffer ([0..11] :: [Word32])
        reloadVertices vb $ V.fromList (map colorize pts)
        writeIORef numState 4
        draw
    otherwise -> return ()



makeScrollable widget = set widget
    [ on mouse := \ mouseEvent -> case mouseEvent of
        MouseWheel downward _ _ -> do
            let dir = if downward then -1 else 1
            val <- get widget selection
            set widget [ selection := val + dir ]
        otherwise -> return ()
    ]

onEnable w b = do
    set w [enabled :~ not]
    enable <- get w enabled
    set b [text := (if enable then "Pause" else "Resume")]

