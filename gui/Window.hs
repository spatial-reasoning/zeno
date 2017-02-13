-- | Open a window and get an OpenGL context.
module Window (UI(..), initGL, terminate, closeWindow) where

import Control.Applicative
import Control.Monad
import Data.IORef
import Data.Set ( Set )
import qualified Data.Set as S
import Data.Time.Clock
import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.GL.StateVar ( get )
import Graphics.Rendering.OpenGL.GL.StringQueries
import Linear
import System.Exit ( exitWith, ExitCode(..) )


-- | Interface updates provided to the party responsible for
-- generating each frame.
data UI = UI { timeStep       :: Double 
             -- ^ Time in seconds since last frame
             , keysPressed    :: Set Key
             -- ^ All keys currently pressed
             , buttonsPressed :: Set MouseButton
             -- ^ All mouse buttons currently pressed
             , scrolled       :: V2 Double
             -- ^ Mouse scroll in x and y direction
             , mousePos       :: V2 Double
             -- ^ Current mouse position
             , windowSize     :: V2 Int
             -- ^ Current window size 
             }

showInfo :: IO ()
showInfo = do
   let proclaim message var = putStrLn . ((message ++ ": ") ++) =<< get var
   proclaim "Vendor" vendor
   proclaim "Renderer" renderer
   proclaim "Version" glVersion
   proclaim "GLSL" shadingLanguageVersion

keyCallback :: IORef (Set Key) -> KeyCallback
keyCallback keys _ k _ KeyState'Pressed   _ = modifyIORef' keys $ S.insert k
keyCallback keys _ k _ KeyState'Repeating _ = return ()
keyCallback keys _ k _ KeyState'Released  _ = modifyIORef' keys $ S.delete k

mbCallback :: IORef (Set MouseButton) -> MouseButtonCallback
mbCallback mbs _ b MouseButtonState'Pressed  _ = modifyIORef' mbs $ S.insert b
mbCallback mbs _ b MouseButtonState'Released _ = modifyIORef' mbs $ S.delete b

scrollCallback :: IORef (V2 Double) -> GLFW.ScrollCallback
scrollCallback sc _ scrollX scrollY = writeIORef' sc (V2 scrollX scrollY)

cpCallback :: IORef (V2 Double) -> CursorPosCallback
cpCallback cp _ x y = modifyIORef' cp (+ V2 x y)

wsCallback :: IORef (V2 Int) -> WindowSizeCallback
wsCallback ws _ w h = writeIORef ws (V2 w h)

wcCallback :: WindowCloseCallback
wcCallback win = do
    destroyWindow win
    terminate
    _ <- exitWith ExitSuccess
    return ()

-- | @initGL windowTitle width height@ creates a window with the given
-- title and dimensions. The action returned presents a new frame (by
-- performing a buffer swap) and produces an updated snapshot of the
-- user interface.
initGL :: String -> Int -> Int -> IO (IO UI)
initGL windowTitle width height = do
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
    windowHint $ WindowHint'OpenGLForwardCompat False
    Just win <- createWindow width height windowTitle Nothing Nothing
    setWindowSize win sizex sizey
    setWindowPos  win posx  posy
    makeContextCurrent (Just win)

    showInfo

    kbState <- newIORef S.empty
    mbState <- newIORef S.empty
    scState <- newIORef (V2 0 0)
    cpState <- getCursorPos win >>= newIORef . uncurry V2
    wsState <- getWindowSize win >>= newIORef . uncurry V2
    lastTick <- getCurrentTime >>= newIORef
    setKeyCallback win (Just $ keyCallback kbState)
    setMouseButtonCallback win (Just $ mbCallback mbState)
    setScrollCallback win (Just $ scrollCallback scState)
    setCursorPosCallback win (Just $ cpCallback cpState)
    setWindowSizeCallback win (Just $ wsCallback wsState)
    setWindowCloseCallback win (Just wcCallback)
    return $ do swapBuffers win
                pollEvents
                t <- getCurrentTime
                dt <- realToFrac . diffUTCTime t <$> readIORef lastTick
                writeIORef lastTick t
                UI dt <$> readIORef kbState
                      <*> readIORef mbState
                      <*> readIORef scState
                      <*> readIORef cpState
                      <*> readIORef wsState

