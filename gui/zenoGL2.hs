{-
   Author :  Andre' van Delden 2004
   
   Other credit to ...

   This program does ...
-}

-- ghci -package wx -package OpenGL

module Main
where
    
import Data.List ( transpose )
import Graphics.UI.WX
import Graphics.UI.WXCore
import Graphics.Rendering.OpenGL
-- Many code and Type are ambiguous, so we must qualify names.
import qualified Graphics.UI.WX as WX
import qualified Graphics.Rendering.OpenGL as GL
import System.FilePath ((</>))



main :: IO()
main = start gui


defaultWidth  = 600
defaultHeight = 375

filename = "test.zen"

gui = do
   f <- frame [ text := "Zeno   –   " ++ filename
              , clientSize := sz 800 400
              , WX.position := pt 400 225
              ]

   p <- panel f []

   let zAbout = infoDialog f "About Zeno" aboutZeno

   -- create menu
   zMenu   <- menuPane [text := "&Zeno"]
   mOpen   <- menuItem zMenu [ text := "&Open"  , on command := zAbout ]
   menuLine zMenu
   mResume <- menuItem zMenu [ text := "&Resume", on command := zAbout ]
   mPause  <- menuItem zMenu [ text := "&Pause"]
   menuLine zMenu
   mQuit   <- menuQuit zMenu [help := "Quit"    , on command := close f]

   -- create Help menu
   zHelp  <- menuHelp []
   zAbout <- menuAbout zHelp [help := "About Zeno"]

   -- create toolbar
--   zToolbar <- toolBar f []
--   toolMenu zToolbar mResume "Resume" ("art"</>"resume.png") []
--   toolMenu zToolbar mPause  "Pause"  ("art"</>"pause.png" ) []

   -- create statusbar field:
   zStatus <- statusField [text := "Welcome to Zeno"]

   -- labels
--   zLabel <- staticText p [text := "Controls", fontWeight := WeightBold]

   -- start/cancel button
   btnResume <- button p
       [ text := "Resume"
       , on command := set f [text := "Zeno   –   " ++ filename]
       ]
   btnStop <- button p
       [ text := "Pause"
       , on command := set f [text :~ (++ "   –   Paused")]
       ]

   -- spin control
   sec <- spinCtrl p 0 59 [outerSize := sz 35 20]

   glCanvas <- glCanvasCreateEx f 0 (Rect 0 0 defaultWidth defaultHeight) 0 "GLCanvas" [GL_RGBA] nullPalette
   glContext <- glContextCreateFromNull glCanvas
   glCanvasSetCurrent glCanvas glContext
   let glWidgetLayout = fill $ widget glCanvas

   WX.set f [ statusBar := [zStatus]
            , menuBar   := [zMenu,zHelp]
            , on (menu zAbout) := infoDialog f "About Zeno" aboutZeno
            , layout := fill $ row 1
                [ vfill $ container p $ column 1 $
                    [ marginTop $ centre $ widget btnStop
                    , centre $ widget btnResume
                    , floatBottom $ widget sec
                    ]
                , glWidgetLayout
                ]
-- you have to use the paintRaw event. Otherwise the OpenGL window won't
-- show anything!
            , on paintRaw := paintGL glCanvas
            ]
   repaint f

convWG (WX.Size w h) = (GL.Size (convInt32  w) (convInt32  h))
convInt32 = fromInteger . toInteger

-- This paint function gets the current glCanvas for knowing where to draw in.
-- It is possible to have multiple GL windows in your application.
paintGL :: GLCanvas a -> DC() -> WX.Rect -> [WX.Rect]-> IO ()
paintGL glWindow dc myrect _ = do
   myInit
   reshape $ convWG $ rectSize myrect
   display
   glCanvasSwapBuffers glWindow
   return ()


ctrlPoints :: [[GL.Vertex3 GL.GLfloat]]
ctrlPoints = [
   [ GL.Vertex3 (-1.5) (-1.5)   4.0,  GL.Vertex3 (-0.5) (-1.5)   2.0,
     GL.Vertex3   0.5  (-1.5) (-1.0), GL.Vertex3   1.5  (-1.5)   2.0 ],
   [ GL.Vertex3 (-1.5) (-0.5)   1.0,  GL.Vertex3 (-0.5) (-0.5)   3.0,
     GL.Vertex3   0.5  (-0.5)   0.0,  GL.Vertex3   1.5  (-0.5) (-1.0) ],
   [ GL.Vertex3 (-1.5)   0.5    4.0,  GL.Vertex3 (-0.5)   0.5    0.0,
     GL.Vertex3   0.5    0.5    3.0,  GL.Vertex3   1.5    0.5    4.0 ],
   [ GL.Vertex3 (-1.5)   1.5  (-2.0), GL.Vertex3 (-0.5)   1.5  (-2.0),
     GL.Vertex3   0.5    1.5    0.0,  GL.Vertex3   1.5    1.5  (-1.0) ]]

initlights :: IO ()
initlights = do
   GL.lighting GL.$= GL.Enabled
   GL.light (GL.Light 0) GL.$= GL.Enabled

   GL.ambient  (GL.Light 0) GL.$= GL.Color4 0.2 0.2 0.2 1.0
   GL.position (GL.Light 0) GL.$= GL.Vertex4 0 0 2 1

   GL.materialDiffuse   GL.Front GL.$= GL.Color4 0.6 0.6 0.6 1.0
   GL.materialSpecular  GL.Front GL.$= GL.Color4 1.0 1.0 1.0 1.0
   GL.materialShininess GL.Front GL.$= 50

myInit :: IO ()
myInit = do
   GL.clearColor GL.$= GL.Color4 0.1 0.1 0.6 0
   GL.depthFunc GL.$= Just GL.Less
   m <- GL.newMap2 (0, 1) (0, 1) (transpose ctrlPoints)
   GL.map2 GL.$= Just (m :: GLmap2 GL.Vertex3 GL.GLfloat)
   GL.autoNormal GL.$= GL.Enabled
   mapGrid2 GL.$= ((20, (0, 1)), (20, (0, 1 :: GL.GLfloat)))
   initlights  -- for lighted version only

display = do
   GL.clear [ GL.ColorBuffer, GL.DepthBuffer ]
   GL.preservingMatrix $ do
     GL.rotate (85 :: GL.GLfloat) (GL.Vector3 1 1 1)
     evalMesh2 Fill (0, 20) (0, 20)
   GL.flush

reshape mysize@(GL.Size w h) = do
   GL.viewport GL.$= (GL.Position 0 0, mysize)
   GL.matrixMode GL.$= GL.Projection
   GL.loadIdentity
   let wf = fromIntegral w
       hf = fromIntegral h
   if w <= h
      then GL.ortho (-4.0) 4.0 (-4.0*hf/wf) (4.0*hf/wf) (-4.0) 4.0
      else GL.ortho (-4.0*wf/hf) (4.0*wf/hf) (-4.0) 4.0 (-4.0) 4.0
   GL.matrixMode GL.$= GL.Modelview 0
   GL.loadIdentity

aboutZeno :: String
aboutZeno = unlines $
    [ "Huha blab iaen iuaren iuatrne iuatren uiatreniua"
    , "Huha blab iaen iuaren iuatrne iuatren uiatreniua"
    ]

