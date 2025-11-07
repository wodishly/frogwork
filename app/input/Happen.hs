module Happen (
    Keywit
  , unwrapHappenKeys
  , unwrapHappenMouse
  , unwrapHappenWindow
  , waxwane
) where

import Data.Maybe (mapMaybe)

import SDL (
    Event (eventPayload)
  , EventPayload (KeyboardEvent, WindowResizedEvent, MouseMotionEvent)
  , InputMotion
  , KeyboardEventData (keyboardEventKeyMotion, keyboardEventKeysym)
  , Keysym (keysymScancode)
  , Scancode, Window, V2 (V2), windowSize, MouseMotionEventData (mouseMotionEventPos)
  )
import Graphics.Rendering.OpenGL (
    ComparisonFunction (Lequal)
  , GLfloat
  , HasSetter (($=))
  , Position (Position)
  , Size (Size)
  , Vertex2
  )

import qualified Graphics.Rendering.OpenGL as GL (depthFunc, get, viewport)

import Mean (doBoth)
import Matrix (RenderView (..))
import Rime (cast)
import Light (frogpoint)


type Keywit = (Scancode, InputMotion)

unwrapHappenMouse :: [Event] -> [Vertex2 GLfloat]
unwrapHappenMouse = mapMaybe (\event -> case eventPayload event of
  MouseMotionEvent e -> Just (frogpoint $ mouseMotionEventPos e)
  _ -> Nothing)

unwrapHappenKeys :: [Event] -> [Keywit]
unwrapHappenKeys = mapMaybe (\event -> case eventPayload event of
  KeyboardEvent e -> Just (doBoth (keysymScancode.keyboardEventKeysym) keyboardEventKeyMotion e)
  _ -> Nothing)

unwrapHappenWindow :: [Event] -> [Bool]
unwrapHappenWindow = mapMaybe (\event -> case eventPayload event of
  WindowResizedEvent _ -> Just True
  _ -> Nothing)

waxwane :: Window -> IO RenderView
waxwane wind = do
  V2 width height <- (cast <$>) <$> GL.get (SDL.windowSize wind)
  GL.viewport $= (Position 0 0, Size width height)
  GL.depthFunc $= Just Lequal
  return RenderView {
      _aspect = fromIntegral width / fromIntegral height
    , _fov = pi / 4.0
    , _near = 0.1
    , _far = 100.0
  }
