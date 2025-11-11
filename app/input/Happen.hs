module Happen (
    Keywit
  , unwrapHappenKeys
  , unwrapHappenMouse
  , unwrapHappenWheel
  , unwrapHappenWindow
  , waxwane
) where

import Data.Maybe (mapMaybe)

import SDL (
    Event (eventPayload)
  , EventPayload (KeyboardEvent, MouseMotionEvent, MouseWheelEvent, WindowResizedEvent)
  , Window, windowSize
  , Scancode
  , InputMotion
  , Keysym (keysymScancode)
  , KeyboardEventData (keyboardEventKeyMotion, keyboardEventKeysym)
  , MouseWheelEventData (mouseWheelEventPos)
  , MouseMotionEventData (mouseMotionEventRelMotion)
  )
import Graphics.Rendering.OpenGL (
    Capability (Enabled)
  , ComparisonFunction (Lequal)
  , HasSetter (($=))
  , Position (Position)
  , Size (Size)
  )

import qualified SDL (Point (P), V2 (V2))
import qualified Graphics.Rendering.OpenGL as GL (
    BlendingFactor (OneMinusSrcAlpha, SrcAlpha)
  , blend
  , blendFunc
  , depthFunc
  , get
  , viewport
  )

import Matrix (RenderView (..), fromSDL)
import Mean (doBoth)
import Rime (Point)


type Keywit = (Scancode, InputMotion)

unwrapHappenMouse :: [Event] -> [Point]
unwrapHappenMouse = mapMaybe (\event -> case eventPayload event of
  MouseMotionEvent e -> Just (fromSDL $ SDL.P $ mouseMotionEventRelMotion e)
  _ -> Nothing)

unwrapHappenWheel :: [Event] -> [Point]
unwrapHappenWheel = mapMaybe (\event -> case eventPayload event of
  MouseWheelEvent e -> Just (fromSDL $ SDL.P $ mouseWheelEventPos e)
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
  SDL.V2 width height <- (fromIntegral <$>) <$> GL.get (SDL.windowSize wind)
  GL.viewport $= (Position 0 0, Size width height)
  GL.depthFunc $= Just Lequal
  GL.blend $= Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  return RenderView {
      aspect = fromIntegral width / fromIntegral height
    , size = (fromIntegral width, fromIntegral height)
    , fov = pi / 4.0
    , near = 0.1
    , far = 100.0
  }
