module Happen where

import qualified Graphics.Rendering.OpenGL as GL (blend, blendFunc, depthFunc, viewport)
import qualified SDL
  ( Event (eventPayload),
    EventPayload (KeyboardEvent, MouseMotionEvent, MouseWheelEvent, WindowResizedEvent),
    GLContext,
    InputMotion (..),
    KeyboardEventData (keyboardEventKeyMotion, keyboardEventKeysym),
    Keysym (keysymScancode),
    MouseMotionEventData (mouseMotionEventRelMotion),
    MouseWheelEventData (mouseWheelEventPos),
    Point (P),
    Scancode,
    V2 (V2),
    Window,
    glGetDrawableSize,
  )

import Mean
import Rime
import Matrix
import Shade
import Snailheart


data Otherworld = Otherworld {
  window :: SDL.Window,
  context :: SDL.GLContext,
  loudness :: Loudness
}

type Keywit = (SDL.Scancode, SDL.InputMotion)

data Mousewit = Mousewit
  { pointer :: Point,
    wheel :: Point
  }
  deriving (Eq, Show)

unwrapHappen :: (SDL.EventPayload -> Maybe a) -> [SDL.Event] -> [a]
unwrapHappen f = mapMaybe (f . SDL.eventPayload)

unwrapHappenPointer :: [SDL.Event] -> [Point]
unwrapHappenPointer = unwrapHappen $ \case
  SDL.MouseMotionEvent e -> Just (fromSDL $ SDL.P $ SDL.mouseMotionEventRelMotion e)
  _ -> Nothing

unwrapHappenWheel :: [SDL.Event] -> [Point]
unwrapHappenWheel = unwrapHappen $ \case
  SDL.MouseWheelEvent e -> Just (fromSDL $ SDL.P $ SDL.mouseWheelEventPos e)
  _ -> Nothing

unwrapHappenKeys :: [SDL.Event] -> [Keywit]
unwrapHappenKeys = unwrapHappen $ \case
    SDL.KeyboardEvent e -> Just (doBoth (SDL.keysymScancode . SDL.keyboardEventKeysym) SDL.keyboardEventKeyMotion e)
    _ -> Nothing

unwrapHappenWindow :: [SDL.Event] -> [Bool]
unwrapHappenWindow = unwrapHappen $ \case
  SDL.WindowResizedEvent _ -> Just True
  _ -> Nothing

waxwane :: SDL.Window -> IO RenderView
waxwane window = do
  SDL.V2 width height <- (fromIntegral <$>) <$> SDL.glGetDrawableSize window
  GL.viewport $= (Position 0 0, Size width height)
  GL.depthFunc $= Just Lequal
  GL.blend $= Enabled
  GL.blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  return RenderView {
    aspect = fromIntegral width / fromIntegral height,
    size = (fromIntegral width, fromIntegral height),
    fov = pi / 4.0,
    near = 0.1,
    far = 100.0
  }
