module Happen (
    Keywit
  , Mousewit
  , Overwindow
  , unwrapHappenKeys
  , unwrapHappenPointer
  , unwrapHappenWheel
  , unwrapHappenWindow
) where

import Data.Maybe (mapMaybe)

import SDL (
    GLContext
  , Event (eventPayload)
  , EventPayload (KeyboardEvent, MouseMotionEvent, MouseWheelEvent, WindowResizedEvent)
  , Scancode
  , InputMotion
  , Keysym (keysymScancode)
  , KeyboardEventData (keyboardEventKeyMotion, keyboardEventKeysym)
  , MouseWheelEventData (mouseWheelEventPos)
  , MouseMotionEventData (mouseMotionEventRelMotion)
  , Window
  )

import qualified SDL (Point (P))

import Matrix (fromSDL)
import Mean (doBoth)
import Rime (Point)


type Keywit = (Scancode, InputMotion)
type Mousewit = (Point, Point) -- pointer, wheel
type Overwindow = (Window, GLContext)

unwrapHappen :: (EventPayload -> Maybe a) -> [Event] -> [a]
unwrapHappen f = mapMaybe (f . eventPayload)

unwrapHappenPointer :: [Event] -> [Point]
unwrapHappenPointer = unwrapHappen (\case
  MouseMotionEvent e -> Just (fromSDL $ SDL.P $ mouseMotionEventRelMotion e)
  _ -> Nothing)

unwrapHappenWheel :: [Event] -> [Point]
unwrapHappenWheel = unwrapHappen (\case
  MouseWheelEvent e -> Just (fromSDL $ SDL.P $ mouseWheelEventPos e)
  _ -> Nothing)

unwrapHappenKeys :: [Event] -> [Keywit]
unwrapHappenKeys = unwrapHappen (\case
    KeyboardEvent e -> Just (doBoth (keysymScancode.keyboardEventKeysym) keyboardEventKeyMotion e)
    _ -> Nothing
  )

unwrapHappenWindow :: [Event] -> [Bool]
unwrapHappenWindow = unwrapHappen (\case
  WindowResizedEvent _ -> Just True
  _ -> Nothing)
