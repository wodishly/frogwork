module Happen (
    Keywit
  , Mousewit (..)
  , unwrapHappenKeys
  , unwrapHappenPointer
  , unwrapHappenWheel
  , unwrapHappenWindow
) where

import Data.Maybe (mapMaybe)

import SDL (
    Event (eventPayload)
  , EventPayload (KeyboardEvent, MouseMotionEvent, MouseWheelEvent, WindowResizedEvent)
  , Scancode
  , InputMotion
  , Keysym (keysymScancode)
  , KeyboardEventData (keyboardEventKeyMotion, keyboardEventKeysym)
  , MouseWheelEventData (mouseWheelEventPos)
  , MouseMotionEventData (mouseMotionEventRelMotion)
  )

import qualified SDL (Point (P))

import Mean (doBoth)
import Rime (Point, fromSDL)


type Keywit = (Scancode, InputMotion)

data Mousewit = Mousewit {
  pointer :: Point
, wheel :: Point
} deriving (Show, Eq)

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
