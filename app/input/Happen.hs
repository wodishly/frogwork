module Happen where

import Data.Maybe (mapMaybe)

import SDL (
  Event (eventPayload),
  EventPayload (KeyboardEvent),
  InputMotion,
  KeyboardEventData (keyboardEventKeyMotion, keyboardEventKeysym),
  Keysym (keysymScancode),
  Scancode
  )

import Mean (doBoth)


type Keywit = (Scancode, InputMotion)

unwrapKeys :: [Event] -> [Keywit]
unwrapKeys = mapMaybe (\event -> case eventPayload event of
  KeyboardEvent e -> Just (doBoth (keysymScancode.keyboardEventKeysym) keyboardEventKeyMotion e)
  _ -> Nothing)
