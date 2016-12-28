module MBotPlus (module MBotPlus, module MBot, module HID) where

import Data.Bits
import MBot
import System.HIDAPI as HID

import Utils

data Led = LeftLed | RightLed deriving (Eq, Ord, Show)

data Direction = DirForward | DirLeft | DirRight | Brake | DirBackward
                    | DirBackwardLeft | DirBackwardRight deriving (Eq, Show)

data Motor = LeftMotor | RightMotor deriving (Eq, Ord, Show)

motorDirection :: Direction -> (Device -> IO())
motorDirection DirForward = goAhead
motorDirection DirLeft = goLeft
motorDirection DirRight = goRight
motorDirection DirBackward = goBackwards
motorDirection DirBackwardLeft = backwardsLeft
motorDirection DirBackwardRight = backwardsRight
motorDirection Brake = stop

-- right motor backwards, left motor zero -> moves left
backwardsLeft :: Device -> IO()
backwardsLeft = do
  sendCommand d $ setMotor (motorId LeftMotor) 0 0
  sendCommand d $ setMotor (motorId RightMotor) (complement 60) (complement 0)

-- left motor backwards, right motor zero -> moves right
backwardsRight :: Device -> IO()
backwardsRight = do
  sendCommand d $ setMotor (motorId LeftMotor) 60 0
  sendCommand d $ setMotor (motorId RightMotor) 0 0

-- gets the identifier of a led
ledId :: Led -> Int
ledId x = 1 + index [LeftLed, RightLed] x

-- gets the double of a line
lineDouble :: Line -> Double
lineDouble x = intDouble $ index [BOTHW, LEFTB, RIGHTB, BOTHB] x

-- gets the identifier of a motor
motorId :: Motor -> Int
motorId r = fromJust $ mapLookup [(LeftMotor, 0x9), (RightMotor, 0xa)] r
