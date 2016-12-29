module MBotPlus () where

import Data.Bits
import Data.Maybe(fromJust)
import MBot
import System.HIDAPI as HID(Device)

import Utils

data Direction = DirForward | DirLeft | DirRight | Brake | DirBackward
                    | DirBackwardLeft | DirBackwardRight deriving (Eq, Show)

data Led = LeftLed | RightLed deriving (Eq, Ord, Show)

data Motor = LeftMotor | RightMotor deriving (Eq, Ord, Show)

-- right motor backwards, left motor zero -> moves left
backwardsLeft :: Device -> IO()
backwardsLeft d = do
  sendCommand d $ setMotor (motorId LeftMotor) 0 0
  sendCommand d $ setMotor (motorId RightMotor) (complement 80) (complement 0)

-- left motor backwards, right motor zero -> moves right
backwardsRight :: Device -> IO()
backwardsRight d = do
  sendCommand d $ setMotor (motorId LeftMotor) 80 0
  sendCommand d $ setMotor (motorId RightMotor) 0 0

-- closes connection
close :: Device -> IO ()
close = closeMBot

-- opens a connection to the mbot
connect :: (IO Device)
connect = openMBot

command :: Device -> Command -> IO ()
command = sendCommand

-- gets the identifier of a led
ledId :: Led -> Int
ledId x = 1 + index [LeftLed, RightLed] x

-- sets a led
led :: Device -> Led -> Double -> Double -> Double -> IO ()
led d l r g b = command d $ setRGB (ledId l) ri gi bi where
  (ri, gi, bi) = (doubleInt r, doubleInt g, doubleInt b)

-- gets the double of a line
lineDouble :: Line -> Double
lineDouble x = intDouble $ index [BOTHW, LEFTB, RIGHTB, BOTHB] x

-- gets the line sensor value
lineSensor :: Device -> IO Double
lineSensor d = do { val <- readLineFollower d; return $ lineDouble val }

-- gets the function for direction movement
motorDirection :: Direction -> (Device -> IO())
motorDirection DirForward = goAhead
motorDirection DirLeft = goLeft
motorDirection DirRight = goRight
motorDirection DirBackward = goBackwards
motorDirection DirBackwardLeft = backwardsLeft
motorDirection DirBackwardRight = backwardsRight
motorDirection Brake = stop

-- gets the identifier of a motor
motorId :: Motor -> Int
motorId r = fromJust $ mapLookup [(LeftMotor, 0x9), (RightMotor, 0xa)] r

-- gets the ultrason value
ultrason :: Device -> IO Double
ultrason d = do { val <- readUltraSonic d; return $ floatDouble val}
