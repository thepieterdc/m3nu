{-|
Module      : MBotPlus
Description : Provides functions to work with the MBot.
Copyright   : (c) Pieter De Clercq, 2016
License     : MIT
Maintainer  : piedcler.declercq@ugent.be
-}
module MBotPlus (module MBotPlus) where

import Data.Bits
import Data.Maybe(fromJust)
import MBot
import System.HIDAPI as HID(Device)

import Utils

-- |Driving directions of the motors.
data Direction = DirForward -- ^ Drive forward
               | DirLeft -- ^ Turn left
               | DirRight -- ^ Turn right
               | Brake -- Stop all motors
               | DirBackward -- ^ Drive backwards
               | DirBackwardLeft -- ^ Drive backwards and turn left
               | DirBackwardRight -- ^ Drive backwards and turn right
               deriving (Eq, Show)

-- |Led identifiers.
data Led = LeftLed -- ^ The left led
         | RightLed -- ^ The right led
         deriving (Eq, Ord, Show)

-- |Motor identifiers.
data Motor = LeftMotor -- ^ The left motor
           | RightMotor -- ^ The right motor
           deriving (Eq, Ord, Show)

-- |Motor command: drive backwards and turn left.
backwardsLeft :: Device -> IO ()
backwardsLeft d = do
  sendCommand d $ setMotor (motorId LeftMotor) 0 0
  sendCommand d $ setMotor (motorId RightMotor) (complement 80) (complement 0)

-- |Motor command: drive backwards and turn right.
backwardsRight :: Device -> IO()
backwardsRight d = do
  sendCommand d $ setMotor (motorId LeftMotor) 80 0
  sendCommand d $ setMotor (motorId RightMotor) 0 0

-- |Close the connection to the MBot.
close :: Device -> IO ()
close = closeMBot

-- |Open a connection to the MBot.
connect :: (IO Device)
connect = openMBot

-- |Sends a comment to the MBot.
command :: Device -> Command -> IO ()
command = sendCommand

-- |Retrieves a LED int by its identifier.
ledId :: Led -> Int
ledId x = 1 + index [LeftLed, RightLed] x

-- |Sets the color on a given LED.
led :: Device -> Led -> Double -> Double -> Double -> IO ()
led d l r g b = command d $ setRGB (ledId l) ri gi bi where
  (ri, gi, bi) = (doubleInt r, doubleInt g, doubleInt b)

{-|
  Converts the linesensor value to a Double.
  BOTHW = 0, LEFTB = 1, RIGHTB = 2, BOTHB = 3
-}
lineDouble :: Line -> Double
lineDouble x = intDouble $ index [BOTHW, LEFTB, RIGHTB, BOTHB] x

-- |Gets the value of the linesensor as a Double.
lineSensor :: Device -> IO Double
lineSensor d = readLineFollower d >>= \l -> return $ lineDouble l

-- |Gets the appropriate motor instructions for a given Direction.
motorDirection :: Direction -> (Device -> IO())
motorDirection DirForward = goAhead
motorDirection DirLeft = goLeft
motorDirection DirRight = goRight
motorDirection DirBackward = goBackwards
motorDirection DirBackwardLeft = backwardsLeft
motorDirection DirBackwardRight = backwardsRight
motorDirection Brake = stop

-- |Gets the motor int by its identifier.
motorId :: Motor -> Int
motorId r = fromJust $ mapLookup [(LeftMotor, 0x9), (RightMotor, 0xa)] r

-- |Gets the value of the ultrasonic sensor as a Double.
ultrason :: Device -> IO Double
ultrason d = readUltraSonic d >>= \u -> return $ floatDouble u
