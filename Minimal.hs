import MBot

main =  do
  d <- openMBot
  sendCommand d $ setRGB 2 0  0 255
  sendCommand d $ setRGB 1 255 0 0
  closeMBot d
