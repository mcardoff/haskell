module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

type Model = (Float, Float)

window :: Display
window = InWindow "Test Window" (600,480) (100,100)

background :: Color
background = white

animationFunc :: Float -> Picture
animationFunc t = Polygon [(50*t,0),(t*50,200),(200+50*t,0)]

-- simulationFunc :: Model -> Picture
-- simulationFunc (a, b) = (b,a)

main :: IO ()
main = simulate
  window
  white
  simulationRate
  initialModel
  drawingFunc
  updateFunc
  where
    simulationRate :: Int
    simulationRate = 60

    initialModel :: Model
    initialModel = (0,0)

    drawingFunc :: Model -> Picture
    -- drawingFunc (theta, dtheta) = Line [(0, 0), (50 * cos theta, 50 * sin theta)]
    drawingFunc (theta, dtheta) = Pictures
        [Translate (-150) 100 $ Text "WOW",
         Line [(0, 0), (50 * cos theta, 50 * sin theta)],
         Translate (50 * cos theta) (50 * sin theta) $ Circle 5]

    updateFunc :: Graphics.Gloss.Data.ViewPort.ViewPort -> Float -> Model -> Model
    updateFunc _ dt (theta, dtheta) =(theta + dt * dtheta, dtheta - dt * cos theta)
-- main = simulate white 20 (0,0) simulationFunc
-- main = animate window white animationFunc


--main = simulate FullScreen white 20 (0,0)

-- main = putStrLn "--Hello, World!--"
