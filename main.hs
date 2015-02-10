
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Complex

-- time -> screen -> angle -> base type -> iterations -> out
-- A tuple containing all the necessary information about the current state
-- of the application
-- time - time elapsed from the last reset, used for animations
-- screen - current screen the user is on
-- angle - base angle of the generated fractal
-- iterations - no. of iterations for the generated fractal
-- out - True if the generated fractal faces outside of the base shape
type State = (Float, Int, Float, Base, Int, Bool)

-- This is just a shortcut so I don't need to type out "Complex Double" all the time
type C = Complex Double

-- All the possible base shapes for the generated Koch fractal
-- 'Side' means a Line, but the name was already taken.
data Base = Side | Triangle | Square deriving (Eq)

-- Gloss doesn't have a 'point' so I just use a line with length 1
-- Points are used later in the Julia set generator
point :: (Float, Float) -> Color -> Picture
point (x,y) c = color c (line [(x, y), (x+1,y+1)])

-- Default window width and height
w :: Int
w = 1366

h:: Int
h = 768

--Side length for the Koch fractals
side :: Float
side = 300.0

--Main function, user can choose between the Koch generator and
--the Julia set generator
main :: IO ()
main = do
  putStrLn "Type 'koch' to start the Koch fractal generator, or 'julia' to start the julia set generator"
  input <- getLine
  if input == "koch" 
  then koch 
  else do
    putStrLn "Which preset ? (1-3)"
    input <- getLine
    putStrLn "Be patient, this can take some time..."
    julia (read input)
   
    
--Opens the Koch generator window     
koch =  play (InWindow "Koch generator" 
             (w, h) 
             (100,  20))
	     black
             60
             initialState
             drawPicture 
             handleInput 
             stepAnimation    

--Opens the Julia set window   
julia n= display (InWindow "Julia Set" (w,h) (100, 20))
               black
               (juliaPresets !! (n-1))

--This ugly function handles input from user in the koch generator.
--It takes the current state and modifies it depending on what is pressed.
--It ignores the input if the # of iterations or angle would go
--beyond boundaries (0-7 for iterations, because higher than that
--is just too slow and 1-90 for the angle)
handleInput :: Event -> State -> State
handleInput (EventKey (Char 'o') Down _ _) (t,3,a,b,i,o) = (t,3,a,b,i,not o)
handleInput (EventKey (SpecialKey KeyRight) Down _ _) (t,3,a,b,i,o) 
    | i<7 = (t,3,a,b,i+1,o)
    | otherwise = (t,3,a,b,i,o)
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) (t,3,a,b,i,o) 
    | i>0 = (t,3,a,b,i-1,o)
    | otherwise = (t,3,a,b,i,o)
handleInput (EventKey (SpecialKey KeyUp) Down _ _) (t,3,a,b,i,o) 
    | a < 90 = (t,3,a+1,b,i,o)
    | otherwise = (t,3,a,b,i,o)
handleInput (EventKey (SpecialKey KeyDown) Down _ _) (t,3,a,b,i,o) 
    | a>1 = (t,3,a-1,b,i,o)
    | otherwise = (t,3,a,b,i,o)
handleInput (EventKey (Char '1') Down _ _) (t,3,a,b,i,o) = (t,3,a,Side,i,o)
handleInput (EventKey (Char '2') Down _ _) (t,3,a,b,i,o) = (t,3,a,Triangle,i,o)
handleInput (EventKey (Char '3') Down _ _) (t,3,a,b,i,o) = (t,3,a,Square,i,o)
handleInput (EventKey (SpecialKey KeyRight) Down _ _) (_,c,a,b,i,o) = 
    (1,(c+1) `mod` 3,a,b,i,o)
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) (_,c,a,b,i,o) = 
    (1,(c-1) `mod` 3,a,b,i,o)
handleInput (EventKey (Char 'g') Down _ _) (_,3,a,b,i,o) = (1.0,0,a,b,i,o)
handleInput (EventKey (Char 'g') Down _ _) (_,_,a,b,i,o) = (1.0,3,a,b,i,o)
handleInput _ state = state

--Sets the initial state
initialState :: State
initialState = (0.0,0,60,Triangle,1,True)

--Adds a multiple of the time difference between frames to the state
stepAnimation :: Float -> State -> State
stepAnimation time (x,choice,a,b,i,o) = (x+(time*500),choice,a,b,i,o)

--This function creates the screen the user sees depending on the state
--of the application. 
--First 3 screens are an animated showcase of preset fractals. User can switch
--between those using arrow keys. By pressing G the screen is set to 3,
--which is the actual generator.
drawPicture :: State -> Picture
drawPicture (time,screen,angle,base,iters,out)
    | screen < 3 = 
  Pictures 
  [Translate (-630) 0 $ Scale (1/3) (1/3) $ color white $ Text "<-"
  ,Translate 570 0 $ Scale (1/3) (1/3) $ color white $ Text "->"
  ,Translate (-600) 300 $ Scale (1/5) (1/5) $ 
   color white $ Text (third (options !! screen))
  ,Translate (-250) 0 $ Rotate (time/10) $ 
   fractalSeq time ((first (options !! screen)) white)
  ,Translate 250 0 $ fractalSeq time ((second (options !! screen) white))
  ,Translate 420 (-310) $ Scale (1/5) (1/5) $ 
   color white $ 
   Text ("Iterations: " ++ (if time < 2500 then show (ceiling(time/500)) else "5"))
  ,Translate (-600) (-310) $ Scale (1/5) (1/5) $
   color white $ Text ("Press G to start the generator.")]

    | screen == 3 =
  Pictures
  [Translate (-600) 300 $ Scale (1/5) (1/5) $ color white $ Text "Generator"
  ,genKoch angle base out white moreOrange iters
  ,Translate (-600) (-310) $ Scale (1/5) (1/5) $ 
   color white $ Text ("Angle: " ++ show angle)
  ,Translate 420 (-310) $ Scale (1/5) (1/5) $ 
   color white $ Text ("Iterations: " ++ show iters)
  ,Translate 420 300 $ Scale (1/5) (1/5) $
   color white $ Text ("Base shapes: ")
  ,Translate 420 270 $ Scale (1/6) (1/6) $
   color white $ Text ("1: Line")
  ,Translate 420 240 $ Scale (1/6) (1/6) $
   color white $ Text ("2: Triangle")
  ,Translate 420 210 $ Scale (1/6) (1/6) $
   color white $ Text ("3: Square")
  ,Translate 420 170 $ Scale (1/5) (1/5) $
   color white $ Text ("Angle:")
  ,Translate 420 140 $ Scale (1/6) (1/6) $
   color white $ Text ("Up/Down")
  ,Translate 420 100 $ Scale (1/5) (1/5) $
   color white $ Text ("Iterations:")
  ,Translate 420 70 $ Scale (1/6) (1/6) $
   color white $ Text ("Left/Right")
  ,Translate 420 30 $ Scale (1/5) (1/5) $
   color white $ Text ("Outside/Inside:")
  ,Translate 420 0 $ Scale (1/6) (1/6) $
   color white $ Text ("'O'")]
    where
      --List of preset screens for the initial showcase
      options :: [((Color -> Int -> Picture),(Color -> Int -> Picture), String)]
      options = [(snowflake,flakeSide,"Koch snowflake")
                ,(cesaro,cesaroSide,"Cesaro fractal 60 degrees")
                ,(cesaro2,cesaroSide2,"Cesaro fractal 85 degrees")]
      --Some helper functions to deal with 3-tuples
      first (x,_,_)  = x
      second (_,y,_) = y
      third (_,_,z)  = z

------------------
--These are all just presets for the fractal generator - genKoch.
--They are displayed in the showcase before the generator
--is started.
snowflake c n = genKoch 60 Triangle True c greener n
flakeSide c n = genKoch 60 Side True c greener n

cesaro c n = genKoch 60 Square False c bluer n
cesaroSide c n = Rotate 180 $ genKoch 60 Side False c bluer n

cesaro2 c n = genKoch 85 Square False c moreOrange n
cesaroSide2 c n = Rotate 180 $ genKoch 85 Side False c moreOrange n
-------------------

--This controls the animation sequence in the showcase. It increases
--the number of fractal iterations depending on the time elapsed.
fractalSeq :: Float -> (Int -> Picture) -> Picture
fractalSeq time f
    | time >= 0 && time < 500 = f 0
    | time >= 500 && time < 1000 = f 1
    | time >= 1000 && time < 1500 = f 2
    | time >= 1500 && time < 2000 = f 3
    | time >= 2000  = f 4

--Brain of the application. It generates a Koch 'snowflake' fractal
--based on all the inputs:
-- alpha - the base angle
-- Base  - which base shape should the fractal be built on
-- out   - True if it is built outside of the shape, False otherwise
-- c     - initial color for the first iteration
-- f     - function (Color -> Color) that changes the color so the 
--         individual iterations can be distinguished
-- n     - how deep should the recursion go
--This is all just geometry to calculate the rotations, translations 
--and scalings, which all depend on the base angle
genKoch :: Float -> Base -> Bool -> Color -> (Color -> Color) -> Int -> Picture
genKoch alpha Side out c f 0 = color c (line [(-side/2,0),(side/2,0)])
genKoch alpha Side out c f n=
    Pictures [Translate ((-1)*((x/2)+((side/2)-x))) 0 $ 
              Scale (x/side) (x/side) $ genKoch alpha Side out c f (n-1)
             ,Translate (y/4) (alt/2) $ Rotate alpha $ 
              Scale (x/side) (x/side) $ genKoch alpha Side out (f c) f (n-1)
             ,Translate ((-y)/4) (alt/2) $ Rotate (360-alpha) $ 
              Scale (x/side) (x/side) $ genKoch alpha Side out (f c) f (n-1)
             ,Translate ((x/2)+((side/2)-x)) 0 $ 
              Scale (x/side) (x/side) $ genKoch alpha Side out c f (n-1)]
    where
      beta = 180 - (2*alpha)
      x    = (side*(sin aRad))/((2*(sin aRad))+(sin bRad))
      y    = side-(2*x)
      aRad = alpha * (pi/180)
      bRad = beta * (pi/180)
      alt  = (sqrt ((4*(x^2)) - y^2))/2

genKoch alpha Triangle out c f n = Translate 0 (((sqrt 3)/6) * (-side)) $
    Pictures [Rotate (if out then 180 else 0) $ genKoch alpha Side out c f n
             ,Translate (-side/4) (((sqrt 3)*side)/4) $ Rotate 120 $ 
              Rotate (if out then 180 else 0) $ genKoch alpha Side out c f n
             ,Translate (side/4) (((sqrt 3)*side)/4) $ Rotate 240 $ 
              Rotate (if out then 180 else 0) $ genKoch alpha Side out c f n]
genKoch alpha Square out c f n =
    Pictures [Translate 0 (-side/2) $ 
              Rotate (if out then 180 else 0) $ genKoch alpha Side out c f n
             ,Translate 0 (side/2) $
              Rotate (if out then 0 else 180) $ genKoch alpha Side out c f n
             ,Translate (side/2) 0 $
              Rotate (if out then 90 else 270) $ genKoch alpha Side out c f n
             ,Translate (-side/2) 0 $
              Rotate (if out then 270 else 90) $ genKoch alpha Side out c f n]

--------------
--These are the color changing functions used in genKoch.
--They take a color and mix it with some other color with the ratio 7:10
greener :: Color -> Color
greener c = mixColors 7 10 (dark $ green) c

bluer :: Color -> Color
bluer c = mixColors 7 10 (dark $ blue) c

moreOrange :: Color -> Color
moreOrange c = mixColors 7 10 (dark $ dark $ orange) c


---------------------
--Julia Set functions

--Transforms the screen coordinates so they are between <-1.6,1.6>
transformCoord :: (Double, Double) -> C
transformCoord (x,y) = ((x*1.6)/(fromIntegral h/2)) :+ ((y*1.6)/(fromIntegral h/2)) 

--Computes the quadratic polynomial z^2 + c where c is the complex parameter,
--z is the point in the complex plane that we are applying the function to, 
--and iters is the maximum number of iterations.
--If the function reaches the max. # of iterations, it returns 0, meaning the
--point is staying bounded.
--The function can also end before it reaches the maximum number of iterations,
--this means that the point has escaped and the number of iterations to get there
--is returned (i) - this will be later used for colouring.
--If neither of these is satisfied, (z^2 + c) is substituted back into the function.
--This happens until one of the conditions is satisfied.
computeJulia :: C -> C -> Int -> Int
computeJulia z c iters = f ((z^2)+c) c 1 iters
    where
      f :: C -> C -> Int -> Int -> Int
      f z c i iters
          | i>iters =0 
          | (realPart (abs z)) > 2 = i
          | otherwise = f ((z^2) + c) c (i+1) iters

--This applies the computeJulia algorithm to every point on the screen.
--It passes the computed value to the coloring function and creates
--a point at the original coordinates with the calculated color.
juliaPic ::  Color -> C -> Int -> Picture
juliaPic col c maxIters =
    Pictures 
    [point (fromIntegral x, fromIntegral y) 
           (genColor col $ computeJulia (f x y) c maxIters)  
               |x <- [ws..we], y <- [hs..he]]
    where
      f x y = transformCoord (fromIntegral x,fromIntegral y)
      ws =  -(h `div` 2)
      hs = ws
      we = h `div` 2
      he = we

--This is a simple coloring function. Bounded points are black, other points
--are colored as a mix of white and color c with ratio which is determined by
--the Int value computed by computeJulia
genColor :: Color -> Int -> Color
genColor c 0 = black
genColor c n = mixColors (fromIntegral n) 20 white $ dark $ dark $ dark $ dark c

--List of presets the user can choose from.
juliaPresets = [juliaPic green ((-0.70176) :+ (-0.3842)) 300
               ,juliaPic blue (0.285 :+ 0.01) 300
               ,juliaPic orange ((-0.4):+ 0.6) 500]
