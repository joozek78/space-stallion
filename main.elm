import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Keyboard
import Markdown
import Signal
import Time (..)
import Window


-- MODEL

areaW = 800
areaH = 600

type alias Model =
  { x:Float, y:Float, vx:Float, vy:Float, dir:Float }


ship : Model
ship =
  { x=0, y=0, vx=0, vy=0, dir=0 }


-- UPDATE

update : (Time, { x:Int, y:Int }, Bool) -> Model -> Model
update (timeDelta, direction, isRunning) model =
  model
    |> changeDirection timeDelta direction
    |> changeVelocity timeDelta direction
    |> updatePosition timeDelta

rotationSpeed = 0.1
acceleration = 0.1

--changeDirection : Time -> {dir: Float} 
changeDirection dt {   x } ({dir} as model)  =
  { model |
    dir <- if | x == 1 -> dir - ( rotationSpeed * dt)
              | x == -1 -> dir + ( rotationSpeed * dt)
              | otherwise -> dir
  }
  
changeVelocity dt {y} ({vx, vy, dir} as model) =
  { model |
    vx <- if | y == 1    -> vx - (sin dir) * acceleration * dt
             | y == -1   -> vx + (sin dir) * acceleration * dt
             | otherwise -> vx,
    vy <- if | y == 1    -> vy + (cos dir) * acceleration * dt
             | y == -1   -> vy - (cos dir) * acceleration * dt
             | otherwise -> vy
  }
    

updatePosition : Time -> Model -> Model
updatePosition dt ({x,y,vx,vy} as model) =
  { model |
      x <- clamp (-areaW/2) (areaW/2) (x + dt * vx),
      y <- clamp (-areaH/2) (areaH/2) (y + dt * vy)
  }


-- VIEW
debugInfo {x,y,vx,vy,dir} = 
  "Rot: "++(toDegrees dir  |> toString)
  ++"<br>Vel: "++(toString vx)++", "++(toString vy)
  
toDegrees r = ((r / pi * 180 |> round) % 360 )

view : (Int,Int) -> Model -> Element
view (w,h) ({x,y,vx,vy,dir} as model)=
      container w h middle <|
      collage areaW areaH
        [ toForm (image areaW areaH "/bg.png")
        , shipForm (x,y) dir
        , toForm (Markdown.toElement (debugInfo model))
            |> move (70-areaW/2, 30-areaH/2)
        ]
shipForm : (Float, Float) -> Float -> Form
shipForm (x, y) dir =
  group [
    circle 10 |> filled red,
    ngon 3 10 |> filled red |> rotate (degrees 90)|> move (0,5)
    ] |> rotate dir |> move (x,y)

-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update ship input)


input : Signal (Time, { x:Int, y:Int }, Bool)
input =
  Signal.sampleOn delta (Signal.map3 (,,) delta Keyboard.arrows Keyboard.shift)


delta : Signal Time
delta =
  Signal.map (\t -> t / 20) (fps 25)
