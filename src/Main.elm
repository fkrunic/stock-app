module Main exposing (main)

{-

TODO: Change background to match stock app
TODO: Convert bootstrap attributes to typed styles
TODO: Make the cards clickable into a focus view that has a chart over time of stock prices
TODO: Fix the view on mobile devices
TODO: Create run tasks for different screen sizes
TODO: Download TODO extension to manage tasks

-}

import Browser 

import Random 
import Time


import Color
import Path exposing (Path)
import Scale exposing (ContinuousScale)
import Shape

import TypedSvg exposing (g, svg)
import TypedSvg.Attributes exposing (fill, stroke, strokeDasharray, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (strokeWidth)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), Transform(..))

import Html exposing (..)
import Html.Attributes exposing (..)

import Bootstrap.CDN as CDN 
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col 
import Bootstrap.ListGroup as ListGroup
import Round



-----------------------------------------------------------------------------------------
-- Initialization and type definitions
-----------------------------------------------------------------------------------------

main : Program () Model Msg
main = Browser.element
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }   


type alias Model = List Stock

type alias Stock = 
  { symbol    : String 
  , display   : String
  , open      : Float  
  , price     : Float 
  , direction : Direction 
  , history   : List Float
  , cp        : ChartParameters
  }


type alias ChartParameters =
  { svgWidth    : Float
  , svgHeight   : Float
  , svgPadding  : Float
  , xlim        : (Float, Float)
  , ylim        : (Float, Float)
  , xScale      : ContinuousScale Float 
  , yScale      : ContinuousScale Float
  , lpTrans     : LinePointTransformer
  , apTrans     : AreaPointTransformer
  , pricePoints : List (Float, Float) 
  , refPoints   : List (Float, Float)
  , fillColor   : Color.Color 
  , lineColor   : Color.Color
  }

makeStock : String -> String -> Float -> Stock
makeStock symbol display open = 
  let svgWidth = 500.0
      svgHeight = 500.0
      svgPadding = 30.0
      xlim = (0.0, toFloat <| duration - 1)
      ylim = (open - 10, open + 10)
      xScale = buildXScale svgWidth svgPadding xlim
      yScale = buildYScale svgHeight svgPadding ylim
  in 
  { symbol = symbol 
  , display = display 
  , open = open 
  , price = open 
  , direction = Neutral 
  , history = [open]
  , cp = 
    { svgWidth = svgWidth
    , svgHeight = svgHeight
    , svgPadding = svgPadding
    , xlim = xlim
    , ylim = ylim
    , xScale = xScale
    , yScale = yScale
    , lpTrans = buildLPT xScale yScale
    , apTrans = buildAPT xScale yScale
    , pricePoints = [(0, open)]
    , refPoints = enumerateF <| List.repeat duration open
    , fillColor = buildFillColor Up
    , lineColor = buildLineColor Up
    }
  }


type Direction = Neutral | Up | Down  

type Msg = Tick Time.Posix
         | Movement (List Float)


subscriptions : Model -> Sub Msg
subscriptions _ = Time.every 1000 Tick


initialOpen : Model
initialOpen = 
  [ makeStock "AAPL" "Apple, Inc."      128.98
  , makeStock "FB"   "Facebook, Inc."   294.42
  , makeStock "GOOG" "Alphabet, Inc."   1675.04
  , makeStock "AMZN" "Amazon.com, inc." 3499.39
  ]


init : () -> (Model, Cmd Msg) 
init _ = (initialOpen, Cmd.none)


-----------------------------------------------------------------------------------------
-- Updating stock prices
-----------------------------------------------------------------------------------------


type alias LinePointTransformer = (Float, Float) -> Maybe (Float, Float)
type alias AreaPointTransformer = (Float, Float) -> Maybe ((Float, Float), (Float, Float))


duration : Int 
duration = 10


enumerateF : List a -> List (Float, a)    
enumerateF xs = let n = List.length xs 
                in  List.map2 Tuple.pair (List.range 0 n |> List.map toFloat) xs  


buildLineColor : Direction -> Color.Color
buildLineColor d = 
  case d of 
    Up      -> Color.rgb255 0 255 0 -- green
    Down    -> Color.rgb255 255 0 0 -- red
    Neutral -> buildLineColor Up


buildFillColor : Direction -> Color.Color 
buildFillColor d = 
  case d of
    Up      -> Color.rgba 0 1 0 0.10  -- transparent green
    Down    -> Color.rgba 1 0 0 0.10  -- transparent red
    Neutral -> buildFillColor Up


updateStock : (Float, Stock) -> Stock   
updateStock ps = 
  case ps of (move, stock) -> let price = stock.price + move
                                  direction = if price > stock.open then Up else Down 

                                  history = List.take duration <| price :: stock.history
                                  pricePoints = enumerateF <| List.reverse history

                                  lineColor = buildLineColor direction
                                  fillColor = buildFillColor direction 

                                  ylim = case (List.minimum history, List.maximum history) of
                                    (Just min, Just max) -> (min - 10, max + 10)
                                    (_, _)               -> (0, 10000)  

                                  cp = stock.cp       
                                  yScale = buildYScale cp.svgHeight cp.svgPadding ylim 
                                  lpTrans = buildLPT cp.xScale yScale
                                  apTrans = buildAPT cp.xScale yScale                      

                              in  { stock | price = price
                                  , direction = direction
                                  , history = history
                                  , cp = 
                                    { cp | ylim = ylim
                                    , yScale = yScale 
                                    , lpTrans = lpTrans 
                                    , apTrans = apTrans
                                    , pricePoints = pricePoints
                                    , fillColor = fillColor
                                    , lineColor = lineColor
                                    }
                                  }


updateModel : List Float -> Model -> Model 
updateModel moves model = List.map2 Tuple.pair moves model |> List.map updateStock


randomMovement : Int -> Random.Generator (List Float)
randomMovement n = Random.list n (Random.float -1 1)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of 
    Tick _           -> let n = List.length model 
                        in  (model, Random.generate Movement (randomMovement n))

    Movement moves   -> (updateModel moves model, Cmd.none) 


-----------------------------------------------------------------------------------------
-- Stock chart rendering
-----------------------------------------------------------------------------------------    


buildXScale : Float -> Float -> (Float, Float) -> ContinuousScale Float
buildXScale svgWidth svgPadding xlim = Scale.linear (0, svgWidth - 2 * svgPadding) xlim 


buildYScale : Float -> Float -> (Float, Float) -> ContinuousScale Float
buildYScale svgHeight svgPadding ylim = Scale.linear (svgHeight - 2 * svgPadding, 0) ylim

buildLPT : ContinuousScale Float -> ContinuousScale Float -> LinePointTransformer
buildLPT xScale yScale (x, y) = Just (Scale.convert xScale x, Scale.convert yScale y)

buildAPT : ContinuousScale Float -> ContinuousScale Float -> AreaPointTransformer
buildAPT xScale yScale (x, y) =
  Just
      ( (Scale.convert xScale x, Tuple.first <| Scale.rangeExtent yScale)
      , (Scale.convert xScale x, Scale.convert yScale y)
      )


linePath : LinePointTransformer -> List (Float, Float) -> Path 
linePath lpTrans = List.map lpTrans >> Shape.line Shape.monotoneInXCurve

areaPath : AreaPointTransformer -> List (Float, Float) -> Path 
areaPath apTrans = List.map apTrans >> Shape.area Shape.monotoneInXCurve         


chart : ChartParameters -> Svg msg
chart cp =
  svg [ viewBox 0 0 cp.svgWidth cp.svgHeight ]
      [ g [ transform [ Translate (cp.svgPadding - 1) (cp.svgHeight - cp.svgPadding) ] ]
          []
      , g [ transform [ Translate (cp.svgPadding - 1) cp.svgPadding ] ]
          []
      , g [ transform [ Translate cp.svgPadding cp.svgPadding ], TypedSvg.Attributes.class [ "series" ] ]
          [ Path.element (areaPath cp.apTrans cp.pricePoints) 
            [ strokeWidth 6, fill <| Paint cp.fillColor ]

          , Path.element (linePath cp.lpTrans cp.pricePoints) 
            [ stroke <| Paint cp.lineColor, strokeWidth 10, fill PaintNone ]

          , Path.element (linePath cp.lpTrans cp.refPoints) 
              [ stroke <| Paint cp.lineColor, strokeWidth 6, fill PaintNone,  strokeDasharray "15" ]
          ]
      ]


-----------------------------------------------------------------------------------------
-- Price rendering
-----------------------------------------------------------------------------------------    


tickerStyle : Direction -> Button.Option a 
tickerStyle d =
  case d of 
    Neutral -> Button.light
    Up      -> Button.success
    Down    -> Button.danger

formatTicker : Float -> String 
formatTicker p =  if p > 0 
                  then "+" ++ Round.round 2 p 
                  else Round.round 2 p

stockListItem : Stock -> ListGroup.Item a 
stockListItem stock = 
  ListGroup.li []
    [ Grid.row []
      [ Grid.col [] 
        [ text stock.symbol 
        , br [] [] 
        , text stock.display
        ]
      , Grid.col [ Col.xs12, Col.mdAuto ] []           
      , Grid.col [ Col.xs, Col.lg1, Col.attrs [ class "text-right" ] ] 
        [ chart stock.cp
        ]
      , Grid.col [ Col.xs, Col.lg1, Col.attrs [ class "text-right" ] ]
        [ text <| Round.round 2 stock.price
        , br [] []
        , Button.button [ tickerStyle stock.direction, Button.block, Button.small ] 
          [ text <| formatTicker <| stock.price - stock.open
          ]
        ]        
      ]
    ]

    
view : Model -> Html Msg 
view model = 
  Grid.container []
    [ CDN.stylesheet 
    , ListGroup.ul <| List.map stockListItem model
    ]
