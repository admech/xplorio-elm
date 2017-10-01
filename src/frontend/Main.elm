import Html exposing (Html)
import Svg
import Svg.Attributes


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = \model -> Sub.none
    }



-- MODEL


type alias Model = Description

type alias Description =
  { inputs : List Input
  , outputs : List Output
  }
type alias Input = { name : String, domain : InputValue }
type alias InputValue = String
type alias Output = { name : String, x : String, y : String }


init : (Model, Cmd Msg)
init =
  ( { inputs =
        [ { name = "phi0", domain = "[-pi, pi]" }
        , { name = "t_max", domain = "{10, 100, 1000}" }
        ]
    , outputs =
        [ { name = "angle", x = "time", y = "phi" }
        , { name = "velocity", x = "time", y = "Dphi" }
        , { name = "phase", x = "phi", y = "Dphi" }
        ]
    }
  , Cmd.none
  )



-- UPDATE


type Msg
  = DoNothing


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    DoNothing ->
      (model, Cmd.none)



-- VIEW


view : Model -> Html Msg
view model =
  Html.div []
    [ Html.h1 [] [ Html.text "Xplorio" ]
    , viewDefine model
    , viewSelect model
    , viewXplore model
    ]

-- physical model definition

viewDefine : Model -> Html Msg
viewDefine model = 
  Html.div []
    [ Html.h2 [] [ Html.text "Define" ]
    , Html.h3 [] [ Html.text "Inputs:" ]
    , Html.ul [] (List.map viewInput model.inputs)
    , Html.h3 [] [ Html.text "Outputs:" ]
    , Html.ul [] (List.map viewOutput model.outputs)
    ]

viewInput : Input -> Html Msg
viewInput { name, domain } =
  Html.li [] [ Html.text (name ++ " ∈ " ++ domain) ]

viewOutput : Output -> Html Msg
viewOutput { name, x, y } =
  Html.li [] [ Html.text (name ++ " = " ++ y ++ "(" ++ x ++ ")" ) ]


-- parameter selector

viewSelect : Model -> Html Msg
viewSelect model =
  Html.div []
    [ Html.h2 [] [ Html.text "Select" ]
    , Html.ul [] (List.map viewInput model.inputs)
    ]


-- xplore area: parameter selector + charts of variables

viewXplore : Model -> Html Msg
viewXplore model =
  Html.div []
    [ Html.h2 [] [ Html.text "Xplore" ]
    , Html.ul [] (List.map viewXploreOutput model.outputs)
    ]

viewXploreOutput : Output -> Html Msg
viewXploreOutput { name, x, y } =
  Html.li [] [ Html.text name ]


