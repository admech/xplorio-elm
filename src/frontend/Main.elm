import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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


type alias Model = 
  { description : Description
  , forms : Forms
  }

type alias Description =
  { inputs : List Input
  , outputs : List Output
  , calculation : List InputValue -> Data
  }
type alias Input = { name : String, domain : InputValueDomain }
type alias InputValueDomain = String
type alias InputValue = Float
type alias Output = { name : String, x : String, y : String }
type alias Data = { xs : List Float, ys : List Float }

type alias Forms =
  { newInputName : String
  }

init : (Model, Cmd Msg)
init =
  ( { description = 
      { inputs =
          [ { name = "phi0", domain = "[-pi, pi]" }
          , { name = "t_max", domain = "{10, 100, 1000}" }
          ]
      , outputs =
          [ { name = "angle", x = "time", y = "phi" }
          , { name = "velocity", x = "time", y = "Dphi" }
          , { name = "phase", x = "phi", y = "Dphi" }
          ]
      , calculation =
          \_ ->
            let xs_ = [ 0, 1, 2, 3, 4, 5, 4, 3, 2, 1 ] in
              { xs = xs_
              , ys = List.map (\x -> x * x) xs_
              }
      }
    , forms =
        { newInputName = "input name"
        } 
    }
  , Cmd.none
  )


-- UPDATE


type Msg
  = DoNothing
  | EnterInputName String
  | AddInput


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    DoNothing ->
      ( model, Cmd.none )
    EnterInputName name ->
      ( let forms = model.forms in { model | forms = { forms | newInputName = name } }
      , Cmd.none
      )
    AddInput ->
      ( let
          description = model.description
        in
          { model | description = { description | inputs = (Input model.forms.newInputName "[0,1]") :: description.inputs } }
      , Cmd.none )



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
    , Html.ul [] (List.map viewInput model.description.inputs)
    , Html.input [ type_ "text", placeholder model.forms.newInputName, onInput EnterInputName ] []
    , Html.button [ onClick AddInput ] [ Html.text "Add" ] 
    , Html.h3 [] [ Html.text "Outputs:" ]
    , Html.ul [] (List.map viewOutput model.description.outputs)
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
    , Html.ul [] (List.map viewInput model.description.inputs)
    ]


-- xplore area: parameter selector + charts of variables

viewXplore : Model -> Html Msg
viewXplore model =
  Html.div []
    [ Html.h2 [] [ Html.text "Xplore" ]
    , Html.ul [] (List.map viewXploreOutput model.description.outputs)
    ]

viewXploreOutput : Output -> Html Msg
viewXploreOutput { name, x, y } =
  Html.li [] [ Html.text name ]


