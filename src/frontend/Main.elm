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
  { -- Description
    inputs : List Input
  , outputs : List Output
  , calculation : List InputValue -> Data
    -- Forms
  , newInputName : String
  , newInputDomain : InputDomain
    -- Error handling
  , error : Maybe String
  }

type alias Input = { name : String, domain : InputDomain }
type InputDomain
  = Range Float Float 
  | Variants (List Float)
  | Real | Integer | Natural
type alias InputValue = Float
type alias Output = { name : String, x : String, y : String }
type alias Data = { xs : List Float, ys : List Float }

init : (Model, Cmd Msg)
init =
  ( { -- Description
      inputs =
        [ { name = "phi0", domain = Range -1 1 }
        , { name = "t_max", domain = Variants [ 10, 100, 1000 ] }
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
      -- Forms
    , newInputName = "input name"
    , newInputDomain = Range -1 1
      -- Error handling
    , error = Nothing
    }
  , Cmd.none
  )


-- UPDATE


type Msg
  = DoNothing
  | ShowError String
  | AddInput
  | EnterInputName String
  | EnterNewInputDomain InputDomain


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    DoNothing ->
      ( model, Cmd.none )
    ShowError s ->
      ( { model | error = Just s }, Cmd.none )
    EnterInputName name ->
      ( { model | newInputName = name }
      , Cmd.none
      )
    EnterNewInputDomain domain ->
      ( { model | newInputDomain = domain }
      , Cmd.none
      )
    AddInput ->
      ( { model | inputs = (Input model.newInputName model.newInputDomain) :: model.inputs }
      , Cmd.none
      )



-- VIEW


view : Model -> Html Msg
view model =
  Html.div []
    [ Html.h1 [] [ Html.text "Xplorio" ]
    , viewError model
    , viewDefine model
    , viewSelect model
    , viewXplore model
    ]

-- error handling

viewError : Model -> Html Msg
viewError model =
  case model.error of
    Nothing -> Html.div [] []
    Just error -> Html.div [] [ Html.text error ]
      

-- physical model definition

viewDefine : Model -> Html Msg
viewDefine model = 
  Html.div []
    [ Html.h2 [] [ Html.text "Define" ]
    , Html.h3 [] [ Html.text "Inputs:" ]
    , Html.ul [] (List.map viewInput model.inputs)
    , Html.input [ placeholder model.newInputName, onInput EnterInputName ] []
    , viewNewInputDomain model.newInputDomain
    , Html.button [ onClick AddInput ] [ Html.text "Add" ] 
    , Html.h3 [] [ Html.text "Outputs:" ]
    , Html.ul [] (List.map viewOutput model.outputs)
    ]

viewInput : Input -> Html Msg
viewInput { name, domain } =
  Html.li [] [ Html.text (name ++ " ∈ " ++ stringFromDomain domain) ]

stringFromDomain : InputDomain -> String
stringFromDomain domain =
  case domain of
    Range min max -> "[" ++ (toString min) ++ ", " ++ (toString max) ++ "]"
    Variants vs -> "{" ++ (String.join ", " (List.map toString vs)) ++ "}"
    other -> toString other

viewNewInputDomain : InputDomain -> Html Msg
viewNewInputDomain domain = 
  case domain of
    Range min max -> 
      Html.div []
        [ Html.input [ placeholder "min", onInput (\fieldValue -> enterNewInputDomain (\it -> Range it max) fieldValue) ] []
        , Html.input [ placeholder "max", onInput (\fieldValue -> enterNewInputDomain (\it -> Range min it) fieldValue) ] []
        ]
    _ -> Html.text "meh"

enterNewInputDomain : (Float -> InputDomain) -> String -> Msg
enterNewInputDomain create val =
  case String.toFloat val of
    Ok f -> EnterNewInputDomain (create f)
    Err s -> ShowError s
      

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


