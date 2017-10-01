import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg
import Svg.Attributes
import Dict exposing (Dict)


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
    inputs : Inputs
  , outputs : List Output
  , calculation : List InputValue -> Data
    -- Forms
  , newInputName : String
  , newInputDomain : InputDomain
    -- Error handling
  , error : Maybe String
  }

type alias Inputs = Dict String InputDomain
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
      inputs = Dict.fromList
        [ ( "phi0", Range -1 1 )
        , ( "t_max", Variants [ 10, 100, 1000 ] )
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
  | SetNewInputDomain InputDomain
  | DeleteInput String


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
    SetNewInputDomain domain ->
      ( { model | newInputDomain = domain }
      , Cmd.none
      )
    AddInput ->
      ( { model | inputs = Dict.insert model.newInputName model.newInputDomain model.inputs }
      , Cmd.none
      )
    DeleteInput name ->
      ( { model | inputs = Dict.remove name model.inputs }
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
    , Html.ul [] (htmlFromInputs model.inputs)
    , Html.fieldset []
        [ radio "Real" (SetNewInputDomain Real)
        , radio "Integer" (SetNewInputDomain Integer)
        , radio "Natural" (SetNewInputDomain Natural)
        , radio "Range" (SetNewInputDomain (Range -1 1))
        , radio "Variants" (SetNewInputDomain (Variants [1, 2, 3]))
        ]
    , Html.input [ placeholder model.newInputName, onInput EnterInputName ] []
    , viewNewInputDomain model.newInputDomain
    , Html.button [ onClick AddInput ] [ Html.text "Add" ] 
    , Html.h3 [] [ Html.text "Outputs:" ]
    , Html.ul [] (List.map viewOutput model.outputs)
    ]

htmlFromInputs : Inputs -> List (Html Msg)
htmlFromInputs inputs =
  (List.map viewInput (entriesFromDict inputs))

entriesFromDict : Dict comparable b -> List (comparable, b)
entriesFromDict dict =
  List.map2 (\u -> \v -> (u, v)) (Dict.keys dict) (Dict.values dict)

radio : String -> msg -> Html msg
radio value msg =
  Html.label
    [ style [("padding", "20px")]
    ]
    [ Html.input [ type_ "radio", name "new-input-domain-type", onClick msg ] []
    , Html.text value
    ]

viewInput : (String, InputDomain) -> Html Msg
viewInput ( name, domain ) =
  Html.li []
    [ Html.text (name ++ " ∈ " ++ stringFromDomain domain)
    , Html.button [ onClick (DeleteInput name) ] [ Html.text "x" ]
    ]

stringFromDomain : InputDomain -> String
stringFromDomain domain =
  case domain of
    Range min max -> "[" ++ (toString min) ++ ", " ++ (toString max) ++ "]"
    Variants vs -> "{" ++ (String.join ", " (List.map toString vs)) ++ "}"
    Real -> "ℝ"
    Integer -> "ℤ"
    Natural -> "ℕ"

viewNewInputDomain : InputDomain -> Html Msg
viewNewInputDomain domain = 
  case domain of
    Range min max -> 
      Html.div []
        [ Html.input [ placeholder "-1", onInput (validateFloat (\it -> SetNewInputDomain (Range it max))) ] []
        , Html.input [ placeholder "1", onInput (validateFloat (\it -> SetNewInputDomain (Range min it))) ] []
        ]
    Variants vs ->
      Html.div []
        [ Html.input [ placeholder "2.7, 1.5, 3", onInput (validateCommaSeparatedFloats (\vs -> SetNewInputDomain (Variants vs))) ] []
        ]
    -- https://en.wikipedia.org/wiki/Mathematical_operators_and_symbols_in_Unicode
    Real -> Html.text "ℝ"
    Integer -> Html.text "ℤ"
    Natural -> Html.text "ℕ"

validateFloat : (Float -> Msg) -> String -> Msg
validateFloat viewFun str =
  case String.toFloat str of
    Ok f -> viewFun f
    Err s -> ShowError s

validateCommaSeparatedFloats : (List Float -> Msg) -> String -> Msg
validateCommaSeparatedFloats viewFun str =
  let (goods, bads) = str
    |> String.split ","
    |> List.map String.trim
    |> List.filter (not << String.isEmpty)
    |> List.map String.toFloat
    |> List.partition (\mbF -> case mbF of
        Ok _ -> True
        Err _ -> False )
  in case bads of
    [] -> let vs = 
            goods
              |> List.map Result.toMaybe
              |> List.map (Maybe.withDefault 9999999) -- fixme: partition does not extact values :(
          in viewFun vs
    _ -> ShowError ("could not parse variants from: " ++ (toString bads))
      

viewOutput : Output -> Html Msg
viewOutput { name, x, y } =
  Html.li [] [ Html.text (name ++ " = " ++ y ++ "(" ++ x ++ ")" ) ]


-- parameter selector

viewSelect : Model -> Html Msg
viewSelect model =
  Html.div []
    [ Html.h2 [] [ Html.text "Select" ]
    , Html.ul [] (htmlFromInputs model.inputs)
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


