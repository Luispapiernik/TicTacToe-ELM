module Main exposing (main)

import Array as A
import Browser
import Browser.Navigation as Nav
import Element as E
import Element.Background as EB
import Element.Events as EE
import Element.Font as EF
import Process as P
import Random as R
import Task as T
import Url exposing (Url)



-- TODO
-- 1. I need to rethink how I am loading Fonts
-- 2. How to manage state in the application in a way the user can reload
--    go back, without completely erasing current state
-- CONSTANTS


backgroundColor : E.Color
backgroundColor =
    E.rgb255 255 243 234


fontColor : E.Color
fontColor =
    E.rgb255 121 137 196


edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


strategies : List Strategy
strategies =
    [ dumbPlayer
    ]


initialGameState =
    { userToken = Heart
    , currentTurn = User
    , tableState = A.repeat 9 Nothing
    , strategy = dumbPlayer
    }



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        }



-- MODEL


type GameTokens
    = Heart
    | Clip


type Players
    = User
    | Computer


type Stage
    = TitleStage
    | GameStage
    | CreditsStage


type alias Table =
    A.Array (Maybe Players)


type alias Strategy =
    Table -> Int


type alias GameState =
    { userToken : GameTokens
    , currentTurn : Players
    , tableState : Table
    , strategy : Strategy
    }


type alias Model =
    { stage : Stage
    , gameState : GameState

    -- , key : Nav.Key
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { stage = TitleStage
      , gameState = initialGameState
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GoToGame GameTokens
    | GoToTitle
    | GoToCredits
    | LocateToken Int Players
    | ResetGame
    | ChosenStrategy Strategy



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GoToGame token ->
            ( { stage = GameStage
              , gameState = { initialGameState | userToken = token }
              }
            , R.generate ChosenStrategy strategyPicker
            )

        GoToCredits ->
            ( { model | stage = CreditsStage }, Cmd.none )

        GoToTitle ->
            ( { model | stage = TitleStage }
            , Cmd.none
            )

        ClickedLink _ ->
            ( model, Cmd.none )

        ChangedUrl _ ->
            ( model, Cmd.none )

        LocateToken index player ->
            let
                newState =
                    processUserTurn index player model

                computerIndex =
                    model.gameState.strategy model.gameState.tableState

                command =
                    if player == User then
                        delay 500 (LocateToken computerIndex Computer)

                    else
                        Cmd.none
            in
            ( newState, command )

        ResetGame ->
            let
                gameState =
                    model.gameState
            in
            ( { model
                | gameState = { gameState | tableState = A.repeat 9 Nothing }
              }
            , R.generate ChosenStrategy strategyPicker
            )

        ChosenStrategy strategy ->
            let
                gameState =
                    model.gameState
            in
            ( { model
                | gameState = { gameState | strategy = strategy }
              }
            , Cmd.none
            )


strategyPicker : R.Generator Strategy
strategyPicker =
    R.uniform dumbPlayer strategies



-- https://stackoverflow.com/questions/40599512/how-to-achieve-behavior-of-settimeout-in-elm


delay : Float -> msg -> Cmd msg
delay time msg =
    -- create a task that sleeps for `time`
    P.sleep time
        |> -- once the sleep is over, ignore its output (using `always`)
           -- and then we create a new task that simply returns a success, and the msg
           T.andThen (always <| T.succeed msg)
        |> -- finally, we ask Elm to perform the Task, which
           -- takes the result of the above task and
           -- returns it to our update function
           T.perform identity


processUserTurn : Int -> Players -> Model -> Model
processUserTurn index player model =
    let
        gameState =
            model.gameState

        updatedTableState =
            A.set index (Just player) gameState.tableState
    in
    { model | gameState = { gameState | tableState = updatedTableState } }


dumbPlayer : Table -> Int
dumbPlayer table =
    0



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = getTitle model.stage
    , body =
        [ E.layout
            [ EB.color backgroundColor
            , E.behindContent <|
                E.image
                    [ E.alpha 0.2
                    ]
                    { src = "../assets/noise.jpg", description = "" }
            , E.clip
            ]
          <|
            getBody model
        ]
    }


getTitle : Stage -> String
getTitle stage =
    case stage of
        TitleStage ->
            "Tic Tac Toc - Title"

        GameStage ->
            "Tic Tac Toc - Game"

        CreditsStage ->
            "Tic Tac Toc - Credits"


getBody : Model -> E.Element Msg
getBody model =
    case model.stage of
        TitleStage ->
            titleStageView

        GameStage ->
            gamePageView model

        CreditsStage ->
            creditPageView


titleStageView : E.Element Msg
titleStageView =
    E.column [ E.centerX, E.height E.fill, E.paddingEach { edges | top = 150, bottom = 50 } ]
        [ E.image
            [ E.centerX
            , E.height <| E.px 250
            , EE.onClick GoToTitle
            ]
            { src = "../assets/icon.png", description = "" }
        , E.el [ E.alignBottom ] E.none
        , E.image [ E.centerX ] { src = "../assets/title.svg", description = "" }
        , E.row
            [ E.centerX

            -- , E.spacingXY 15 0
            ]
            [ E.image
                [ E.pointer
                , E.mouseOver [ E.alpha 0.5 ]
                , EE.onClick <| GoToGame Heart
                ]
                { src = "../assets/heart.png", description = "" }
            , E.image
                [ E.pointer
                , E.mouseOver [ E.alpha 0.5 ]
                , EE.onClick <| GoToGame Clip
                ]
                { src = "../assets/clip.png", description = "" }
            ]
        , E.el [ E.paddingEach { edges | bottom = 70 } ] E.none
        , E.el
            [ E.centerX
            , EE.onClick GoToCredits
            , E.pointer
            , EF.color fontColor
            , E.mouseOver [ E.alpha 0.5 ]
            , EF.family
                [ EF.external
                    { url = "https://fonts.googleapis.com/css?family=Gaegu"
                    , name = "gaegu"
                    }
                ]
            , EF.size 25
            ]
          <|
            E.text "CREDITS"
        ]


hoverTilesAttr : Int -> List (E.Attribute Msg)
hoverTilesAttr index =
    [ E.width <| E.px 100
    , E.height <| E.px 100
    , E.mouseOver [ E.alpha 0.5 ]
    , EE.onClick <| LocateToken index User
    ]


drawToken : Int -> Maybe GameTokens -> E.Element Msg
drawToken index token =
    case token of
        Just Heart ->
            E.el (hoverTilesAttr index) <| E.image [] { description = "", src = "../assets/heart.png" }

        Just Clip ->
            E.el (hoverTilesAttr index) <| E.image [] { description = "", src = "../assets/clip.png" }

        Nothing ->
            E.el (hoverTilesAttr index ++ [ E.mouseOver [ EB.color <| E.rgb 1 0 0 ] ]) E.none


getToken : Maybe Players -> GameTokens -> Maybe GameTokens
getToken tablePlayer userToken =
    case tablePlayer of
        Just User ->
            if userToken == Heart then
                Just Heart

            else
                Just Clip

        Just Computer ->
            if userToken == Heart then
                Just Clip

            else
                Just Heart

        _ ->
            Nothing


getTablePlayer : Int -> Table -> Maybe Players
getTablePlayer index tableState =
    let
        token =
            A.get index tableState
    in
    case token of
        Just (Just move) ->
            Just move

        _ ->
            Nothing


tableView : GameState -> E.Element Msg
tableView gameState =
    E.column [ E.alpha 0.5 ]
        [ E.row []
            [ drawToken 0 <| getToken (getTablePlayer 0 gameState.tableState) gameState.userToken
            , drawToken 1 <| getToken (getTablePlayer 1 gameState.tableState) gameState.userToken
            , drawToken 2 <| getToken (getTablePlayer 2 gameState.tableState) gameState.userToken
            ]
        , E.row []
            [ drawToken 3 <| getToken (getTablePlayer 3 gameState.tableState) gameState.userToken
            , drawToken 4 <| getToken (getTablePlayer 4 gameState.tableState) gameState.userToken
            , drawToken 5 <| getToken (getTablePlayer 5 gameState.tableState) gameState.userToken
            ]
        , E.row []
            [ drawToken 6 <| getToken (getTablePlayer 6 gameState.tableState) gameState.userToken
            , drawToken 7 <| getToken (getTablePlayer 7 gameState.tableState) gameState.userToken
            , drawToken 8 <| getToken (getTablePlayer 8 gameState.tableState) gameState.userToken
            ]
        ]


gamePageView : Model -> E.Element Msg
gamePageView model =
    E.column [ E.centerX ]
        [ E.image
            [ E.centerX
            , E.height <| E.px 250
            , EE.onClick GoToTitle
            ]
            { src = "../assets/icon.png"
            , description = ""
            }
        , E.row []
            [ E.image [] { src = "../assets/heart.png", description = "" }
            , E.text "'s turn"
            ]
        , E.image
            [ E.inFront <| tableView model.gameState
            ]
            { src = "../assets/board.svg", description = "" }
        , E.image
            [ EE.onClick ResetGame
            ]
            { src = "../assets/start_over.svg", description = "" }
        ]


creditPageView : E.Element Msg
creditPageView =
    E.column [ E.centerX, E.centerY, E.height E.fill ]
        [ addContributor "Developer" "Luis Papiernik" "luispapiernik.dev"
        , addContributor "Designer" "inJoy Design" "designinjoy.com"
        , E.el [ E.paddingEach { edges | bottom = 0 } ] E.none
        , E.el
            [ E.centerX
            , EE.onClick GoToTitle
            ]
          <|
            E.image [] { src = "../assets/icon_close.svg", description = "" }
        ]


addContributor : String -> String -> String -> E.Element Msg
addContributor title name url =
    E.column [ E.centerX ]
        [ E.el
            [ E.centerX
            , EF.size 70
            , EF.family
                [ EF.external
                    { url = "https://fonts.googleapis.com/css?family=Gaegu"
                    , name = "gaegu"
                    }
                ]
            ]
          <|
            E.text title
        , E.el [ E.paddingEach { edges | top = 10 } ] E.none
        , E.el
            [ E.centerX
            , EF.size 35
            , EF.family
                [ EF.external
                    { url = "https://fonts.googleapis.com/css?family=Gaegu"
                    , name = "gaegu"
                    }
                ]
            , E.pointer
            ]
          <|
            E.text name
        , E.el [ E.paddingEach { edges | top = 5 } ] E.none
        , E.newTabLink
            [ E.centerX
            , EF.color fontColor
            , EF.size 20
            , EF.family
                [ EF.external
                    { url = "https://fonts.googleapis.com/css?family=Gaegu"
                    , name = "gaegu"
                    }
                ]
            , E.pointer
            , E.mouseOver [ E.alpha 0.5 ]
            ]
            { url = "https://" ++ url, label = E.text url }
        ]
