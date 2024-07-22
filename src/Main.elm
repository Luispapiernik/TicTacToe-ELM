module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Element as E
import Element.Background as EB
import Element.Events as EE
import Element.Font as EF
import Element.Input as EI
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


type UserToken
    = Heart
    | Clip


type Stage
    = TitleStage
    | GameStage
    | CreditsStage


type alias Model =
    { stage : Stage
    , userToken : Maybe UserToken

    -- , key : Nav.Key
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { stage = TitleStage, userToken = Nothing }, Cmd.none )


-- UPDATE


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GoToGame UserToken
    | GoToTitle
    | GoToCredits


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GoToGame token ->
            ( { stage = GameStage, userToken = Just token }, Cmd.none )

        GoToCredits ->
            ( { model | stage = CreditsStage }, Cmd.none )

        GoToTitle ->
            ( { stage = TitleStage, userToken = Nothing }, Cmd.none )

        ClickedLink _ ->
            ( model, Cmd.none )

        ChangedUrl _ ->
            ( model, Cmd.none )


-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = getTitle model.stage
    , body =
        [ E.layout
            [ EB.color backgroundColor
            , E.behindContent <| E.image [ E.alpha 0.2 ] { src = "../assets/noise.jpg", description = "" }
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
            "TicTacToc"

        GameStage ->
            "TicTacToc"

        CreditsStage ->
            "Credits"


getBody : Model -> E.Element Msg
getBody model =
    case model.stage of
        TitleStage ->
            titleStageView

        GameStage ->
            gamePageView model.userToken

        CreditsStage ->
            creditPageView


titleStageView : E.Element Msg
titleStageView =
    E.column [ E.centerX, E.height E.fill, E.paddingEach { edges | top = 150, bottom = 50 } ]
        [ E.image [ E.centerX, E.height <| E.px 250, EE.onClick GoToTitle ] { src = "../assets/icon.png", description = "" }
        , E.el [ E.alignBottom ] E.none
        , E.image [ E.centerX ] { src = "../assets/title.svg", description = "" }
        , E.row
            [ E.centerX
            ]
            [ E.image [ E.mouseOver [ E.alpha 0.5 ], EE.onClick <| GoToGame Heart ] { src = "../assets/heart.png", description = "" }
            , E.image [ E.mouseOver [ E.alpha 0.5 ], EE.onClick <| GoToGame Clip ] { src = "../assets/clip.png", description = "" }
            ]
        , E.el [ E.paddingEach { edges | bottom = 70 } ] E.none
        , E.el
            [ E.centerX
            , EE.onClick GoToCredits
            , EF.color fontColor
            , E.mouseOver [ E.alpha 0.5 ]
            , EF.family [ EF.external { url = "https://fonts.googleapis.com/css?family=Gaegu", name = "gaegu" } ]
            , EF.size 25
            ]
          <|
            E.text "CREDITS"
        ]


gamePageView : Maybe UserToken -> E.Element Msg
gamePageView userToken =
    case userToken of
        Nothing ->
            E.text "CREDITS"

        Just _ ->
            E.text "CREDITS"


creditPageView : E.Element Msg
creditPageView =
    E.column [ E.centerX ]
        [ addContributor "Developer" "Luis Papiernik" "luispapiernik.dev"
        , addContributor "Designer" "inJoy Design" "http://designinjoy.com"
        , E.el [ EE.onClick GoToTitle ] <| E.text "X"
        ]


addContributor : String -> String -> String -> E.Element Msg
addContributor title name url =
    E.column []
        [ E.text title, E.text name, E.text url ]
