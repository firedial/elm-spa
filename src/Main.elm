module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html
import Html.Attributes
import Http
import Route
import Url
import Url.Builder

main : Program () Model Msg
main =
    Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = UrlRequested
    }

type alias Model =
    { key : Nav.Key
    , page : Page
    }

type Page
    = NotFound
    | Top
    | Detail

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    Model key Top
    |> goTo (Route.parse url)

type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | Loaded (Result Http.Error Page)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url))
                Browser.External href ->
                    ( model, Nav.load href )
        UrlChanged url ->
            goTo (Route.parse url) model
        Loaded result ->
            ( { model
                | page =
                    case result of
                        Ok page ->
                            page
                        _ ->
                            NotFound
                }
            , Cmd.none    
            )

goTo : Maybe Route.Route -> Model -> ( Model, Cmd Msg )
goTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )
        Just Route.Top ->
            ( { model | page = Top }, Cmd.none )
        Just Route.Detail->
            ( { model | page = Detail }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

view : Model -> Browser.Document Msg
view model =
    { title = "test"
    , body = 
        [ Html.a [Html.Attributes.href "/"] [Html.text "top"]
        , Html.br [] []
        , Html.a [Html.Attributes.href "/detail"] [Html.text "detail"]
        , Html.br [] []
        , case model.page of
            NotFound ->
                viewNotFound
            Top ->
                viewTop
            Detail ->
                viewDetail
        ]
    }

viewNotFound : Html.Html msg
viewNotFound = 
    Html.text "not found"
    
viewTop : Html.Html msg
viewTop = 
    Html.text "top"

viewDetail : Html.Html msg
viewDetail = 
    Html.text "detail"
