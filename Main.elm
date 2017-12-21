module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href, src)
import Http
import Json.Decode exposing (Decoder, list, field, string, map5, float, int, maybe)


-- initialization


init : ( Model, Cmd Msg )
init =
    ( initModel, getMetricsCmd )


initModel : Model
initModel =
    { metrics = Metrics 0.0 0 0 [] "", isLoading = True, error = Nothing }


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.none)
        }



-- model


type alias HttpRequest =
    { time : Int
    , status : Int
    , path : String
    , method : String
    , durationMs : Int
    }


type alias Metrics =
    { cpuUsage : Float
    , memoryUsage : Int
    , memoryTotal : Int
    , httpRequests : List HttpRequest
    , log : String
    }


type alias Model =
    { metrics : Metrics
    , isLoading : Bool
    , error : Maybe String
    }



-- data handlers


metricsDecoder : Decoder Metrics
metricsDecoder =
    map5 Metrics
        (field "cpuUsage" float)
        (field "memoryUsage" int)
        (field "memoryTotal" int)
        (field "httpRequests"
            (list
                (map5 HttpRequest
                    (field "time" int)
                    (field "status" int)
                    (field "path" string)
                    (field "method" string)
                    (field "durationMs" int)
                )
            )
        )
        (field "log" string)



-- update


type Msg
    = Refresh
    | MetricsResponse (Result Http.Error Metrics)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Refresh ->
            ( { model | isLoading = True }, getMetricsCmd )

        MetricsResponse (Ok metrics) ->
            ( { model | metrics = metrics, isLoading = False }, Cmd.none )

        MetricsResponse (Err error) ->
            ( { model
                | error =
                    Just (Debug.log "error while getting metrics" (toString error))
                , isLoading = False
              }
            , Cmd.none
            )


getMetricsCmd : Cmd Msg
getMetricsCmd =
    let
        req =
            Http.get "http://localhost:4000/my-metrics" metricsDecoder
    in
        Http.send MetricsResponse req



-- view


viewTop : Html Msg
viewTop =
    div [ class "top-details" ]
        [ aside [ class "exception-logo" ] []
        , header [ class "exception-info" ]
            [ h1 [ class "title" ] [ text "Phoenix Monitor Dashboard" ] ]
        ]


viewBottom : Html Msg
viewBottom =
    div [ class "conn-info" ]
        [ details [ class "conn-details" ]
            [ summary [] [ text "Server Log" ]
            , dl [] [ text "log goes here" ]
            ]
        , footer []
            [ text "Just a little play by Leo Ribeiro "
            , a [ href "http://github.com/leordev/phoenix-monitor-dashboard" ]
                [ text "GitHub" ]
            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ viewTop, viewBottom ]
