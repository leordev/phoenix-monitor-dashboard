module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, list, field, string, map5, float, int, maybe)
import Time
import Date
import Date.Extra.Core as DateCore
import Date.Extra.Format as DateFormat
import Date.Extra.Config.Config_en_us as DateConfig


-- initialization


init : ( Model, Cmd Msg )
init =
    ( initModel, getMetricsCmd )


initModel : Model
initModel =
    { metrics = Metrics 0.0 0 0 [] ""
    , bottomContent = Requests
    , isLoading = True
    , error = Nothing
    }


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


type BottomContent
    = Requests
    | Logs


type alias Model =
    { metrics : Metrics
    , bottomContent : BottomContent
    , isLoading : Bool
    , error : Maybe String
    }



-- data handlers


metricsDecoder : Decoder Metrics
metricsDecoder =
    map5 Metrics
        (field "cpu_usage" float)
        (field "memory_usage" int)
        (field "memory_total" int)
        (field "http_requests"
            (list
                (map5 HttpRequest
                    (field "time" int)
                    (field "status" int)
                    (field "path" string)
                    (field "method" string)
                    (field "duration_ms" int)
                )
            )
        )
        (field "log" string)



-- update


type Msg
    = Refresh
    | SetBottomContent BottomContent
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

        SetBottomContent content ->
            ( { model | bottomContent = content }, Cmd.none )


getMetricsCmd : Cmd Msg
getMetricsCmd =
    let
        req =
            Http.get "http://localhost:4000/my-metrics" metricsDecoder
    in
        Http.send MetricsResponse req



-- view


viewCard : String -> String -> String -> Html Msg
viewCard title content footer =
    div [ class "card" ]
        [ div [ class "card-title" ] [ text title ]
        , div [ class "card-content" ] [ text content ]
        , div [ class "card-footer" ] [ text footer ]
        ]


viewTop : Model -> Html Msg
viewTop model =
    let
        metrics =
            model.metrics
    in
        div [ class "top-details" ]
            [ aside [ class "exception-logo" ] []
            , header [ class "exception-info" ]
                [ h1 [ class "title" ] [ text "Phoenix Monitor Dashboard" ] ]
            , div [ class "code-explorer" ]
                [ viewCard "CPU Usage"
                    ((toString (metrics.cpuUsage * 100)) ++ "%")
                    ("out of 100%")
                , viewCard
                    "Memory Usage"
                    (toString metrics.memoryUsage)
                    ("out of " ++ (toString metrics.memoryTotal))
                , viewCard "Total Requests"
                    (toString (List.length metrics.httpRequests))
                    "avg response of 4ms"
                ]
            ]


bottomButton : Model -> String -> BottomContent -> Html Msg
bottomButton model name action =
    if model.bottomContent == action then
        span [ class "bottom-item-selected" ] [ text name ]
    else
        a [ onClick (SetBottomContent action) ]
            [ text name ]


formatTime : Int -> String
formatTime time =
    Debug.log "testing time " (round ((toFloat time) / 1000000))
        |> DateCore.fromTime
        |> DateFormat.format DateConfig.config "%d-%m-%y at %H:%M:%S.%L"
        |> Debug.log "current time "


listRequests : List HttpRequest -> List (Html Msg)
listRequests requests =
    requests
        |> List.map
            (\req ->
                tr []
                    [ td [] [ text (formatTime req.time) ]
                    , td [] [ text req.method ]
                    , td [] [ text (toString req.status) ]
                    , td [] [ text req.path ]
                    , td [] [ text (toString req.durationMs) ]
                    ]
            )


bottomContent : Model -> Html Msg
bottomContent model =
    div [ class "bottom-content" ]
        [ case model.bottomContent of
            Requests ->
                div [ class "requests" ]
                    [ table []
                        [ thead []
                            [ tr []
                                [ th [] [ text "Time" ]
                                , th [] [ text "Method" ]
                                , th [] [ text "Status" ]
                                , th [] [ text "Path" ]
                                , th [] [ text "Duration (ms)" ]
                                ]
                            ]
                        , tbody [] (listRequests model.metrics.httpRequests)
                        ]
                    ]

            Logs ->
                div [ class "logs" ]
                    [ pre [] [ text model.metrics.log ] ]
        ]


viewBottom : Model -> Html Msg
viewBottom model =
    let
        requestsButton =
            bottomButton model "Requests" Requests

        logsButton =
            bottomButton model "Logs" Logs

        content =
            bottomContent model
    in
        div [ class "bottom-container" ]
            [ requestsButton
            , logsButton
            , content
            , footer []
                [ text "Just a little play by Leo Ribeiro "
                , a [ href "http://github.com/leordev/phoenix-monitor-dashboard" ]
                    [ text "GitHub" ]
                ]
            ]


view : Model -> Html Msg
view model =
    div []
        [ viewTop model, viewBottom model ]
