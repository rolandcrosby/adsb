module Main exposing (..)

import Dict exposing (Dict(..))
import Time exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style)
import Http
import Task
import Geolocation
import ADSB


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { latitude : Float
    , longitude : Float
    , planes : Dict Int ADSB.Aircraft
    , lastDv : String
    , error : String
    , requestInFlight : Bool
    }


type Msg
    = LoadPlanes Float Float
    | PlaneResponse (Result Http.Error ADSB.Response)
    | Tick Time
    | SetLocation (Result Geolocation.Error Geolocation.Location)


init : ( Model, Cmd Msg )
init =
    ( Model 0 0 Dict.empty "" "" False
    , Task.attempt SetLocation Geolocation.now
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadPlanes lat lng ->
            if model.requestInFlight then
                ( model, Cmd.none )
            else
                ( { model | requestInFlight = True }
                , sendUpdateRequest model
                )

        PlaneResponse (Ok response) ->
            ( { model
                | planes = ADSB.mergePlaneDicts model.planes (ADSB.makePlaneDict response.aircraft)
                , lastDv = response.lastDv
                , requestInFlight = False
              }
            , Cmd.none
            )

        PlaneResponse (Err e) ->
            ( { model | error = toString e, requestInFlight = False }, Cmd.none )

        Tick t ->
            update (LoadPlanes model.latitude model.longitude) model

        SetLocation (Ok location) ->
            ( { model | latitude = location.latitude, longitude = location.longitude, requestInFlight = True }
            , sendUpdateRequest model
            )

        SetLocation (Err err) ->
            ( { model | error = toString err }
            , Cmd.none
            )


sendUpdateRequest : Model -> Cmd Msg
sendUpdateRequest model =
    Http.send PlaneResponse
        (ADSB.updateForLatLong
            model.latitude
            model.longitude
            model.lastDv
            (ADSB.icaosFromDict model.planes)
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.latitude /= 0 then
        Time.every (5 * Time.second) Tick
    else
        Sub.none


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text ("(" ++ (toString model.latitude) ++ "," ++ (toString model.longitude) ++ ")") ]
        , p [] [ text ("lastDv: " ++ model.lastDv) ]
        , p []
            [ text
                (if model.requestInFlight then
                    "request in flight... "
                 else
                    " "
                )
            ]
        , p [] [ text model.error ]
        , pre [] [text (toString (Dict.values model.planes))]
        , ul []
            (List.map planeEntry
                (List.filter
                    (\p -> (Maybe.withDefault 1000 p.altitude) < 1000)
                    (Dict.values model.planes)
                )
            )
        ]


planeEntry : ADSB.Aircraft -> Html Msg
planeEntry plane =
    li []
        [ text ("id: " ++ (toString plane.id))
        , text (", registration: " ++ (Maybe.withDefault "unknown" plane.registration))
        , text
            (", altitude: "
                ++ case plane.altitude of
                    Nothing ->
                        "unknown"

                    Just alt ->
                        toString alt
            )
        , text
            (case plane.standardAltitude of
                Nothing ->
                    ""

                Just alt ->
                    " (" ++ toString alt ++ ")"
            )
        , text
            (case plane.verticalSpeed of
                Nothing ->
                    ""

                Just vspd ->
                    " (" ++ toString vspd ++ " fpm)"
            )
        , text
            (", position: "
                ++ case ADSB.latLng plane of
                    Nothing ->
                        "unknown"

                    Just latlng ->
                        toString latlng
            )
        , span
            [ style
                [ ( "transform", "rotate(" ++ (toString (Maybe.withDefault 0 plane.heading)) ++ "deg)" )
                , ( "display", "inline-block" )
                , ( "width", "1em" )
                ]
            ]
            [ text "^" ]
        ]
