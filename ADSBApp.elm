module Main exposing (..)

import Dict exposing (Dict(..))
import Time exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onMouseOver, onMouseOut, onClick)
import Http
import Task
import Geolocation
import ADSB


defaultLocation : ( Float, Float )
defaultLocation =
    ( 40.6355622, -73.7923178 )


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { latLng : ( Float, Float )
    , planes : Dict Int ADSB.Aircraft
    , selectedPlane : Maybe ADSB.Aircraft
    , lastDv : String
    , error : String
    , requestInFlight : Bool
    }


type Msg
    = LoadPlanes
    | PlaneResponse (Result Http.Error ADSB.Response)
    | Tick Time
    | SetLocation (Result Geolocation.Error Geolocation.Location)
    | Select (Maybe ADSB.Aircraft)


init : ( Model, Cmd Msg )
init =
    ( Model ( 0, 0 ) Dict.empty Nothing "" "" False
    , Task.attempt SetLocation Geolocation.now
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadPlanes ->
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
            update (LoadPlanes) model

        SetLocation (Ok location) ->
            let
                m =
                    { model | latLng = ( location.latitude, location.longitude ), requestInFlight = True }
            in
                ( m, sendUpdateRequest m )

        SetLocation (Err err) ->
            let
                m =
                    { model | latLng = defaultLocation, error = toString err, requestInFlight = True }
            in
                ( m, sendUpdateRequest m )

        Select aircraft ->
            ( { model | selectedPlane = aircraft }, Cmd.none )


sendUpdateRequest : Model -> Cmd Msg
sendUpdateRequest model =
    Http.send PlaneResponse
        (ADSB.updateForLatLong
            model.latLng
            model.lastDv
            (ADSB.icaosFromDict model.planes)
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.latLng /= ( 0, 0 ) then
        Time.every (5 * Time.second) Tick
    else
        Sub.none


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "margin", "1em" )
            ]
        ]
        [ h2 [] [ text (toString model.latLng) ]
        , p [] [ text ("lastDv: " ++ model.lastDv) ]
        , case model.selectedPlane of
            Nothing ->
                p [] [ text " " ]

            Just plane ->
                planeEntry plane p
        , p []
            [ text
                (if model.requestInFlight then
                    "request in flight... "
                 else
                    " "
                )
            ]
        , p [] [ text model.error ]
        , p [] [ text ("bounds: " ++ toString (bounds (Dict.values model.planes))) ]
        , planeWidget (Dict.values model.planes)
        ]


bounds : List ADSB.Aircraft -> ( ( Float, Float ), ( Float, Float ) )
bounds planes =
    let
        lats =
            List.filterMap (\a -> a.latitude) planes

        lngs =
            List.filterMap (\a -> a.longitude) planes

        minLat =
            Maybe.withDefault 99 (List.minimum lats)

        maxLat =
            Maybe.withDefault 99 (List.maximum lats)

        minLng =
            Maybe.withDefault 99 (List.minimum lngs)

        maxLng =
            Maybe.withDefault 99 (List.maximum lngs)
    in
        ( ( minLat, minLng ), ( maxLat, maxLng ) )


pos : ( ( Float, Float ), ( Float, Float ) ) -> ( Float, Float ) -> ( Float, Float )
pos ( ( minLat, minLng ), ( maxLat, maxLng ) ) ( lat, lng ) =
    let
        h =
            maxLat - minLat

        w =
            maxLng - minLng
    in
        ( (lat - minLat) / h, (lng - minLng) / w )


planeWidget : List ADSB.Aircraft -> Html Msg
planeWidget planes =
    let
        bbox =
            bounds planes
    in
        div
            [ style
                [ ( "width", "500px" )
                , ( "height", "500px" )
                , ( "position", "relative" )
                ]
            ]
            (List.map
                (\plane ->
                    let
                        ( y, x ) =
                            pos bbox (Maybe.withDefault ( 0, 0 ) (ADSB.latLng plane))
                    in
                        div
                            [ style
                                [ ( "position", "absolute" )
                                , ( "top", (toString (500 - (y * 500))) ++ "px" )
                                , ( "left", (toString (x * 500)) ++ "px" )
                                , ( "transform", "rotate(" ++ (toString ((Maybe.withDefault 0 plane.heading) - 90)) ++ "deg)" )
                                , ( "font-family", "Arial Unicode MS" )
                                ]
                            , Html.Attributes.title ("reg: " ++ (Maybe.withDefault "unknown" plane.registration))
                            , onClick (Select (Just plane))
                            ]
                            [ text "✈" ]
                )
                planes
            )


planeEntry : ADSB.Aircraft -> (List (Html.Attribute Msg) -> List (Html.Html Msg) -> Html Msg) -> Html Msg
planeEntry plane htmlTag =
    htmlTag []
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
        , text
            ("operator: " ++ (Maybe.withDefault "unknown" plane.operator))
        ]
