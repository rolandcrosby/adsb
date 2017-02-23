module ADSB exposing (..)

import Dict exposing (Dict(..))
import Http
import Json.Decode as Decode exposing (int, string, float, list, nullable, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)


prodUrl =
    "/proxy"

type alias Aircraft =
    { id : Int
    , icao : Maybe String
    , registration : Maybe String
    , standardAltitude : Maybe Int
    , altitude : Maybe Int
    , latitude : Maybe Float
    , longitude : Maybe Float
    , operator : Maybe String
    , operatorIcao : Maybe String
    , verticalSpeed : Maybe Int
    , heading : Maybe Float
    }

aircraftDecoder : Decoder Aircraft
aircraftDecoder =
    decode Aircraft
        |> required "Id" int
        |> optional "Icao" (nullable string) Nothing
        |> optional "Reg" (nullable string) Nothing
        |> optional "Alt" (nullable int) Nothing
        |> optional "GAlt" (nullable int) Nothing
        |> optional "Lat" (nullable float) Nothing
        |> optional "Long" (nullable float) Nothing
        |> optional "Op" (nullable string) Nothing
        |> optional "OpCode" (nullable string) Nothing
        |> optional "Vsi" (nullable int) Nothing
        |> optional "Trak" (nullable float) Nothing

type alias Response =
    { aircraft : List Aircraft
    , lastDv : String
    }

responseDecoder : Decoder Response
responseDecoder =
    decode Response
        |> required "acList" (list aircraftDecoder)
        |> required "lastDv" string

latLng : Aircraft -> Maybe ( Float, Float )
latLng aircraft =
    case aircraft.latitude of
        Nothing ->
            Nothing

        Just lat ->
            case aircraft.longitude of
                Nothing ->
                    Nothing

                Just lng ->
                    Just ( lat, lng )


mergeField : Maybe a -> Maybe a -> Maybe a
mergeField old new =
    case old of
        Nothing ->
            new

        Just _ ->
            case new of
                Nothing ->
                    old

                Just _ ->
                    new


mergeAircraft : Aircraft -> Aircraft -> Aircraft
mergeAircraft old new =
    Aircraft
        old.id
        (mergeField old.icao new.icao)
        (mergeField old.registration new.registration)
        (mergeField old.standardAltitude new.standardAltitude)
        (mergeField old.altitude new.altitude)
        (mergeField old.latitude new.latitude)
        (mergeField old.longitude new.longitude)
        (mergeField old.operator new.operator)
        (mergeField old.operatorIcao new.operatorIcao)
        (mergeField old.verticalSpeed new.verticalSpeed)
        (mergeField old.heading new.heading)


makePlaneDict : List Aircraft -> Dict Int Aircraft
makePlaneDict planes =
    Dict.fromList
        (List.map (\p -> ( p.id, p )) planes)


mergePlaneDicts : Dict Int Aircraft -> Dict Int Aircraft -> Dict Int Aircraft
mergePlaneDicts oldDict newDict =
    Dict.merge
        Dict.insert
        (\k va vb d -> Dict.insert k (mergeAircraft va vb) d)
        Dict.insert
        oldDict
        newDict
        Dict.empty



icaosFromDict : Dict comparable Aircraft -> List String
icaosFromDict acDict =
    Dict.values acDict
    |> List.filterMap (\a -> a.icao)

updatePlaneList : List ( String, String ) -> List String -> Http.Request Response
updatePlaneList params icaos =
    let
        query =
            String.join
                "&"
                (List.map
                    (\( a, b ) -> Http.encodeUri a ++ "=" ++ Http.encodeUri b)
                    params
                )

        icaoBody =
            "icaos=" ++ (String.join "-" icaos)
    in
        Http.post
            (prodUrl ++ "?" ++ query)
            (Http.stringBody "application/x-www-form-urlencoded" icaoBody)
            responseDecoder


updateForLatLong : (Float, Float) -> String -> List String -> Http.Request Response
updateForLatLong (lat, lng) lastDv icaos =
    updatePlaneList
        [ ( "lat", toString lat )
        , ( "lng", toString lng )
        , ( "ldv", lastDv )
        , ( "fDstL", "0" )
        , ( "fDstU", "100" )
        ]
        icaos