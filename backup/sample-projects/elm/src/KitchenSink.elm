port module KitchenSink exposing (..)


type Id
    = Id String


idString : Id -> String
idString (Id id) =
    id


double : Int -> Int
double half =
    half * 2


add : Int -> Int -> Int
add a b =
    a + b


deconstructions : String
deconstructions =
    let
        ( a, b ) =
            ( 1, "yeah" )

        nah =
            "nope"
    in
    b


port outgoingPort : Int -> Cmd msg


port incomingPort : Sub Int
