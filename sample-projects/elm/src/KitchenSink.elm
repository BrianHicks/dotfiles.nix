port module KitchenSink exposing (..)

import Html
import Html.Styled.Attributes as Attrs
    exposing
        ( css
        , href
        )


type Id
    = Id String


idString : Id -> String
idString (Id id) =
    id


idStringCase : Id -> String
idStringCase id =
    case id of
        Id id_ ->
            id_


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

someCoolRecord : { foo : Int, bar : Int }
someCoolRecord =
    { foo = 1
    , bar = 2
    }
