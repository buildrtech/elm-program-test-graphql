-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Swapi.Enum.Episode exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| One of the films in the Star Wars Trilogy

  - Empire - Released in 1980.
  - Jedi - Released in 1983.
  - Newhope - Released in 1977.

-}
type Episode
    = Empire
    | Jedi
    | Newhope


list : List Episode
list =
    [ Empire, Jedi, Newhope ]


decoder : Decoder Episode
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "EMPIRE" ->
                        Decode.succeed Empire

                    "JEDI" ->
                        Decode.succeed Jedi

                    "NEWHOPE" ->
                        Decode.succeed Newhope

                    _ ->
                        Decode.fail ("Invalid Episode type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : Episode -> String
toString enum =
    case enum of
        Empire ->
            "EMPIRE"

        Jedi ->
            "JEDI"

        Newhope ->
            "NEWHOPE"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe Episode
fromString enumString =
    case enumString of
        "EMPIRE" ->
            Just Empire

        "JEDI" ->
            Just Jedi

        "NEWHOPE" ->
            Just Newhope

        _ ->
            Nothing