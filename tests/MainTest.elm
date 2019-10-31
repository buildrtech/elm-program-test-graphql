module MainTest exposing (all)

import Expect
import Http
import Json.Decode as Decode
import Main
import ProgramTest exposing (ProgramTest)
import SimulatedEffect.Cmd
import SimulatedEffect.Http
import Test exposing (..)
import Test.Html.Selector exposing (text)


start : ProgramTest Main.Model Main.Msg Main.Effect
start =
    ProgramTest.createDocument
        { init = Main.init
        , update = Main.update
        , view = Main.view
        }
        |> ProgramTest.withSimulatedEffects simulateEffects
        |> ProgramTest.start ()


all : Test
all =
    describe "Graphql Requests"
        [ test "string request" <|
            \() ->
                start
                    |> ProgramTest.simulateHttpOk
                        "POST"
                        "https://elm-graphql.herokuapp.com/graphql?index=0"
                        """{ "data": { "hello3832528868": "example" }}"""
                    |> ProgramTest.simulateHttpOk
                        "POST"
                        "https://elm-graphql.herokuapp.com/graphql?index=1"
                        """{ "data": { "today3832528868": "example" }}"""
                    |> ProgramTest.expectViewHas [ text "example", text "7" ]
        ]


simulateEffects : Main.Effect -> ProgramTest.SimulatedEffect Main.Msg
simulateEffects effect =
    case effect of
        Main.Batch effects ->
            effects
                |> List.map simulateEffects
                |> SimulatedEffect.Cmd.batch

        Main.NoEffect ->
            SimulatedEffect.Cmd.none

        Main.GraphqlRequest index responseToMsg query ->
            SimulatedEffect.Http.post
                { url = "https://elm-graphql.herokuapp.com/graphql?index=" ++ String.fromInt index
                , body = SimulatedEffect.Http.stringBody "application/json" query
                , expect = SimulatedEffect.Http.expectString responseToMsg
                }
