module MainTest exposing (all)

import Expect
import Main
import ProgramTest exposing (ProgramTest)
import SimulatedEffect.Cmd
import SimulatedEffect.Http
import Test exposing (..)


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
                    |> ProgramTest.expectHttpRequest
                        "POST"
                        "https://elm-graphql.herokuapp.com/graphql"
                        (.body >> Expect.equal """{"value":1}""")
        ]


simulateEffects : Main.Effect -> ProgramTest.SimulatedEffect Main.Msg
simulateEffects effect =
    case effect of
        Main.NoEffect ->
            SimulatedEffect.Cmd.none

        Main.GraphqlRequest _ query ->
            SimulatedEffect.Http.post
                { url = "https://elm-graphql.herokuapp.com/graphql"
                , body = SimulatedEffect.Http.stringBody "application/json" query
                , expect = SimulatedEffect.Http.expectString Main.GotStringResponse
                }



--
-- Main.ChangeLight { url, onResult, decoder, body } ->
--     SimulatedEffect.Http.post
--         { url = url
--         , body = SimulatedEffect.Http.jsonBody body
--         , expect = SimulatedEffect.Http.expectJson onResult decoder
--         }
