module MainTest exposing (all)

import Expect
import HomeAutomationExample as Main
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
        [ test "controlling a light" <|
            \() ->
                start
                    |> ProgramTest.simulateHttpOk
                        "GET"
                        "https://elm-graphql.herokuapp.com"
                        """[{"id":"K001", "name":"Kitchen", "dimmable":false, "value":0}]"""
                    |> ProgramTest.clickButton "Turn on"
                    |> ProgramTest.expectHttpRequest
                        "POST"
                        "http://localhost:8003/lighting_service/v1/devices/K001"
                        (.body >> Expect.equal """{"value":1}""")
        ]


simulateEffects : Main.Effect -> ProgramTest.SimulatedEffect Main.Msg
simulateEffects effect =
    case effect of
        Main.NoEffect ->
            SimulatedEffect.Cmd.none

        Main.GraphqlRequest ->
            SimulatedEffect.Http.get
                { url = "https://elm-graphql.herokuapp.com"
                , expect = SimulatedEffect.Http.expectJson onResult decoder
                }



--
-- Main.ChangeLight { url, onResult, decoder, body } ->
--     SimulatedEffect.Http.post
--         { url = url
--         , body = SimulatedEffect.Http.jsonBody body
--         , expect = SimulatedEffect.Http.expectJson onResult decoder
--         }
