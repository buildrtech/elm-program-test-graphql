module MainTest exposing (all)

import Expect
import Http
import Json.Decode as Decode
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
                    |> ProgramTest.simulateHttpOk
                        "POST"
                        "https://elm-graphql.herokuapp.com/graphql"
                        """{ "data": { "today3832528868": "hi" }}"""
                    |> ProgramTest.simulateHttpOk
                        "POST"
                        "https://elm-graphql.herokuapp.com/graphql"
                        """{ "data": { "hello3832528868": "hi" }}"""
                    |> ProgramTest.done
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

        Main.GraphqlRequest decoder query ->
            SimulatedEffect.Http.post
                { url = "https://elm-graphql.herokuapp.com/graphql"
                , body = SimulatedEffect.Http.stringBody "application/json" query
                , expect = SimulatedEffect.Http.expectString <| decoderToMsg decoder
                }


decoderToMsg : Decode.Decoder msg -> Result Http.Error String -> msg
decoderToMsg decoder result =
    case result of
        Ok string ->
            case string |> Decode.decodeString decoder of
                Ok value ->
                    value

                Err error ->
                    Debug.todo (error |> Decode.errorToString)

        Err errorHttp ->
            Debug.todo <|
                Debug.toString errorHttp



--
-- Main.ChangeLight { url, onResult, decoder, body } ->
--     SimulatedEffect.Http.post
--         { url = url
--         , body = SimulatedEffect.Http.jsonBody body
--         , expect = SimulatedEffect.Http.expectJson onResult decoder
--         }
