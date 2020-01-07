module MainTest exposing (all)

import Expect
import Graphql.Document
import Graphql.SelectionSet as SelectionSet
import Http
import Json.Decode as Decode
import Main
import Mock
import ProgramTest exposing (ProgramTest)
import SimulatedEffect.Cmd
import SimulatedEffect.Http
import Swapi.Query
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
                    |> expectQuery
                        "https://elm-graphql.herokuapp.com/graphql?index=0"
                        (SelectionSet.map2 Tuple.pair
                            Swapi.Query.hello
                            Swapi.Query.today
                        )
                    --|> ProgramTest.ensureHttpRequest
                    --    "POST"
                    --    "https://elm-graphql.herokuapp.com/graphql?index=0"
                    --    (.body >> Expect.equal (Swapi.Query.hello |> Graphql.Document.serializeQuery))
                    --|> ProgramTest.simulateHttpOk
                    --    "POST"
                    --    "https://elm-graphql.herokuapp.com/graphql?index=0"
                    --    (buildHelloResponse "example"
                    |> ProgramTest.ensureHttpRequest
                        "POST"
                        "https://elm-graphql.herokuapp.com/graphql?index=1"
                        (.body >> Expect.equal (Swapi.Query.today |> Graphql.Document.serializeQuery))
                    |> ProgramTest.simulateHttpOk
                        "POST"
                        "https://elm-graphql.herokuapp.com/graphql?index=1"
                        """{ "data": { "today3832528868": "example" }}"""
                    |> ProgramTest.expectViewHas [ text "example", text "7" ]
        ]


mockApi =
    Mock.init
        |> Mock.hello "example"
        |> Mock.today "example"


expectQuery url graphqlQuery program =
    program
        |> ProgramTest.ensureHttpRequest
            "POST"
            url
            (.body >> Expect.equal (graphqlQuery |> Graphql.Document.serializeQuery))
        |> ProgramTest.simulateHttpOk
            "POST"
            url
            (buildHelloResponse graphqlQuery "example")


buildHelloResponse graphqlQuery value =
    Mock.init
        |> Mock.hello "example"
        |> Mock.today "exampleToday"
        |> Mock.response graphqlQuery
        |> Result.withDefault ""


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
