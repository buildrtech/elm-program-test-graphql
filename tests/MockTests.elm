module MockTests exposing (suite)

import Expect
import Mock
import Swapi.Query
import Test exposing (describe, test)


suite =
    describe "mocks"
        [ test "empty" <|
            \_ ->
                Mock.init
                    |> Mock.response Swapi.Query.hello
                    |> Expect.equal (Err { missingFields = [ "hello" ] })
        , test "simple string field" <|
            \_ ->
                Mock.init
                    |> Mock.hello "example"
                    |> Mock.response Swapi.Query.hello
                    |> Expect.equal
                        (Ok
                            """{"data":{"hello3832528868":"example"}}"""
                        )
        , test "multiple fields" <|
            \_ ->
                Mock.init
                    |> Mock.hello "example"
                    |> Mock.today "todayExample"
                    |> Mock.response Swapi.Query.hello
                    |> Expect.equal
                        (Ok
                            """{"data":{"hello3832528868":"example","today3832528868":"todayExample"}}"""
                        )
        ]
