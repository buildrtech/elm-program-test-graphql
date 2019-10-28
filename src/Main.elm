module Main exposing (Effect(..), Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Dict exposing (Dict)
import Graphql.Document
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Encode
import RemoteData exposing (RemoteData)
import Swapi.Object
import Swapi.Query as Query
import Url exposing (Url)


type alias Model =
    ()


initialModel : Model
initialModel =
    ()


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    Browser.document
        { init =
            \flags ->
                init flags
                    |> Tuple.mapSecond perform
        , update =
            \msg model ->
                update msg model
                    |> Tuple.mapSecond perform
        , subscriptions = subscriptions
        , view = view
        }


init : Flags -> ( Model, Effect )
init () =
    ( initialModel
    , batch
        [ graphqlEffect
            0
            Query.hello
            GotStringResponse
        , graphqlEffect
            1
            (Query.today |> SelectionSet.map String.length)
            GotIntResponse
        ]
    )


type Effect
    = Batch (List Effect)
    | GraphqlRequest Int (Result Http.Error String -> Msg) String
    | NoEffect


batch : List Effect -> Effect
batch effects =
    Batch effects


graphqlEffect : Int -> SelectionSet decodesTo RootQuery -> (RemoteData Http.Error decodesTo -> Msg) -> Effect
graphqlEffect index selectionSet toMsg =
    let
        decoder =
            selectionSet
                |> Graphql.Document.decoder
    in
    GraphqlRequest
        index
        (decoderToResultToMsg decoder toMsg)
        (Graphql.Document.serializeQuery selectionSet)


perform : Effect -> Cmd Msg
perform effect =
    case effect of
        Batch effects ->
            effects
                |> List.map perform
                |> Cmd.batch

        NoEffect ->
            Cmd.none

        GraphqlRequest index responseToMsg query ->
            Http.post
                { url = "https://elm-graphql.herokuapp.com/graphql"
                , body = Http.stringBody "application/json" query
                , expect = Http.expectString responseToMsg
                }


decoderToResultToMsg :
    Decode.Decoder decodesTo
    -> (RemoteData Http.Error decodesTo -> Msg)
    -> Result Http.Error String
    -> Msg
decoderToResultToMsg decoder toMsg response =
    response
        |> Result.andThen
            (\responseBody ->
                responseBody
                    |> Decode.decodeString decoder
                    |> Result.mapError (\_ -> Http.BadBody "Failed to parse")
            )
        |> RemoteData.fromResult
        |> toMsg



-- UPDATE


type Msg
    = GotIntResponse (RemoteData Http.Error Int)
    | GotStringResponse (RemoteData Http.Error String)



-- = GotStringResponse (RemoteData (Graphql.Http.Error String) String)


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case Debug.log "MSG" msg of
        GotIntResponse response ->
            ( model, NoEffect )

        GotStringResponse response ->
            ( model, NoEffect )


view : Model -> Browser.Document Msg
view model =
    { title = "Graphql Test"
    , body = []
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
