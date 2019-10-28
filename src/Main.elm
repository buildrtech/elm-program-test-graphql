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
        [ graphqlEffect Query.hello (\string -> GotStringResponse <| Ok string)
        , graphqlEffect
            (Query.today |> SelectionSet.map String.length)
            (\int -> GotIntResponse <| Ok int)
        ]
    )


type Effect
    = Batch (List Effect)
    | GraphqlRequest (Decode.Decoder Msg) String
    | NoEffect


batch : List Effect -> Effect
batch effects =
    Batch effects


graphqlEffect : SelectionSet decodesTo RootQuery -> (decodesTo -> Msg) -> Effect
graphqlEffect selectionSet toMsg =
    GraphqlRequest
        (selectionSet
            |> Graphql.Document.decoder
            |> Decode.map toMsg
        )
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

        GraphqlRequest decoder query ->
            Http.post
                { url = "https://elm-graphql.herokuapp.com"
                , body = Http.stringBody "application/json" query
                , expect = Http.expectString GotStringResponse
                }



-- UPDATE


type Msg
    = GotIntResponse (Result Http.Error Int)
    | GotStringResponse (Result Http.Error String)



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
