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
    , graphqlEffect Query.hello (\_ -> GotStringResponse <| Ok "")
    )


type Effect
    = NoEffect
    | GraphqlRequest (Decode.Decoder Msg) String


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
    = GotStringResponse (Result Http.Error String)



-- = GotStringResponse (RemoteData (Graphql.Http.Error String) String)


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
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
