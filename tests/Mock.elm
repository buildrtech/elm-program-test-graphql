module Mock exposing (..)

import Graphql.Document.Field
import Graphql.RawField as RawField exposing (RawField)
import Graphql.SelectionSet exposing (SelectionSet(..))
import Json.Encode as Encode


type Mock
    = Mock (List Field)


type alias Field =
    { name : String, value : String }


init =
    Mock []


hello value mock =
    addField "hello" value mock


addField name value (Mock fields) =
    Mock <|
        { name = name, value = value }
            :: fields


today value mock =
    addField "today" value mock


response selectionSet (Mock mockFields) =
    if mockFields |> List.isEmpty then
        Err
            { missingFields = [ "hello" ]
            }

    else
        Encode.object
            [ ( "data"
              , mockedValues selectionSet (Mock mockFields)
              )
            ]
            |> Encode.encode 0
            |> Ok


mockedValues : SelectionSet a b -> Mock -> Encode.Value
mockedValues (SelectionSet rawFields decoder) mock =
    rawFields
        |> Debug.log "rawFields"
        |> List.map (mockedField mock)
        |> Encode.object


mockedField : Mock -> RawField -> ( String, Encode.Value )
mockedField (Mock fields) rawField =
    let
        maybeValue =
            fields
                |> List.filter
                    (\field ->
                        case rawField of
                            RawField.Leaf { fieldName } args ->
                                field.name == fieldName

                            _ ->
                                Debug.todo ""
                    )
                |> List.head
                |> Maybe.map .value
    in
    case maybeValue of
        Just value ->
            ( Graphql.Document.Field.hashedAliasName rawField, Encode.string value )

        Nothing ->
            Debug.todo ""


type alias Error =
    { missingFields : List String }
