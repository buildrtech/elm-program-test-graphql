module Mock exposing (..)

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

    else if List.length mockFields == 2 then
        Encode.object
            [ ( "data"
              , Encode.object
                    [ ( "hello3832528868", Encode.string "example" )
                    , ( "today3832528868", Encode.string "todayExample" )
                    ]
              )
            ]
            |> Encode.encode 0
            |> Ok

    else
        Encode.object
            [ ( "data"
              , Encode.object [ ( "hello3832528868", Encode.string "example" ) ]
              )
            ]
            |> Encode.encode 0
            |> Ok


type alias Error =
    { missingFields : List String }
