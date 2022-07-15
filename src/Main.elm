module Main exposing (Msg, OuterModel, main)

import Browser
import Browser.Dom
import Browser.Events
import Color
import Html exposing (Html)
import List.Extra
import PixelEngine exposing (Area, Input(..))
import PixelEngine.Options as Options exposing (Options)
import PixelEngine.Tile as Tile exposing (Tile, Tileset)
import Random exposing (Generator, Seed)
import Task



{------------------------
    TYPES
------------------------}


type alias Size =
    { width : Int
    , height : Int
    }


type OuterModel
    = Initializing
    | WaitingSize Seed
    | WaitingSeed Size
    | Playing InnerModel


type alias InnerModel =
    { seed : Seed
    , size : Size
    , player :
        { position : Position
        , health : Int
        , hasHit : Maybe String
        }
    , entities : List Entity
    }


type alias Entity =
    { name : String
    , position : Position
    , data : EntityData
    }


type EntityData
    = Bat { health : Int }
    | Rat { health : Int }


type alias Position =
    ( Int, Int )


type Direction
    = Left
    | Right
    | Up
    | Down


type Msg
    = Move (Maybe Direction)
    | SetSeed Seed
    | SetSize Size
    | Nop



{------------------------
   GLOBAL VARIABLES
------------------------}


boardSize : number
boardSize =
    16


tileSize : number
tileSize =
    16



{------------------------
    INIT
------------------------}


init : () -> ( OuterModel, Cmd Msg )
init _ =
    ( Initializing
    , Cmd.batch
        [ Random.generate SetSeed Random.independentSeed
        , Task.perform
            (\{ viewport } ->
                SetSize
                    { width = floor viewport.width
                    , height = floor viewport.height
                    }
            )
            Browser.Dom.getViewport
        ]
    )


innerInit : Seed -> Size -> InnerModel
innerInit seed size =
    let
        player :
            { position : Position
            , health : number
            , hasHit : Maybe String
            }
        player =
            { position = ( 0, 0 )
            , health = 10
            , hasHit = Nothing
            }

        ( entities, newSeed ) =
            Random.step entitiesGenerator seed
    in
    { seed = newSeed
    , size = size
    , player = player
    , entities =
        List.filter
            (\{ position } ->
                (position /= player.position)
                    && (position /= ( boardSize - 1, boardSize - 1 ))
            )
            entities
    }



{------------------------
    UPDATE
------------------------}


update : Msg -> OuterModel -> ( OuterModel, Cmd Msg )
update msg outerModel =
    ( case ( msg, outerModel ) of
        ( Nop, _ ) ->
            outerModel

        ( SetSeed seed, Initializing ) ->
            WaitingSize seed

        ( SetSize size, Initializing ) ->
            WaitingSeed size

        ( _, Initializing ) ->
            outerModel

        ( SetSize size, WaitingSeed _ ) ->
            WaitingSeed size

        ( SetSeed seed, WaitingSeed size ) ->
            Playing <| innerInit seed size

        ( _, WaitingSeed _ ) ->
            outerModel

        ( SetSize size, WaitingSize seed ) ->
            Playing <| innerInit seed size

        ( _, WaitingSize _ ) ->
            outerModel

        ( SetSize size, Playing innerModel ) ->
            Playing { innerModel | size = size }

        ( SetSeed _, Playing _ ) ->
            outerModel

        ( Move maybeDirection, Playing ({ player } as innerModel) ) ->
            if player.position == ( boardSize - 1, boardSize - 1 ) || player.health <= 0 then
                outerModel

            else
                innerModel
                    |> stepEnemies
                    |> movePlayer maybeDirection
                    |> Playing
    , Cmd.none
    )


movePlayer : Maybe Direction -> InnerModel -> InnerModel
movePlayer maybeDirection ({ player } as innerModel) =
    case moveResult maybeDirection innerModel of
        OutOfRange ->
            innerModel

        Moved moved entity ->
            let
                ( damage, hasHit, entityName ) =
                    case entity of
                        Nothing ->
                            ( 0, Nothing, Nothing )

                        Just en ->
                            case en.data of
                                Bat _ ->
                                    ( 1, Just "Bat", Just en.name )

                                Rat _ ->
                                    ( 2, Just "Rat", Just en.name )
            in
            { innerModel
                | player =
                    { player
                        | position = moved
                        , health = player.health - damage
                        , hasHit = hasHit
                    }
                , entities =
                    List.Extra.updateIf
                        (\{ name } -> Just name == entityName)
                        damageEntity
                        innerModel.entities
            }


damageEntity : Entity -> Entity
damageEntity entity =
    { entity
        | data =
            case entity.data of
                Bat { health } ->
                    Bat { health = max 0 <| health - 1 }

                Rat { health } ->
                    Rat { health = max 0 <| health - 1 }
    }


stepEnemies : InnerModel -> InnerModel
stepEnemies innerModel =
    let
        ( entities, newSeed ) =
            innerModel.entities
                |> List.foldr
                    (\entity ( acc, seed ) ->
                        if isAlive entity.data then
                            let
                                generator =
                                    case entity.data of
                                        Bat _ ->
                                            randomDirectionLegalFrom entity.position
                                                |> Random.map
                                                    (\direction ->
                                                        { entity | position = move direction entity.position }
                                                    )

                                        Rat { health } ->
                                            if health == 1 then
                                                Random.constant entity

                                            else
                                                randomDirectionLegalFrom entity.position
                                                    |> Random.map
                                                        (\direction ->
                                                            { entity | position = move direction entity.position }
                                                        )

                                ( entity_, seed_ ) =
                                    Random.step generator seed
                            in
                            ( entity_ :: acc, seed_ )

                        else
                            ( entity :: acc, seed )
                    )
                    ( [], innerModel.seed )
    in
    { innerModel | entities = entities, seed = newSeed }


randomDirectionLegalFrom : Position -> Generator Direction
randomDirectionLegalFrom ( x, y ) =
    let
        bound condition value =
            if condition then
                Just value

            else
                Nothing

        list =
            List.filterMap identity
                [ bound (x > 0) Left
                , bound (x < boardSize - 1) Right
                , bound (y > 0) Up
                , bound (y < boardSize - 1) Down
                ]
    in
    case list of
        [] ->
            Random.constant Down

        h :: t ->
            Random.uniform h t


type MoveResult
    = OutOfRange
    | Moved Position (Maybe Entity)


moveResult : Maybe Direction -> InnerModel -> MoveResult
moveResult maybeDirection ({ player } as innerModel) =
    let
        (( x, y ) as moved) =
            case maybeDirection of
                Just dir ->
                    move dir player.position

                Nothing ->
                    player.position
    in
    if x < 0 || x >= boardSize || y < 0 || y >= boardSize then
        OutOfRange

    else
        let
            entity =
                List.Extra.find
                    (\{ position, data } ->
                        position == moved && isAlive data
                    )
                    innerModel.entities
        in
        Moved moved entity


isAlive : EntityData -> Bool
isAlive data =
    case data of
        Bat { health } ->
            health > 0

        Rat { health } ->
            health > 0


move : Direction -> Position -> Position
move direction ( x, y ) =
    case direction of
        Up ->
            ( x, y - 1 )

        Left ->
            ( x - 1, y )

        Right ->
            ( x + 1, y )

        Down ->
            ( x, y + 1 )


entitiesGenerator :
    Generator
        (List
            { position : ( Int, Int )
            , name : String
            , data : EntityData
            }
        )
entitiesGenerator =
    let
        randoms =
            Random.map2
                (\position data ->
                    { position = position
                    , data = data
                    }
                )
                randomPosition
                randomEntity
                |> Random.list 30

        rat =
            Random.map2
                (\position data ->
                    { position = position
                    , data = data
                    }
                )
                randomPosition
                randomRat
    in
    Random.map2 (::) rat randoms
        |> Random.map
            (List.indexedMap
                (\i { position, data } ->
                    { position = position
                    , data = data
                    , name = "Enemy " ++ String.fromInt i
                    }
                )
            )


randomPosition : Generator Position
randomPosition =
    Random.map2 Tuple.pair
        (Random.int 0 (boardSize - 1))
        (Random.int 0 (boardSize - 1))


randomEntity : Generator EntityData
randomEntity =
    Random.weighted ( 5, randomBat ) [ ( 1, randomRat ) ]
        |> Random.andThen identity


randomBat : Generator EntityData
randomBat =
    Random.constant (Bat { health = 1 })


randomRat : Generator EntityData
randomRat =
    Random.map (\health -> Rat { health = health }) (Random.int 1 2)


tilePositionFromIndex : Int -> Position
tilePositionFromIndex index =
    ( modBy 12 index, index // 12 )



{------------------------
    SUBSCRIPTIONS
------------------------}


subscriptions : OuterModel -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Sub.map (Maybe.withDefault Nop) <|
            PixelEngine.basicControls controls
        , Browser.Events.onResize
            (\w h ->
                SetSize
                    { width = w
                    , height = h
                    }
            )
        ]



{------------------------
    CONTROLS
------------------------}


controls : Input -> Maybe Msg
controls input =
    case input of
        InputLeft ->
            Just <| Move <| Just Left

        InputRight ->
            Just <| Move <| Just Right

        InputUp ->
            Just <| Move <| Just Up

        InputDown ->
            Just <| Move <| Just Down

        InputA ->
            Just <| Move Nothing

        _ ->
            Nothing



{------------------------
    VIEW
------------------------}


areas : InnerModel -> List (Area Msg)
areas ({ player } as innerModel) =
    let
        background : List ( Position, Tile msg )
        background =
            List.range 0 (boardSize - 1)
                |> List.concatMap
                    (\x ->
                        List.range 0 (boardSize - 1)
                            |> List.map
                                (\y ->
                                    ( ( x, y )
                                    , Tile.fromPosition
                                        (tilePositionFromIndex
                                            (if (Tuple.first <| Random.step (Random.int 0 10) (Random.initialSeed <| x + y * boardSize)) < 4 then
                                                49

                                             else
                                                48
                                            )
                                        )
                                    )
                                )
                    )

        doorPosition : Position
        doorPosition =
            ( boardSize - 1, boardSize - 1 )

        door : ( Position, Tile msg )
        door =
            ( doorPosition, Tile.fromPosition (tilePositionFromIndex 45) )

        ( messageTop, messageBottom ) =
            if player.position == doorPosition then
                ( "", "You won!" )

            else if player.health <= 0 then
                ( "", "You died!" )

            else
                let
                    format label value =
                        label ++ String.padLeft (boardSize - String.length label) ' ' value
                in
                ( case
                    player.hasHit
                  of
                    Nothing ->
                        "Get to the door"

                    Just name ->
                        "Hit a " ++ name ++ "!"
                , format "Health" <| String.fromInt player.health
                )

        berlin : Tileset
        berlin =
            { source = "berlin.png"
            , spriteWidth = tileSize
            , spriteHeight = tileSize
            }

        tinyDungeon : Tileset
        tinyDungeon =
            { source = "kenney_tinydungeon.png"
            , spriteWidth = tileSize
            , spriteHeight = tileSize
            }
    in
    [ tilesFromText ( 0, 0 ) messageTop
        |> PixelEngine.tiledArea
            { rows = 1
            , tileset = berlin
            , background =
                PixelEngine.colorBackground <|
                    if String.isEmpty messageTop then
                        Color.black

                    else
                        Color.white
            }
    , tilesFromText ( 0, 0 ) messageBottom
        |> PixelEngine.tiledArea
            { rows = 1
            , tileset = berlin
            , background =
                PixelEngine.colorBackground <|
                    if String.isEmpty messageBottom then
                        Color.black

                    else
                        Color.white
            }
    , (background
        ++ door
        :: playerToTile player
        :: List.map entityToTile innerModel.entities
      )
        |> PixelEngine.tiledArea
            { rows = boardSize
            , tileset = tinyDungeon
            , background = PixelEngine.colorBackground Color.black
            }
    ]


textLines : number
textLines =
    2


tilesFromText : ( Int, Int ) -> String -> List ( ( Int, Int ), Tile msg )
tilesFromText ( dx, dy ) text =
    text
        |> String.toList
        |> List.indexedMap
            (\i char ->
                let
                    code =
                        Char.toCode char - 0x20
                in
                ( ( dx + i, dy ), Tile.fromPosition ( modBy 16 code, code // 16 ) )
            )


playerToTile :
    { a
        | position : Position
        , health : Int
    }
    -> ( Position, Tile msg )
playerToTile { position, health } =
    ( position
    , Tile.movable "player" <|
        Tile.fromPosition <|
            tilePositionFromIndex <|
                if health > 0 then
                    99

                else
                    121
    )


entityToTile : { name : String, position : Position, data : EntityData } -> ( Position, Tile msg )
entityToTile { name, position, data } =
    if isAlive data then
        let
            index =
                case data of
                    Bat _ ->
                        120

                    Rat { health } ->
                        if health == 1 then
                            124

                        else
                            123
        in
        ( position, Tile.movable name <| Tile.fromPosition <| tilePositionFromIndex index )

    else
        ( position, Tile.multipleTiles [] )



{------------------------
    CONFIGURATION
------------------------}


options : Int -> Options Msg
options scale =
    Options.default
        |> Options.withMovementSpeed 0.2
        |> Options.withScale scale


view :
    OuterModel
    -> { title : String, body : List (Html Msg) }
view outerModel =
    { title = "Roll2Die"
    , body =
        case outerModel of
            Playing innerModel ->
                let
                    scale : Int
                    scale =
                        floor <|
                            min
                                (toFloat (innerModel.size.width - 2 * margin)
                                    / (boardSize * tileSize)
                                )
                                (toFloat (innerModel.size.height - 2 * margin)
                                    / ((boardSize + textLines) * tileSize)
                                )

                    margin : number
                    margin =
                        20
                in
                [ PixelEngine.toHtml
                    { width = boardSize * tileSize
                    , options = Just (options scale)
                    }
                    (areas innerModel)
                ]

            _ ->
                [ Html.text "Initializing..." ]
    }


main : Program () OuterModel Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
