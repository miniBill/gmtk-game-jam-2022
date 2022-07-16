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
import Process
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
    | WaitingPlayer InnerModel
    | WaitingEnemies InnerModel


type alias InnerModel =
    { seed : Seed
    , size : Size
    , player : Player
    , entities : List Entity
    , level : Int
    }


type alias Player =
    { position : Position
    , health : Int
    , hit : Maybe Hit
    , armed : Bool
    }


type alias Hit =
    ( Int, String )


type alias Entity =
    { name : String
    , position : Position
    , data : EntityData
    , intention : Maybe Direction
    }


type EntityData
    = Bat { health : Int }
    | Rat { health : Int }
    | Spider
    | Potion { health : Int }
    | Sord { picked : Bool, index : Int }


type alias Position =
    ( Int, Int )


type Direction
    = Left
    | Right
    | Up
    | Down


type Msg
    = Move (Maybe Direction)
    | MoveEnemies
    | SetSeed Seed
    | SetSize Size
    | Nop
    | NewLevel



{------------------------
   GLOBAL VARIABLES
------------------------}


boardSize : number
boardSize =
    20


tileSize : number
tileSize =
    16


doorPosition : Position
doorPosition =
    ( boardSize - 1, boardSize - 1 )


initialPlayerPosition : ( Int, Int )
initialPlayerPosition =
    ( 0, 0 )



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


innerInit : Seed -> Size -> Int -> InnerModel
innerInit seed size level =
    let
        player : Player
        player =
            { position = initialPlayerPosition
            , health = 5
            , hit = Nothing
            , armed = False
            }

        ( entities, newSeed ) =
            Random.step (entitiesGenerator level) seed
    in
    { seed = newSeed
    , size = size
    , player = player
    , entities = entities
    , level = level
    }
        |> addIntentionToEntities



{------------------------
    UPDATE
------------------------}


update : Msg -> OuterModel -> ( OuterModel, Cmd Msg )
update msg outerModel =
    case ( msg, outerModel ) of
        ( Nop, _ ) ->
            ( outerModel, Cmd.none )

        ( SetSeed seed, Initializing ) ->
            ( WaitingSize seed, Cmd.none )

        ( SetSize size, Initializing ) ->
            ( WaitingSeed size, Cmd.none )

        ( _, Initializing ) ->
            ( outerModel, Cmd.none )

        ( SetSize size, WaitingSeed _ ) ->
            ( WaitingSeed size, Cmd.none )

        ( SetSeed seed, WaitingSeed size ) ->
            ( WaitingPlayer <| innerInit seed size 1, Cmd.none )

        ( _, WaitingSeed _ ) ->
            ( outerModel, Cmd.none )

        ( SetSize size, WaitingSize seed ) ->
            ( WaitingPlayer <| innerInit seed size 1, Cmd.none )

        ( _, WaitingSize _ ) ->
            ( outerModel, Cmd.none )

        ( SetSize size, WaitingPlayer innerModel ) ->
            ( WaitingPlayer { innerModel | size = size }, Cmd.none )

        ( SetSize size, WaitingEnemies innerModel ) ->
            ( WaitingEnemies { innerModel | size = size }, Cmd.none )

        ( SetSeed _, WaitingPlayer _ ) ->
            ( outerModel, Cmd.none )

        ( SetSeed _, WaitingEnemies _ ) ->
            ( outerModel, Cmd.none )

        ( Move maybeDirection, WaitingPlayer ({ player } as innerModel) ) ->
            if hasWon player || player.health <= 0 then
                ( outerModel, Cmd.none )

            else
                let
                    newModel : InnerModel
                    newModel =
                        { innerModel | player = { player | hit = Nothing } }
                            |> movePlayer maybeDirection
                            |> applyDamage
                in
                ( WaitingEnemies newModel
                , if hasWon newModel.player || newModel.player.health <= 0 then
                    Task.perform (\_ -> NewLevel) <| Process.sleep (1000 * 2)

                  else
                    Task.perform (\_ -> MoveEnemies) <| Process.sleep (1000 * 0.1)
                )

        ( NewLevel, WaitingPlayer innerModel ) ->
            ( newLevel innerModel, Cmd.none )

        ( MoveEnemies, WaitingPlayer _ ) ->
            ( outerModel, Cmd.none )

        ( Move direction, WaitingEnemies innerModel ) ->
            let
                newModel =
                    innerModel
                        |> moveEnemies
                        |> applyDamage
                        |> addIntentionToEntities

                ( result, cmd1 ) =
                    update (Move direction) <| WaitingPlayer newModel

                cmd2 =
                    if newModel.player.health <= 0 then
                        Task.perform (\_ -> NewLevel) <| Process.sleep (1000 * 2)

                    else
                        Cmd.none
            in
            ( result, Cmd.batch [ cmd1, cmd2 ] )

        ( NewLevel, WaitingEnemies innerModel ) ->
            ( newLevel innerModel, Cmd.none )

        ( MoveEnemies, WaitingEnemies innerModel ) ->
            let
                newModel =
                    innerModel
                        |> moveEnemies
                        |> applyDamage
                        |> addIntentionToEntities
            in
            ( WaitingPlayer newModel
            , if newModel.player.health <= 0 then
                Task.perform (\_ -> NewLevel) <| Process.sleep (1000 * 2)

              else
                Cmd.none
            )


newLevel : InnerModel -> OuterModel
newLevel innerModel =
    let
        newModel : InnerModel
        newModel =
            if hasWon innerModel.player then
                innerInit innerModel.seed innerModel.size <| innerModel.level + 1

            else
                innerInit innerModel.seed innerModel.size 1
    in
    if hasWon innerModel.player then
        let
            newPlayer : Player
            newPlayer =
                newModel.player
        in
        WaitingPlayer
            { newModel
                | player =
                    { newPlayer
                        | health = max 3 <| innerModel.player.health
                    }
            }

    else
        WaitingPlayer newModel


moveEnemies : InnerModel -> InnerModel
moveEnemies innerModel =
    { innerModel
        | entities =
            List.map
                (\entity ->
                    case entity.intention of
                        Nothing ->
                            entity

                        Just direction ->
                            { entity | position = move direction entity.position }
                )
                innerModel.entities
    }


hasWon : Player -> Bool
hasWon player =
    player.position == ( boardSize - 1, boardSize - 1 ) && player.health > 0


applyDamage : InnerModel -> InnerModel
applyDamage ({ player } as innerModel) =
    if player.hit == Nothing then
        let
            entity : Maybe Entity
            entity =
                List.Extra.find
                    (\{ position, data } ->
                        position == player.position && isActive data
                    )
                    innerModel.entities
        in
        case entity of
            Nothing ->
                innerModel

            Just en ->
                let
                    hit =
                        getHit player en

                    armed : Bool
                    armed =
                        case en.data of
                            Sord _ ->
                                True

                            _ ->
                                False
                in
                { innerModel
                    | player =
                        { player
                            | health = clamp 0 9 <| player.health - Tuple.first hit
                            , hit = Just hit
                            , armed = player.armed || armed
                        }
                    , entities =
                        List.Extra.updateIf
                            (\{ name } -> name == en.name)
                            damageEntity
                            innerModel.entities
                }

    else
        innerModel


getHit : Player -> Entity -> Hit
getHit player en =
    if isActive en.data then
        case ( player.armed, en.data ) of
            ( False, Bat _ ) ->
                ( 2, "A 2 hit you!" )

            ( True, Bat _ ) ->
                ( 1, "Slain a 1!" )

            ( False, Rat _ ) ->
                ( 3, "A 3 bit you!" )

            ( True, Rat { health } ) ->
                ( health
                , if health == 1 then
                    "Slain a 1!"

                  else
                    "Hit a 2!"
                )

            ( _, Spider ) ->
                ( 4, "You hit a 4wall!" )

            ( _, Potion { health } ) ->
                ( -health, "Got a potion (+" ++ String.fromInt health ++ "hp)!" )

            ( _, Sord _ ) ->
                ( 0, "Picked up SORD!" )

    else
        ( 0, "" )


movePlayer : Maybe Direction -> InnerModel -> InnerModel
movePlayer maybeDirection ({ player } as innerModel) =
    case moveResult maybeDirection player of
        OutOfRange ->
            innerModel

        Moved moved ->
            { innerModel
                | player =
                    { player
                        | position = moved
                    }
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

                Spider ->
                    Spider

                Potion _ ->
                    Potion { health = 0 }

                Sord s ->
                    Sord { s | picked = True }
    }


addIntentionToEntities : InnerModel -> InnerModel
addIntentionToEntities innerModel =
    let
        ( entities, newSeed ) =
            randomMap innerModel.seed
                (addIntentionToEntity innerModel.player)
                innerModel.entities
    in
    { innerModel | entities = entities, seed = newSeed }


addIntentionToEntity : Player -> Entity -> Generator Entity
addIntentionToEntity player entity =
    let
        directionGenerator : Generator (Maybe Direction)
        directionGenerator =
            case ( isActive entity.data, entity.data ) of
                ( True, Bat _ ) ->
                    Random.andThen identity <|
                        Random.weighted
                            ( 1, towardsPlayer player entity )
                            [ ( 3, Random.map Just <| randomDirectionLegalFrom entity.position ) ]

                ( True, Rat { health } ) ->
                    Random.andThen identity <|
                        Random.weighted
                            ( 1 + toFloat health, towardsPlayer player entity )
                            [ ( 1, Random.map Just <| randomDirectionLegalFrom entity.position ) ]

                ( True, Spider ) ->
                    Random.constant Nothing

                ( True, Potion _ ) ->
                    Random.constant Nothing

                ( True, Sord _ ) ->
                    Random.constant Nothing

                ( False, _ ) ->
                    Random.constant Nothing
    in
    Random.map
        (\direction ->
            { entity | intention = direction }
        )
        directionGenerator


randomMap : Seed -> (a -> Generator b) -> List a -> ( List b, Seed )
randomMap seed f list =
    List.foldr
        (\x ( acc, accSeed ) ->
            let
                ( fx, newSeed ) =
                    Random.step (f x) accSeed
            in
            ( fx :: acc, newSeed )
        )
        ( [], seed )
        list


towardsPlayer : Player -> Entity -> Generator (Maybe Direction)
towardsPlayer player entity =
    let
        ( px, py ) =
            player.position

        ( ex, ey ) =
            entity.position

        dirs : List Direction
        dirs =
            [ ifTrue (px < ex) Left
            , ifTrue (px > ex) Right
            , ifTrue (py < ey) Up
            , ifTrue (py > ey) Down
            ]
                |> List.filterMap identity
    in
    case dirs of
        [] ->
            Random.constant Nothing

        h :: t ->
            Random.map Just <| Random.uniform h t


ifTrue : Bool -> a -> Maybe a
ifTrue condition value =
    if condition then
        Just value

    else
        Nothing


randomDirectionLegalFrom : Position -> Generator Direction
randomDirectionLegalFrom ( x, y ) =
    let
        list : List Direction
        list =
            List.filterMap identity
                [ ifTrue (x > 0) Left
                , ifTrue (x < boardSize - 1) Right
                , ifTrue (y > 0) Up
                , ifTrue (y < boardSize - 1) Down
                ]
    in
    case list of
        [] ->
            Random.constant Down

        h :: t ->
            Random.uniform h t


type MoveResult
    = OutOfRange
    | Moved Position


moveResult : Maybe Direction -> Player -> MoveResult
moveResult maybeDirection player =
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
        Moved moved


isActive : EntityData -> Bool
isActive data =
    case data of
        Bat { health } ->
            health > 0

        Rat { health } ->
            health > 0

        Potion { health } ->
            health > 0

        Sord { picked } ->
            not picked

        Spider ->
            True


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


entitiesGenerator : Int -> Generator (List Entity)
entitiesGenerator level =
    let
        randomEnemyPosition : Generator Position
        randomEnemyPosition =
            randomPosition <|
                \( x, y ) ->
                    (x > 4 || y > 4)
                        && (x < boardSize - 2 || y < boardSize - 2)

        buildEntity :
            Generator EntityData
            -> Generator Position
            -> Generator { data : EntityData, position : Position }
        buildEntity =
            Random.map2
                (\data position ->
                    { data = data
                    , position = position
                    }
                )

        randoms : Generator (List { data : EntityData, position : Position })
        randoms =
            buildEntity randomEnemy randomEnemyPosition
                |> Random.list (clamp 5 60 <| 5 * level)

        spiders : Generator (List { data : EntityData, position : Position })
        spiders =
            buildEntity randomSpider randomEnemyPosition
                |> Random.list (clamp 10 60 <| 10 * level)

        potions : Generator (List { data : EntityData, position : Position })
        potions =
            buildEntity randomPotion randomEnemyPosition
                |> Random.list (clamp 3 10 <| 20 - level)

        sord : Generator (List { data : EntityData, position : Position })
        sord =
            buildEntity randomSord
                (randomPosition <|
                    \( x, y ) ->
                        (x > boardSize // 4)
                            && (y > boardSize // 4)
                            && (x < boardSize // 2)
                            && (y < boardSize // 2)
                )
                |> Random.list (max 1 <| 4 - level)

        dedupAndNameEntities : List { position : Position, data : EntityData } -> List Entity
        dedupAndNameEntities raw =
            raw
                |> List.Extra.gatherEqualsBy .position
                |> List.map
                    (\( h, t ) ->
                        List.Extra.find
                            (\{ data } ->
                                case data of
                                    Sord _ ->
                                        True

                                    _ ->
                                        False
                            )
                            (h :: t)
                            |> Maybe.withDefault h
                    )
                |> List.sortBy
                    (\{ data } ->
                        case data of
                            Sord _ ->
                                0

                            Potion _ ->
                                1

                            Spider ->
                                2

                            Bat _ ->
                                3

                            Rat _ ->
                                4
                    )
                |> List.indexedMap
                    (\i { position, data } ->
                        { position = position
                        , data = data
                        , intention = Nothing
                        , name = "Enemy " ++ String.fromInt i
                        }
                    )
    in
    Random.map4 (\a b c d -> a ++ b ++ c ++ d)
        sord
        randoms
        spiders
        potions
        |> Random.map dedupAndNameEntities


randomSord : Generator EntityData
randomSord =
    Random.map (\index -> Sord { picked = False, index = index })
        (Random.uniform 22 (List.range 23 29))


randomPosition : (Position -> Bool) -> Generator Position
randomPosition accept =
    Random.map2 Tuple.pair
        (Random.int 0 (boardSize - 1))
        (Random.int 0 (boardSize - 1))
        |> Random.andThen
            (\pos ->
                if accept pos then
                    Random.constant pos

                else
                    Random.lazy (\_ -> randomPosition accept)
            )


randomEnemy : Generator EntityData
randomEnemy =
    Random.weighted ( 5, randomBat ) [ ( 1, randomRat ) ]
        |> Random.andThen identity


randomBat : Generator EntityData
randomBat =
    Random.constant (Bat { health = 1 })


randomRat : Generator EntityData
randomRat =
    Random.map (\health -> Rat { health = health }) (Random.int 1 2)


randomSpider : Generator EntityData
randomSpider =
    Random.constant Spider


randomPotion : Generator EntityData
randomPotion =
    Random.map (\health -> Potion { health = health }) (Random.int 1 3)


tilePositionFromIndex : Int -> Position
tilePositionFromIndex index =
    ( modBy 11 index, index // 11 )



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
areas ({ player, level } as innerModel) =
    let
        background : List ( Position, Tile msg )
        background =
            List.range 0 (boardSize - 1)
                |> List.foldl
                    (\x ( acc, seed ) ->
                        let
                            ( fx, seed_ ) =
                                List.range 0 (boardSize - 1)
                                    |> randomMap seed
                                        (\y ->
                                            backgroundTile x y
                                        )
                        in
                        ( fx ++ acc, seed_ )
                    )
                    ( [], Random.initialSeed level )
                |> Tuple.first

        backgroundTile : Int -> Int -> Generator ( Position, Tile msg )
        backgroundTile x y =
            Random.weighted ( 4, 0 ) [ ( 3, 1 ) ]
                |> Random.map
                    (\r ->
                        ( ( x, y )
                        , Tile.fromPosition
                            (tilePositionFromIndex r)
                        )
                    )

        door : ( Position, Tile msg )
        door =
            ( doorPosition, Tile.fromPosition (tilePositionFromIndex 2) )

        messageTop =
            if hasWon player then
                "You won the roll!"

            else
                case player.hit of
                    Just ( _, message ) ->
                        message

                    Nothing ->
                        if player.health <= 0 then
                            ""

                        else if level == 1 then
                            "Get to the door"

                        else
                            "Level " ++ String.fromInt level

        messageBottom =
            let
                format : String -> String -> String
                format label value =
                    label ++ String.padLeft (boardSize - String.length label) ' ' value

                emptys =
                    case player.hit of
                        Nothing ->
                            ""

                        Just ( hitCount, _ ) ->
                            String.repeat hitCount "`"

                fulls =
                    String.repeat (max 0 player.health) "\u{007F}"
            in
            if player.health <= 0 then
                "You got diced!"

            else
                format "Health" <| emptys ++ fulls

        berlin : Tileset
        berlin =
            { source = "berlin.png"
            , spriteWidth = tileSize
            , spriteHeight = tileSize
            }

        tinyDungeon : Tileset
        tinyDungeon =
            { source = "tileset.png"
            , spriteWidth = tileSize
            , spriteHeight = tileSize
            }
    in
    [ tilesFromText ( 0, 0 ) messageTop
        |> PixelEngine.tiledArea
            { rows = 1
            , tileset = berlin
            , background = PixelEngine.colorBackground <| Color.black
            }
    , tilesFromText ( 0, 0 ) messageBottom
        |> PixelEngine.tiledArea
            { rows = 1
            , tileset = berlin
            , background = PixelEngine.colorBackground <| Color.black
            }
    , (background
        ++ door
        :: playerToTile player
        :: List.map (entityToTile player) innerModel.entities
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
                    code : Int
                    code =
                        Char.toCode char - 0x20
                in
                ( ( dx + i, dy ), Tile.fromPosition ( modBy 16 code, code // 16 ) )
            )


playerToTile : Player -> ( Position, Tile msg )
playerToTile { position, health, armed } =
    ( position
    , Tile.movable "player" <|
        Tile.fromPosition <|
            tilePositionFromIndex <|
                if health > 0 then
                    if armed then
                        4

                    else
                        3

                else
                    5
    )


entityToTile : Player -> Entity -> ( Position, Tile msg )
entityToTile player ({ name, position, data, intention } as entity) =
    let
        maybeIndex : Maybe Int
        maybeIndex =
            case ( data, isActive data ) of
                ( Potion { health }, _ ) ->
                    case health of
                        0 ->
                            Just 6

                        1 ->
                            Just 8

                        2 ->
                            Just 7

                        _ ->
                            Just 9

                ( _, False ) ->
                    Nothing

                ( Sord { index }, _ ) ->
                    Just index

                _ ->
                    Just <| (+) 15 <| Tuple.first <| getHit player entity

        tiles : List (Maybe Int)
        tiles =
            [ maybeIndex
            , Maybe.map
                (\direction ->
                    case direction of
                        Up ->
                            11

                        Right ->
                            12

                        Down ->
                            13

                        Left ->
                            14
                )
                intention
            ]
    in
    ( position
    , tiles
        |> List.filterMap
            (Maybe.map (Tile.fromPosition << tilePositionFromIndex))
        |> Tile.multipleTiles
        |> Tile.movable name
    )



{------------------------
    CONFIGURATION
------------------------}


options : Int -> Options Msg
options scale =
    Options.default
        |> Options.withMovementSpeed 0.1
        |> Options.withScale scale


view :
    OuterModel
    -> { title : String, body : List (Html Msg) }
view outerModel =
    { title = "Roll2Die"
    , body =
        case outerModel of
            WaitingPlayer innerModel ->
                viewInner innerModel

            WaitingEnemies innerModel ->
                viewInner innerModel

            _ ->
                [ Html.text "Initializing..." ]
    }


viewInner : InnerModel -> List (Html Msg)
viewInner innerModel =
    let
        scale : Int
        scale =
            floor <|
                min
                    (toFloat innerModel.size.width
                        / (boardSize * tileSize)
                    )
                    (toFloat innerModel.size.height
                        / ((boardSize + textLines) * tileSize)
                    )
    in
    [ PixelEngine.toHtml
        { width = boardSize * tileSize
        , options = Just (options scale)
        }
        (areas innerModel)
    ]


main : Program () OuterModel Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
