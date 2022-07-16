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
    }


type alias Player =
    { position : Position
    , health : Int
    , hasHit : Maybe String
    , armed : Bool
    }


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


innerInit : Seed -> Size -> InnerModel
innerInit seed size =
    let
        player : Player
        player =
            { position = initialPlayerPosition
            , health = 3
            , hasHit = Nothing
            , armed = False
            }

        ( entities, newSeed ) =
            Random.step entitiesGenerator seed
    in
    { seed = newSeed
    , size = size
    , player = player
    , entities = entities
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
            ( WaitingPlayer <| innerInit seed size, Cmd.none )

        ( _, WaitingSeed _ ) ->
            ( outerModel, Cmd.none )

        ( SetSize size, WaitingSize seed ) ->
            ( WaitingPlayer <| innerInit seed size, Cmd.none )

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
                    newModel =
                        { innerModel | player = { player | hasHit = Nothing } }
                            |> movePlayer maybeDirection
                            |> applyDamage
                in
                ( WaitingEnemies newModel
                , if hasWon newModel.player then
                    Cmd.none

                  else
                    Task.perform (\_ -> MoveEnemies) <| Process.sleep (1000 * 0.2)
                )

        ( MoveEnemies, WaitingPlayer _ ) ->
            ( outerModel, Cmd.none )

        ( Move _, WaitingEnemies _ ) ->
            ( outerModel, Cmd.none )

        ( MoveEnemies, WaitingEnemies innerModel ) ->
            ( innerModel
                |> moveEnemies
                |> applyDamage
                |> addIntentionToEntities
                |> WaitingPlayer
            , Cmd.none
            )


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
    if player.hasHit == Nothing then
        let
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
                    ( damage, armed, hasHit ) =
                        case ( player.armed, en.data ) of
                            ( False, Bat _ ) ->
                                ( 1, False, "A Bat hit you!" )

                            ( True, Bat _ ) ->
                                ( 0, False, "Slain a Bat!" )

                            ( False, Rat _ ) ->
                                ( 2, False, "A Rat hit you!" )

                            ( True, Rat { health } ) ->
                                ( health - 1
                                , False
                                , if health == 1 then
                                    "Slain a Rat!"

                                  else
                                    "Hit a Rat!"
                                )

                            ( _, Spider ) ->
                                ( 2, False, "You got poisoned!" )

                            ( _, Potion { health } ) ->
                                ( -health, False, "Got a potion (+" ++ String.fromInt health ++ "hp)" )

                            ( _, Sord _ ) ->
                                ( 0, True, "Picked up SORD" )
                in
                { innerModel
                    | player =
                        { player
                            | health = player.health - damage
                            , hasHit = Just hasHit
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


entitiesGenerator : Generator (List Entity)
entitiesGenerator =
    let
        randomEnemyPosition =
            randomPosition <|
                \( x, y ) ->
                    (x > 4 || y > 4)
                        && (x < boardSize - 2 || y < boardSize - 2)

        buildEntity =
            Random.map2
                (\data position ->
                    { data = data
                    , position = position
                    }
                )

        randoms =
            buildEntity randomEnemy randomEnemyPosition
                |> Random.list 30

        spiders =
            buildEntity randomSpider randomEnemyPosition
                |> Random.list 40

        potions =
            buildEntity randomPotion randomEnemyPosition
                |> Random.list 12

        sord =
            buildEntity randomSord (randomPosition <| \( x, y ) -> x < 6 && y < 6)

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
                |> List.indexedMap
                    (\i { position, data } ->
                        { position = position
                        , data = data
                        , intention = Nothing
                        , name = "Enemy " ++ String.fromInt i
                        }
                    )
    in
    Random.map4 (\a b c d -> a :: b ++ c ++ d)
        sord
        randoms
        spiders
        potions
        |> Random.map dedupAndNameEntities


randomSord : Generator EntityData
randomSord =
    Random.map (\index -> Sord { picked = False, index = index })
        (Random.uniform 103 [ 104, 105, 106, 107, 117, 118, 119 ])


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

        door : ( Position, Tile msg )
        door =
            ( doorPosition, Tile.fromPosition (tilePositionFromIndex 45) )

        ( messageTop, messageBottom ) =
            let
                format label value =
                    label ++ String.padLeft (boardSize - String.length label) ' ' value
            in
            ( if hasWon player then
                "You won!"

              else
                Maybe.withDefault
                    (if player.health <= 0 then
                        ""

                     else
                        "Get to the door"
                    )
                    player.hasHit
            , if player.health <= 0 then
                "You died!"

              else
                format "Health" <| String.fromInt <| max 0 player.health
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


playerToTile : Player -> ( Position, Tile msg )
playerToTile { position, health, armed } =
    ( position
    , Tile.movable "player" <|
        Tile.fromPosition <|
            tilePositionFromIndex <|
                if health > 0 then
                    if armed then
                        96

                    else
                        99

                else
                    121
    )


entityToTile : Entity -> ( Position, Tile msg )
entityToTile { name, position, data, intention } =
    let
        maybeIndex =
            case data of
                Bat { health } ->
                    if health == 0 then
                        Nothing

                    else
                        Just 120

                Rat { health } ->
                    case health of
                        0 ->
                            Nothing

                        1 ->
                            Just 124

                        _ ->
                            Just 123

                Spider ->
                    Just 122

                Potion { health } ->
                    case health of
                        0 ->
                            Just 113

                        1 ->
                            Just 115

                        2 ->
                            Just 116

                        _ ->
                            Just 114

                Sord { picked, index } ->
                    if picked then
                        Nothing

                    else
                        Just index

        tiles =
            [ maybeIndex
            , Maybe.map
                (\direction ->
                    case direction of
                        Up ->
                            132

                        Right ->
                            133

                        Down ->
                            134

                        Left ->
                            135
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
        |> Options.withMovementSpeed 0.2
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


main : Program () OuterModel Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
