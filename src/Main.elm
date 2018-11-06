port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import Dict

-- MAIN

main =
    Browser.document
        { init = init
        , update = updateWithStorage
        , subscriptions = subscriptions
        , view = \model -> { title = "trELMo", body = [view model] }
        }

port setStorage : EncodedModel -> Cmd msg

updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        ( newModel
        , Cmd.batch [ setStorage  (encodeModel newModel), cmds ]
        )

encodeModel : Model -> EncodedModel
encodeModel model =
    { cards = Dict.toList model.cards
    |> List.map (\(k, v) -> 
        (k, { items = (Dict.toList v.items)
            , title = v.title
            , description = v.description
            , active = v.active
            , completed = v.completed
            }
        ))
    }

decodeModel : EncodedModel -> Model
decodeModel encodedModel =
    { cards = encodedModel.cards
    |> List.map (\(k, v) ->
        (k, { items = (Dict.fromList v.items)
        , title = v.title
        , description = v.description
        , active = v.active
        , completed = v.completed
        }
    ))
    |> Dict.fromList    
    }

-- MODEL

type alias Model = 
    { cards : Dict.Dict Int Card
    }

type alias EncodedModel = 
    { cards : List (Int, EncodedCard)
    }

type alias Card = 
    { items : Dict.Dict Int Item
    , title : String
    , description: String
    , active : Bool
    , completed : Bool
    }

type alias EncodedCard = 
    { items: List (Int, Item)
    , title : String
    , description : String
    , active : Bool
    , completed : Bool
    }

type alias Item = 
    { title : String
    , description: String
    , active : Bool
    , completed : Bool
    }

init : Maybe EncodedModel -> (Model, Cmd Msg)
init encodedModel = 
    ( (decodeModel (Maybe.withDefault { cards = [] } encodedModel))
    , Cmd.none
    )

-- UPDATE

type Msg
    = AddCard
    | DeleteCard Int
    | AddItem Int
    | ToggleComplete Int Int
    | DeleteItem Int Int
    | UpdateCardTitle Int String
    | UpdateCardDescription Int String
    | UpdateItemTitle Int Int String
    | UpdateItemDescription Int Int String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AddCard ->
            ( { model | cards = Dict.insert (getNewId model.cards) newCard model.cards }
            , Cmd.none
            )
        DeleteCard id ->
            ( { model | cards = Dict.remove id model.cards }
            , Cmd.none
            )
        AddItem cardId ->
            ( { model | cards = Dict.update cardId (itemUpdater addItem (-1)) model.cards }
            , Cmd.none
            )
        ToggleComplete cardId itemId ->
            ( { model | cards = Dict.update cardId (itemUpdater completeItem itemId) model.cards }
            , Cmd.none
            )
        DeleteItem cardId itemId ->
            ( { model | cards = Dict.update cardId (itemUpdater deleteItem itemId) model.cards }
            , Cmd.none
            )
        UpdateCardTitle cardId title ->
            ( { model | cards = Dict.update cardId (Maybe.map <| updateTitle title) model.cards }
            , Cmd.none
            )
        UpdateCardDescription cardId description ->
            ( { model | cards = Dict.update cardId (Maybe.map <| updateDescription description) model.cards }
            , Cmd.none
            )
        UpdateItemTitle itemId cardId title ->
            ( { model | cards = Dict.update cardId (itemFieldUpdater updateItemTitle itemId title) model.cards }
            , Cmd.none
            )
        UpdateItemDescription itemId cardId description ->
            ( { model | cards = Dict.update cardId (itemFieldUpdater updateItemDescription itemId description) model.cards }
            , Cmd.none
            )

newCard : Card
newCard =
    { items = (Dict.singleton 0 newItem)
    , title = ""
    , description = ""
    , active = True
    , completed = False
    }

newItem : Item
newItem = 
    { title = ""
    , description = ""
    , active = True
    , completed = False
    }

cardUpdater callback content card =
    case card of
        Just a ->
            Just (callback a content)
        Nothing ->
            card

itemUpdater callback itemId card =
    case card of
        Just a ->
            if itemId < 0 then
                Just { a | items = (callback (getNewId a.items)) a.items }
            else
                Just { a | items = (callback itemId) a.items }
        Nothing ->
            card

itemFieldUpdater callback itemId field card =
    case card of
        Just a ->
            Just { a | items = Dict.update itemId (callback field) a.items }
        Nothing ->
            card

addItem itemId =
    Dict.insert itemId newItem

completeItem itemId =
    Dict.update itemId (updateCompletedStatus)

deleteItem itemId =
    Dict.remove itemId

updateCompletedStatus item =
    case item of
        Just a ->
            Just { a | completed = not a.completed }
        Nothing ->
            item

updateItemTitle title item =
    case item of
        Just a ->
            Just { a | title = title }
        Nothing ->
            item

updateItemDescription description item =
    case item of
        Just a ->
            Just { a | description = description }
        Nothing ->
            item

updateTitle title a =
    { a | title = title }

updateDescription description a =
    { a | description = description }

getNewId dict =
    case List.head (List.reverse (Dict.toList dict)) of
        Just (k, v) ->
            k + 1
        Nothing ->
            0

getCompleteItems list =
    List.filter (\i -> (Tuple.second i).completed) list

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- VIEW

view : Model -> Html Msg
view model = 
    div [] [
        headerView
        , div [ class "container is-fluid" ] [
            (cardsView (Dict.toList model.cards))
        ]
    ]

headerView =
    div [ class "navbar is-primary padding-sides" ] [
        div [ class "navbar-brand" ] [
            h2 [ class "title is-4 has-text-white" ] [ text "trELMo" ]
        ]
        , div [ class "navbar-menu" ] [
            div [ class "navbar-end" ] [
                div [ class "buttons" ] [
                    button [ class "button is-primary", onClick AddCard ] [
                        span [] [ text "Add Card" ]
                        , span [ class "icon is-small" ] [ i [ class "fas fa-plus"] [] ]
                    ]
                ]
            ]
        ]
    ]

cardsView cardList =
    div [ class "columns is-multiline is-centered" ]
        (List.map (\cardData -> (cardView cardData)) cardList)

cardView cardData =
    let
        id = Tuple.first cardData
        card = Tuple.second cardData
    in
    
    div [ class "column is-4" ] [ 
        div [ class "box has-background-primary has-text-centered fade-in" ] [
            input [ class "title-input", value card.title, placeholder "Card title", (onInput (UpdateCardTitle id)) ] []
            , input [ class "description-input", value card.description, placeholder "Add a description...", (onInput (UpdateCardDescription id)) ] []
            , progress [ classList [ ("progress", True), ("is-info", (itemsCompletePercentage card.items) /= "100"), ("is-success box-shadow", (itemsCompletePercentage card.items) == "100") ], value (itemsCompletePercentage card.items), Html.Attributes.max "100" ] []
            , div [ class "todo-area" ] 
                (List.map (\item -> (listItemView item id)) (Dict.toList card.items))
            , div [ class "card-bottom has-text-white" ] [
                div [ class "delete-card" ] [
                    span [ class "icon is-small", onClick (DeleteCard id) ] [ i [ class "fas fa-trash-alt" ] [] ]
                ]
                , div [ class "add-todo" ] [
                    div [ class "has-text-white", onClick (AddItem id) ] [ text "Add a new item" ]
                    , span [ class "icon is-small", onClick (AddItem id) ] [ i [ class "fas fa-plus"] [] ]
                ]
            ]
        ]
    ]

--listItemView : Item -> Int -> Html Msg
listItemView itemData cardId =
    let
        itemId = Tuple.first itemData
        item = Tuple.second itemData
    in
        
    div [ classList [ ("todo-item", True), ("fade-in", True), ("complete", (item.completed))] ] [
        div [ class "icon-container" ] [
            span [ class "icon is-small", onClick (ToggleComplete cardId itemId) ] [ 
                i [ class (listItemIcon (item.completed)) ] [] 
            ]
        ]
        , div [ class "content-container" ] [
            input [ class "item-title", value item.title, placeholder "New item", (onInput (UpdateItemTitle itemId cardId)) ] []
            , textarea [ class "item-description", placeholder "Add a description...", (onInput (UpdateItemDescription itemId cardId)) ] [ text item.description ]
        ]
        , div [ class "delete-container"] [
            span [ class "icon is-small", onClick (DeleteItem cardId itemId) ] [ i [ class "fas fa-trash-alt has-text-danger" ] [] ]
        ]
    ]

listItemIcon : Bool -> String
listItemIcon status =
    if status then
        "fas fa-check has-text-success completed"
    else
        "far fa-square has-text-link active"
            

itemsCompletePercentage itemsDict =
    let
        items = Dict.toList itemsDict
    in
        if (List.length items) == 0 then
            String.fromInt 0
        else
            String.fromFloat (((toFloat (List.length (getCompleteItems items))) / toFloat (List.length items)) * 100)