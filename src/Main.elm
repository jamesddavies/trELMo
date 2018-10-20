port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String

-- MAIN

main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = \model -> { title = "trELMo", body = [view model] }
        }

-- MODEL

type alias Model = 
    { cards : List Card
    }

type alias Card = 
    { items : List Item
    , title : String
    , description: String
    , id : Int
    , status : ItemStatus
    }

type alias Item = 
    { title : String
    , description: String
    , id : Int
    , status : ItemStatus
    }

type ItemStatus = Active | Completed | Archived


init : () -> (Model, Cmd Msg)
init _ = 
    ( Model []
    , Cmd.none
    )

-- UPDATE

type Msg
    = AddCard
    | DeleteCard Int
    | AddItem Int
    | CompleteItem Int Int
    | DeleteItem Int Int
    | UpdateCardTitle Int String
    | UpdateCardDescription Int String
    | UpdateItemTitle Int Int String
    | UpdateItemDescription Int Int String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AddCard ->
            ( { model | cards = List.append model.cards [newCard (getNewId model.cards)]}
            , Cmd.none
            )
        DeleteCard id ->
            ( { model | cards = List.filter (\card -> card.id /= id) model.cards}
            , Cmd.none
            )
        AddItem newId ->
            ( { model | cards = mapCards model.cards newId addItem }
            , Cmd.none
            )
        CompleteItem cardId itemId ->
            ( { model | cards = mapItems model.cards cardId itemId completeItem }
            , Cmd.none
            )
        DeleteItem cardId itemId ->
            ( { model | cards = mapItems model.cards cardId itemId deleteItem }
            , Cmd.none
            )
        UpdateCardTitle cardId title ->
            ( { model | cards = (mapCardsParam model.cards cardId updateTitle title)}
            , Cmd.none
            )
        UpdateCardDescription cardId description ->
            ( { model | cards = (mapCardsParam model.cards cardId updateDescription description)}
            , Cmd.none
            )
        UpdateItemTitle itemId cardId title ->
            ( { model | cards = (mapItemsParam model.cards cardId itemId updateTitle title)}
            , Cmd.none
            )
        UpdateItemDescription itemId cardId description ->
            ( { model | cards = (mapItemsParam model.cards cardId itemId updateDescription description)}
            , Cmd.none
            )

newCard : Int -> Card
newCard n =
    { items = [
        { title = ""
        , description = ""
        , id = 0
        , status = Active
        }
    ]
    , title = ""
    , description = ""
    , id = n
    , status = Active
    }

newItem : Int -> Item
newItem n = 
    { title = ""
    , description = ""
    , id = n
    , status = Active
    }

mapCards cards id callback =
    List.map (\card -> if card.id == id then callback card else card) cards

mapCardsParam cards id callback param =
    List.map (\card -> if card.id == id then callback card param else card) cards

mapItems cards cardId itemId callback =
    List.map (\card -> if card.id == cardId then callback card itemId else card) cards

mapItemsParam cards cardId itemId callback param =
    List.map (\card -> 
        if card.id == cardId then 
            { card | items = (List.map (\item -> if item.id == itemId then callback item param else item) card.items)}
        else 
            { card | items = card.items }
    ) cards

addItem : Card -> Card
addItem card =
    { card | items = List.append card.items [newItem (getNewId card.items)]}

completeItem : Card -> Int -> Card
completeItem card itemId =
    { card | items = (List.map (\item -> if item.id == itemId then { item | status = Completed } else item) card.items)}

deleteItem : Card -> Int -> Card
deleteItem card itemId =
    { card | items = (List.filter (\item -> item.id /= itemId) card.items)}

updateTitle a title =
    { a | title = title }

updateDescription a description =
    { a | description = description }

getNewId list =
    case List.head (List.reverse list) of
        Just el ->
            el.id + 1
        Nothing ->
            0

isComplete : ItemStatus -> Bool
isComplete status =
    case status of
        Active ->
            False
        Completed ->
            True
        Archived ->
            False

getCompleteItems list =
    List.filter (\i -> (isComplete i.status)) list

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- VIEW

view : Model -> Html Msg
view model = 
    div [] [
        headerView
        , div [ class "container is-flid" ] [
            (cardsView model.cards)
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
    div [ class "column is-4" ] [ 
        div [ class "box has-background-primary has-text-centered fade-in" ] [
            input [ class "title-input", value cardData.title, placeholder "Card title", (onInput (UpdateCardTitle cardData.id)) ] []
            , input [ class "description-input", value cardData.description, placeholder "Add a description...", (onInput (UpdateCardDescription cardData.id)) ] []
            , progress [ classList [ ("progress", True), ("is-info", (itemsCompletePercentage cardData.items) /= "100"), ("is-success box-shadow", (itemsCompletePercentage cardData.items) == "100") ], value (itemsCompletePercentage cardData.items), Html.Attributes.max "100" ] []
            , div [ class "todo-area" ] 
                (List.map (\item -> (listItemView item cardData.id)) cardData.items)
            , div [ class "card-bottom has-text-white" ] [
                div [ class "delete-card" ] [
                    span [ class "icon is-small", onClick (DeleteCard cardData.id) ] [ i [ class "fas fa-trash-alt" ] [] ]
                ]
                , div [ class "add-todo" ] [
                    div [ class "has-text-white", onClick (AddItem cardData.id) ] [ text "Add a new item" ]
                    , span [ class "icon is-small", onClick (AddItem cardData.id) ] [ i [ class "fas fa-plus"] [] ]
                ]
            ]
        ]
    ]

listItemView : Item -> Int -> Html Msg
listItemView item cardId =
    div [ classList [ ("todo-item", True), ("fade-in", True), ("complete", (isComplete item.status))] ] [
        div [ class "icon-container" ] [
            span [ class "icon is-small", onClick (CompleteItem cardId item.id) ] [ i [ class (listItemIcon item.status) ] [] ]
        ]
        , div [ class "content-container" ] [
            input [ class "item-title", value item.title, placeholder "New item", (onInput (UpdateItemTitle item.id cardId)) ] []
            , textarea [ class "item-description", placeholder "Add a description...", (onInput (UpdateItemDescription item.id cardId)) ] [ text item.description ]
        ]
        , div [ class "delete-container"] [
            span [ class "icon is-small", onClick (DeleteItem cardId item.id) ] [ i [ class "fas fa-trash-alt has-text-danger" ] [] ]
        ]
    ]

listItemIcon : ItemStatus -> String
listItemIcon status =
    case status of
        Active ->
            "far fa-square has-text-link active"
        Completed ->
            "fas fa-check has-text-success completed"
        Archived ->
            "fas fa-archive has-text-grey-light archived"

itemsCompletePercentage items =
   String.fromFloat (((toFloat (List.length (getCompleteItems items))) / toFloat (List.length items)) * 100)