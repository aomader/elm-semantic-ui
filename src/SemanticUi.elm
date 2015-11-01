module SemanticUi
    ( Element
    , render

    , Size(..)
    , size

    , Button
    , button
    , button'
    ) where

{-| Elm bindings for Semantic UI using a declarative API and useful
abstractions.

# Rendering

@docs Element, render

# Styling

@docs Size, size

# Elements

## Button

@docs Button, button, button'

-}

import List exposing (append)
import String exposing (join)

import Html exposing (Html, Attribute)
import Html as H
import Html.Attributes as A


{-| An element defines a control. A control has a `state` and a function to
turn this state into an `Html` object. -}
type alias Element a =
    { state : a
    , render : a -> Html
    }

{-| Create the `Html` for a Semantic UI `Element`.

    button "click" |> render
-}
render : Element a -> Html
render element =
    element.render element.state


element : a -> (a -> Html) -> Element a
element state render =
    { state = state, render = render }

style : (a -> a) -> Element a -> Element a
style f element =
    let oldStyle = element.state
        newStyle = f oldStyle
    in { element | state <- newStyle }


{-| Some elements might have different sizes. -}
type Size
    = Mini
    | Tiny
    | Small
    | Medium
    | Large
    | Big
    | Huge
    | Massive

type alias Sized a =
    { a | size : Size }

{-| Adjust the size of an element.

    button "click" |> size Huge
 -}
size : Size -> Element (Sized a) -> Element (Sized a)
size size = style <| \state -> { state | size <- size }


type alias Readable a =
    { a | text : String }

{-| Adjust the text of an element.

    button "click" |> text "DON'T click"
-}
text : String -> Element (Readable a) -> Element (Readable a)
text text = style <| \state -> { state | text <- text }


{-| The button state driving the visual appearance. -}
type alias Button =
    { text : String
    , size : Size
    }

{-| A button with a text. -}
button : String -> Element Button
button text =
    let
        state =
            { text = text
            , size = Medium
            }

        render state =
            H.div
                [ A.class "ui button" ]
                [ H.text state.text ]

    in element state render

{-| A button with an empty text. -}
button' : Element Button
button' = button ""
