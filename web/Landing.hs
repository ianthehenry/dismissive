module Landing (Partial, signupPage) where

import BasePrelude
import Data.Text (Text)
import Lucid
import Lucid.Base

type Partial m = ([Text], [Text], HtmlT m ())

field :: Monad m => HtmlT m () -> HtmlT m () -> HtmlT m ()
field label content = div_ (span_ [class_ "label"] label <> content)

polyline_ :: Term arg result => arg -> result
polyline_ = term "polyline"

points_ :: Text -> Attribute
points_ = makeAttribute "points"

caret :: Monad m => HtmlT m ()
caret = svg_ (polyline_ [points_ "1,1 3,3 5,1"] "")

signupPage :: Monad m => Partial m
signupPage = (["/static/landing.css"], ["/static/landing.js"], ) $ do
  h1_ "Dismissive"
  div_ [class_ "window"] $ do
    div_ [class_ "window-bar"] "Send email... to the future!"
    div_ [class_ "window-content"] $ do
      div_ [class_ "header"] $ do
        field "To:" $ span_ [class_ "token"] ("remind+a82b1bf9@dismissive.io" <> caret)
        field "Cc:" ""
        field "Subject:" (span_ [class_ "content"] "")
      div_ [class_ "body"] (span_ [class_ "cursor"] "")
  form_ [method_ "POST", action_ "/auth"] $ do
    p_ "Log in or sign up with your email address:"
    input_ [name_ "username", style_ "display: none", type_ "text"]
    input_ [placeholder_ "foo@example.com", name_ "email", type_ "email"]
    input_ [type_ "submit", value_ "→"]
