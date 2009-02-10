module AppControl (appHandler) where
import Happstack.Server
import Happstack.State
import AppState

appHandler :: [ServerPartT IO Response]
appHandler =
  [dir "entries" [postEntry, getEntries]
  ,fileServe ["index.html"] "public"]
  
--getEntries :: ServerPartT IO Response
getEntries = method GET $ renderGuestBook
  
-- only accept a post method for adding a new guestbook entry
postEntry :: ServerPartT IO Response
postEntry = withData $ \e @ (GuestBookEntry {author=author, message=message}) ->
  [method POST $ do
     update (AddGuestBookEntry e)
     renderGuestBook
  ]

-- this tells happstack how to turn post data into a datatype using 'withData'
instance FromData GuestBookEntry where
  fromData = do
    author <- look "author"
    message <- look "message"
    return $ GuestBookEntry author message

-- rendering details for guestbook page
renderGuestBook = do
  gb <- query ReadGuestBook
  let li e = "<li><strong>" ++ author e ++ ":</strong> " ++ message e ++ "</li>"
  ok $ setHeader "Content-Type" "text/html" $
    toResponse $ "<ul>" ++ concatMap li gb ++ "</ul>"


