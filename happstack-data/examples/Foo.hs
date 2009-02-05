
{-# OPTIONS_GHC -fglasgow-exts -fth
                -fallow-overlapping-instances
                -fallow-undecidable-instances #-}

module Main (main) where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString.Char8 as P
import Data.Generics.SYB.WithClass.Basics hiding (Prefix)
import Data.Generics.SYB.WithClass.Derive
import Data.Generics.SYB.WithClass.Instances ()
import HAppS.Data.HTML_Forms.Form
import Xml.DeriveAll -- Euch, should be in sybwc

import HAppS hiding (Proxy)

$( deriveAll [d| data Foo = Foo Int (Maybe Bool)
                          | Bar (Wrapper String)
                    deriving Show
                 data Wrapper a = Wrapper a
                    deriving Show
               |] )
-- $( deriveAll [d| data Foo = Bar String deriving Show |] )

main :: IO ()
main = stdMain server

server :: StdPart ()
server = simpleHTTP http :*: End

http :: [ServerPart]
http = [ dir "form.css" [method GET $ text_css $ ok myCss],
         dir "res" [withData (\d -> [method POST $ text_html $ res (mkVal d)])],
         method GET $ text_html $ ok $ mkPage myContent
       ]

instance FromData CGIParams where
    fromData = liftM CGIParams ask

res :: Either String Foo -> IO Result
res (Left err) = ok ("Failed: " ++ err)
res (Right v) = ok ("Succeeded: " ++ show v)

mkVal :: CGIParams -> Either String Foo
mkVal = fromForm

lookupString :: CGIParams -> String -> Maybe String
lookupString (CGIParams ps) key
 = case lookup key ps of
       Just val -> Just $ P.unpack $ inputValue val
       Nothing -> Nothing

setContextType :: String -> IO Result -> IO Result
setContextType ct f = liftM (setHeader "Content-Type" ct) f

text_html :: IO Result -> IO Result
text_html = setContextType "text/html"

text_css :: IO Result -> IO Result
text_css = setContextType "text/css"

{-
Old API:

helloWorld :: () -> () -> Ev () req (Either Request String)
helloWorld () () = respond $ mkPage myContent

css :: () -> () -> Ev () req (Either Request String)
css () () = respond myCss

main :: IO ()
main = stdHTTP
     $ debugFilter :
       noState :
       [
        h ["form.css"] GET $ ok css,
        h () GET $ ok helloWorld
       ]

-}

myCss :: String
myCss = "\
\.choices {\n\
\    margin: 3px;\n\
\    border: 3px solid purple;\n\
\}\n\
\.choice {\n\
\    margin: 3px;\n\
\    border: 1px solid red;\n\
\}\n\
\.contains {\n\
\    margin: 3px;\n\
\    border: 1px solid blue;\n\
\}"

myContent :: String
myContent = "<h1>Foo</h1>\n"
         ++ "<form method=\"post\" action=\"res\">\n"
         ++ mkForm (undefined :: Foo)
         ++ "<input type=\"submit\" value=\"Go\">\n"
         ++ "</form>\n"

mkPage :: String -> String
mkPage content = mkTag "html"
                     (mkTag "head"
                         (mkTag "title" "Foo"
                       ++ "<link rel=\"Stylesheet\" type=\"text/css\" href=\"form.css\"></link>\n")
                   ++ mkTag "body" content)

