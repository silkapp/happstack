{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Main where

import Control.Monad.Trans(MonadIO(liftIO))
import Data.Text (pack)
import Happstack.Server
import Happstack.Server.Hamlet
import Text.Hamlet
import Text.Hamlet.Monad (hamletToText, liftHamlet)

-- | type used to populate the template
data Person = Person
    { name :: IO HtmlContent -- maybe it requires a database lookup
    , age :: HtmlContent
    , page :: PersonUrls
    , isMarried :: Bool
    , children :: [HtmlContent]
    }

-- | type which represents paths to different pages on the site
data PersonUrls = Homepage | PersonPage String

-- | function to turn the url type into a string
renderUrls :: PersonUrls -> String
renderUrls Homepage = "/"
renderUrls (PersonPage name) = '/' : name

-- | hamlet template which generates page footer
footer :: Monad m => Hamlet url m ()
footer = [$hamlet|
#footer Thank you, come again
|]


-- | hamlet template which generates a page
template :: Person -> Hamlet PersonUrls IO ()
template person = [$hamlet|
!!!
%html
    %head
        %title Hamlet Demo
    %body
        %h1 Information on $*name.person$
        %p $*name.person$ is $age.person$ years old.
        %h2
            $if isMarried.person
                <b>Married</b>
            $else
                Not <b>married</b>
        %ul
            $forall children.person child
                %li $child$
        %p
            %a!href=@page.person@ See the page.
        ^footer^
|]


-- | some dummy content to use with the template
person :: Person
person = Person
            { name = return $ Unencoded $ pack "Michael"
            , age = Unencoded $ pack "twenty five & a half"
            , page = PersonPage "michael"
            , isMarried = True
            , children = [ Unencoded $ pack "Adam"
                         , Unencoded $ pack "Ben"
                         , Unencoded $ pack "Chris"
                         ]
            }

-- | example of using 'hamletToResponse' to turn the 'Hamlet' template
-- into a 'ServerPartT IO Response'.
--
-- NOTE: we ignore the requested URL in this example for simplicity
main :: IO ()
main = simpleHTTP nullConf $ liftIO $ hamletToResponse renderUrls $ template person
