{-# LANGUAGE CPP, TemplateHaskell, UndecidableInstances, DeriveDataTypeable #-}
module HAppS.Data.Atom 
    (
     Entry(..)
    ,Feed(..)
    ,Author(..)
    ,Contributor(..)
    ,Category(..)
    ,Id(..)
    ,Title(..)
    ,Published(..)
    ,Updated(..)
    ,Summary(..)
    ,Content(..)
    ,Word(..)
    ,Email(..)


)
    where

import HAppS.Data


#ifndef __HADDOCK__
$( deriveAll [''Ord,''Eq,''Read,''Show,''Default] 
   [d|
    data Feed = Feed [Entry]

    data Entry = 
        Entry
        [Author]                                                                                                                                              
        [Category]  
        (Maybe Content )
        [Contributor]
        Id
        
        -- Link is a computed value
        (Maybe Published)
        -- ignoring stuff I don't care about
        (Maybe Summary)
        Title
        Updated

       {--

        Entry 

        --stuff not in the spec but commonly useful for management
        --a -- sometime you want to stick other data in here
        --(Maybe Owner) -- not in spec but useful for actual management
        --(Maybe Refs) -- for use when Entry is actually a comment on another 

        Author -- we will have only one author
        [Contributor] -- author can credit contributors
        [Category] -- allow user to select categories
        Id 
        -- Link is a computed value
        Title
        Updated 
        Maybe Published
        -- (Maybe Rights)
        Maybe Summary -- We choose to always have a summary
        Maybe Content -- We choose to always have content
        --}

    --data PersonConstruct = PersonConstruct Name (Maybe URI) (Maybe Email)
    type PersonConstruct = (Name, Maybe Uri, Maybe Email)
    data Author = Author PersonConstruct
    data Contributor = Contributor PersonConstruct
    newtype Name = Name Text 
    --type MbRefs = Maybe Refs

    --newtype Owner = Owner String

    data Category = Category Term (Maybe Scheme) (Maybe Label) 
    newtype Term = Term String 
    newtype Scheme = Scheme String 
    newtype Label = Label String 

    newtype Id = Id Integer 
    --newtype Refs = Refs Integer

    type DateConstruct = Integer

    data Title = Title TextConstruct 
    data Subtitle = Subtitle TextConstruct 
    newtype Summary = Summary TextConstruct 
    newtype Content = Content TextConstruct 

    newtype Uri = Uri String 
    newtype Email = Email String 
    newtype Updated = Updated DateConstruct
    newtype Published = Published DateConstruct

    type Text = String
    type TextConstruct = Text

    newtype Word = Word String
 |] )

#define V(x) instance Version x
V(Uri);V(Email);V(Updated);V(Published);V(Title);V(Subtitle)
V(Summary);V(Content);V(Author);V(Contributor);V(Name);V(Category)
V(Term);V(Scheme);V(Label);V(Id)
$(deriveSerializeFor [''Uri, ''Email, ''Updated, ''Published
                     ,''Title, ''Subtitle, ''Summary, ''Content
                     ,''Author, ''Contributor, ''Name, ''Category
                     ,''Term, ''Scheme, ''Label, ''Id])

#endif

