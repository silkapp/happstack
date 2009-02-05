{-# LANGUAGE TemplateHaskell, FlexibleInstances, UndecidableInstances -- just for example at the bottom
    ,CPP, DeriveDataTypeable, MultiParamTypeClasses
  #-}
module HAppS.Data.Pairs (pairsToXml,xmlToPairs,pairsToHTMLForm,xmlToHTMLForm
                        ,toPairs,toPairsX,fromPairs,toHTMLForm
                        ,Pairs,AsPairs
                        ) where

import Data.Char
import Data.List
import Data.Maybe

---stuff for examples
import HAppS.Data.DeriveAll
import HAppS.Util.Common

import Data.Generics as G
import HAppS.Data.Default -- for pairs
import HAppS.Data.Xml
import Control.Monad.Identity

type Pairs = [(String,String)]

pairsToXml :: Pairs -> [Element]
pairsToXml pairs = fst $ formIntoEls "" $ map slash pairs

slash :: (String,t) -> (String,t)
slash p@('/':_,_) = p
slash (n,v) = ('/':n,v)

formIntoEls :: String -> Pairs -> ([Element], Pairs)
formIntoEls _ [] = ([],[])
formIntoEls ctx pairs@((name,val):rest)
    | not $ isPrefixOf ctx name = ([],pairs)
    | isAttr = moreCtx $ Attr elName val
    | isLeaf = moreCtx $ Elem elName [CData val]
    | otherwise =
        let (es,pairs') = formIntoEls ctx' pairs
            (es',pairs'') = formIntoEls ctx pairs'
        in
        (Elem elName es:es',pairs'')
    where
    ctx' = ctx ++ "/" ++ top
    node =  tail $ drop (length ctx) name
    (top,subs) = break (=='/') $  node
    elName = takeWhile (/='[') top
    isLeaf = null subs
    isAttr = head top == '@'
    moreCtx el = let (restCtx,restPairs) = formIntoEls ctx rest
                 in (el:restCtx,restPairs)
xmlToPairs :: [Element] -> Pairs
xmlToPairs =
    map (\(x,y)->(tail x,y)) .
    xmlIntoPairs 0 ""

xmlIntoPairs :: Int -> String -> [Element] -> Pairs
xmlIntoPairs _ _ [] = []
xmlIntoPairs x ctx (Elem _ []:xs) = xmlIntoPairs x ctx xs
xmlIntoPairs x ctx (Attr n v:xs) = (ctx++"/"++n,v):xmlIntoPairs x ctx xs
xmlIntoPairs _ ctx (CData v:[]) = [(ctx,v)]
xmlIntoPairs _ _ (CData _:_) = []
xmlIntoPairs i ctx ((Elem n xs):xs') =
    thisElPairs ++ restPairs
    where
    --if we have two elements with the same name then we have to add [i]
    nIndex = n ++ "[" ++ (show i) ++ "]"
    (iNext,nName)
        | i/=0 = (i+1,nIndex)
        | null xs' = (0,n)
        | next==n = (i+1,nIndex)
        | otherwise = (0,n)
    Elem next _ = head xs'
    thisElPairs = (xmlIntoPairs 0 (ctx++"/"++nName) xs)
    restPairs = (xmlIntoPairs iNext ctx xs')

pairsToHTMLForm :: String -> String -> String -> Pairs -> [Element]
pairsToHTMLForm iden action method pairs
 = [Elem "form" (Attr "action" action :
                Attr "id" iden :
                Attr "method" method :
                map pToInput pairs ++
                [submitButton])]

submitButton :: Element
submitButton = Elem "input" [Attr "type" "submit"]

pToInput :: (String,String) -> Element
pToInput (n,v)=
    Elem "div" [Attr "class" "formEl",
                Elem "span" [Attr "class" "name"
                            ,CData $ map (\x->if x=='/' then ' ' else x) n]
               ,Elem "input" [Attr "name" n,Attr "value" v]]

xmlToHTMLForm :: (Xml a, Show a, Data a, Eq a) =>
                 String -> String -> String -> a -> [Element]
xmlToHTMLForm iden method action
 = pairsToHTMLForm iden method action  . toPairsX -- xmlToPairs

class (Xml x,Show x, G.Data x) => AsPairs x where
    toPairs::x->Pairs
    fromPairs::Pairs -> Maybe x

instance (Xml a,Show a,G.Data a,Eq a) => AsPairs a where
    toPairs x = xmlToPairs $ toPublicXml x
    fromPairs [] = Nothing
    fromPairs pairs = if res == dv && notRigidMatch then Nothing else Just res
        where
        xml = pairsToXml $ mapFst clean pairs
        res = runIdentity $ fromXml Flexible xml
        mbRigidMatch = fromXml Rigid xml
        _ = [mbRigidMatch,Just res]
        isRigidMatch = isJust mbRigidMatch
        notRigidMatch = not isRigidMatch
        dv = defaultValue
        (cons,_) = break (==' ') $ show dv
        clean n = if (map toLower parent)==(map toLower cons) then n
                  else if head n =='/' then n
                  else (cons++('/':name))
            where
            name = trimSlash n
            (parent,_) = break (=='/') name
        trimSlash n = if head n=='/' then tail n else n

toPairsX :: (Xml a, Show a, Data a, Eq a) => a -> Pairs
toPairsX x = map (\(n,v)->let (_,child)=break (=='/') n in
                            if null child then (n,v) else (tail child,v)) $ toPairs x

toHTMLForm :: (Xml a, Show a, Data a, Eq a) =>
              String -> String -> String -> a -> [Element]
toHTMLForm iden method action = xmlToHTMLForm iden method action 



--- example usage and tests here
$( deriveAll [''Show,''Default,''Eq]
   [d|
       data UserInfo = UserInfo User Pass 
       newtype User = User String 
       newtype Pass = Pass String 
    |]
 )
