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
import Data.Typeable

import Data.Generics as G
import HAppS.Data.Default -- for pairs
import HAppS.Data.Xml
import Control.Monad.Identity

pairsToXml pairs = fst $ formIntoEls "" $ map slash pairs
slash p@('/':n,v) = p
slash (n,v) = ('/':n,v)


type Pairs = [(String,String)]
formIntoEls :: String -> Pairs -> ([Element], Pairs)
formIntoEls ctx [] = ([],[])
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

xmlToPairs =
    map (\(x,y)->(tail x,y)) .
    xmlIntoPairs 0 ""
--    where stripInitialSlashes = map (\(x,y)->(tail x,y))

xmlIntoPairs x ctx [] = []
xmlIntoPairs x ctx (Elem n []:xs) = xmlIntoPairs x ctx xs
xmlIntoPairs x ctx (Attr n v:xs) = (ctx++"/"++n,v):xmlIntoPairs x ctx xs
xmlIntoPairs x ctx (CData v:[]) = [(ctx,v)]
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

pairsToHTMLForm iden action method pairs
 = [Elem "form" (Attr "action" action :
                Attr "id" iden :
                Attr "method" method :
                map pToInput pairs ++
                [submitButton])]
submitButton = Elem "input" [Attr "type" "submit"]
pToInput (n,v)=
    Elem "div" [Attr "class" "formEl",
                Elem "span" [Attr "class" "name"
                            ,CData $ map (\x->if x=='/' then ' ' else x) n]
               ,Elem "input" [Attr "name" n,Attr "value" v]]

xmlToHTMLForm iden method action
 = pairsToHTMLForm iden method action  . toPairsX -- xmlToPairs

class (Xml x,Show x, G.Data x) => AsPairs x where
    toPairs::x->Pairs
    fromPairs::Pairs -> Maybe x

instance (Xml a,Show a,G.Data a,Eq a) => AsPairs a where
    toPairs x = xmlToPairs $ toPublicXml x
    fromPairs [] = Nothing
    fromPairs pairs = --if res == Just defaultValue then Nothing else res
                      if res == dv && notRigidMatch then Nothing else Just res
        where
        xml = pairsToXml $ mapFst clean pairs
        res = runIdentity $ fromXml Flexible xml
        mbRigidMatch = fromXml Rigid xml
        _ = [mbRigidMatch,Just res]
        isRigidMatch = isJust mbRigidMatch
        notRigidMatch = not isRigidMatch
        dv = defaultValue
        --fromJust $ head [Just defaultValue,res]
        (cons,_) = break (==' ') $ show dv
        clean n = if (map toLower parent)==(map toLower cons) then n
                  else if head n =='/' then n
                  else (cons++('/':name))
            where
            name = trimSlash n
            (parent,child) = break (=='/') name
        trimSlash n = if head n=='/' then tail n else n
{--
    fromPairs pairs = res
        where
        res = if (map toLower parent)==(map toLower cons)
              then fromXml $ pairsToXml pairs
              else fromPairs $ 
                   --(error . ((show (parent,cons,name))++) . show) $ 
                   map (\(n,v)-> if head n=='/' then (n,v) 
                                 else (cons++('/':trimSlash n),v))
                   pairs
        dv = fromJust $ head [Just defaultValue,res]
        (n,_) = head pairs
        name = trimSlash n

        (parent,child) = break (=='/') name
        (cons,_) = break (==' ') $ show dv
--}


--    fromPairs ps = fromPairs' ps
--fromPairs' :: (Xml b, Show b) => [([Char], String)] -> Maybe b


--instance AsPairs Element where
--    toPairs x = xmlToPairs [x]
--    fromPairs = head . pairsToXml

--    fromPairs x = fromXml $ pairsToXml x


toPairsX x = map (\(n,v)->let (par,child)=break (=='/') n in
                              if null child then (n,v) else (tail child,v)) $ toPairs x


toHTMLForm iden method action = xmlToHTMLForm iden method action 



#ifndef __HADDOCK__
--- example usage and tests here
$( deriveAll [''Show,''Default,''Eq]
   [d|
       data UserInfo = UserInfo User Pass 
       newtype User = User String 
       newtype Pass = Pass String 
    |]
 )
#endif

x = toPairs (defaultValue::UserInfo)

--  It would be nice if every Xml type was fromData
--instance FromData UserInfo

v = toHTMLForm "defForm" "GET" "method" $ (defaultValue::UserInfo)

t1 = toPairs $ UserInfo (User "alex") (Pass "pass")

t2pairs = [("userInfo/user","alex"),("userInfo/pass","pass")]
t2 = fromPairs t2pairs ::Maybe UserInfo

t3pairs = [("user","alex"),("pass","pass")]
t3 = fromPairs t3pairs  ::Maybe UserInfo

t4 = toPairsX $ UserInfo (User "alex") (Pass "pass")
t5 = fromPairs t4::Maybe UserInfo

t6pairs = [("/userInfo/user","alex"),("/userInfo/pass","pass")]
t6 = fromPairs t6pairs ::Maybe UserInfo

-- these are nothing because if there is no information in the pairs then 
-- it is a defaultValue and that is bogus
t7 = (fromPairs [("/blag","alex"),("hbli","mypass")]::Maybe UserInfo)  ==  Nothing
t8 = (fromPairs [("/blag","alex"),("/hbli","mypass")]::Maybe UserInfo) == Nothing

t9 = (fromPairs [("pass","alex"),("/hbli","mypass")]::Maybe UserInfo) == 
     (Just $ UserInfo defaultValue (Pass "alex"))


