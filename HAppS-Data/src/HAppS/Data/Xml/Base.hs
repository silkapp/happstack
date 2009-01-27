{-# LANGUAGE TemplateHaskell, FlexibleInstances,
             OverlappingInstances, UndecidableInstances, CPP,
             ScopedTypeVariables, GADTs,
             PolymorphicComponents, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             PatternGuards, PatternSignatures #-}

module HAppS.Data.Xml.Base where

import Control.Monad.Identity
import Control.Monad.State
import Data.Char
import Data.List
import Data.Generics.SYB.WithClass.Basics
import Data.Generics.SYB.WithClass.Instances ()
import Data.Maybe
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import HAppS.Data.Default
import HAppS.Data.DeriveAll
import HAppS.Data.Migrate
import HAppS.Data.Normalize
import HAppS.Util.TH
import Language.Haskell.TH
import qualified Data.Generics as G

$(deriveAll [''Default, ''Eq,''Read,''Ord] [d|
    data Element = Elem String [Element]
                 | CData String
                 | Attr String String
 |])

insEl :: (Data XmlD a, Default a, Data NormalizeD a,
          Data XmlD b, Default b, Data NormalizeD b) =>
         a -> b -> Element
insEl a b = case toXml b  of
            (Elem n xs:_) -> Elem n $ toPublicXml a ++ xs
            _ -> error "can't insert a into b"


-- This is a more readable representation than the default, but is still
-- Haskell syntax
instance Show Element where
    show (Elem s es) = "Elem " ++ show s ++ " ["
                       ++ fiddle (unlines (indent (concatMap lines $ comma $ map show es)))
                       
                      ++ "]"
        where indent = map ("    " ++)
              comma::[String]->[String]
              comma [] = []
              comma (x:xs) = (' ':x):map (',':) xs
              fiddle "" = ""
              fiddle xs = '\n' : (if last xs == '\n' then init xs else xs)
    show (CData s) = "CData " ++ show s
    show (Attr k v) = "Attr " ++ show k ++ " " ++ show v

fromXml :: forall m a . (Monad m, Xml a) => Rigidity m -> [Element] -> m a
fromXml r xs = case readXml r xs of
               Just (_, v) ->
                   return v
               Nothing ->
                   case r of
                   Rigid -> fail "fromXml XXX"
                   Flexible -> return defaultValue

data Other b = forall a . (Migrate a b, Xml a) => Other a
             | NoOther

toPublicXml :: Xml a => a -> [Element]
toPublicXml x = clean $ toXml x
    where
    clean [] = []
    clean ((Elem n xs):rest) = (Elem n $ clean xs): clean rest
    clean (CData s:rest)=CData s:clean rest
    clean (Attr n v:rest) = if n `elem` [typeAttr,versionAttr] then clean rest
                            else Attr n v:clean rest

data Rigidity m where
    Rigid :: Rigidity Maybe
    Flexible :: Rigidity Identity

instance Show (Rigidity m) where
    show Rigid = "Rigid"
    show Flexible = "Flexible"

class (Data XmlD a,
       Default a, -- We'd rather have this only in the Flexible case,
                  -- but bugs in GHC 6.6.1 and problems getting the
                  -- instance for child types in constrFromElements
                  -- mean it's a constraint of the Xml class for now.
       Normalize a)
   => Xml a where
    toXml :: a -> [Element]
    toXml = defaultToXml

    -- readXml is like readXml' except it normalises the Elements and
    -- the result
    readXml :: Monad m => Rigidity m -> [Element] -> Maybe ([Element], a)
    readXml = defaultReadXml

    readXml' :: Monad m => Rigidity m -> [Element] -> Maybe ([Element], a)
    readXml' = defaultReadXml'

    normalizeXml :: a{- can't look at this value -} -> [Element] -> [Element]
    normalizeXml _ = id

    version :: a{- can't look at this value -} -> Maybe String
    version _ = Just "0"

    otherVersion :: a{- can't look at this value -} -> Other a
    otherVersion _ = NoOther

    typ :: a{- can't look at this value -} -> String
    typ _ = dataTypeName (dataTypeOf xmlProxy (undefined :: a))

instance (Data XmlD t, Default t, Normalize t) => Xml t

data XmlD a = XmlD { toXmlD :: a -> [Element],
                     readMXmlD :: forall m . Monad m
                               => Rigidity m -> ReadM m a,
                     readMXmlNoRootDefaultD :: forall m . Monad m
                                            => Rigidity m -> ReadM Maybe a }

xmlProxy :: Proxy XmlD
xmlProxy = error "xmlProxy"

instance Xml t => Sat (XmlD t) where
    dict = XmlD { toXmlD = toXml,
                  readMXmlD = readMXml,
                  readMXmlNoRootDefaultD = readMXmlNoRootDefault }

first :: (a -> a) -> [a] -> [a]
first _ [] = []
first f (x:xs) = f x : xs

defaultToXml :: Xml t => t -> [Element]
defaultToXml x
 = let me = first toLower $ constring $ toConstr xmlProxy x
       rest = Attr typeAttr (dataTypeName (dataTypeOf xmlProxy x)) :
            transparentToXml x
       rest' = case version x of
                   Nothing -> rest
                   Just v -> Attr versionAttr v : rest
   in [Elem me rest']

transparentToXml :: Xml t => t -> [Element]
transparentToXml x = concat $ gmapQ xmlProxy (toXmlD dict) x

transparentReadXml :: forall m t . (Monad m, Xml t)
                   => Rigidity m -> [Element] -> Maybe ([Element], t)
transparentReadXml r es
 = aConstrFromElements r (dataTypeConstrs (dataTypeOf xmlProxy resType)) es
   where resType :: t
         resType = typeNotValue resType

transparentXml :: Name -> Q [Dec]
transparentXml n
 = do i <- reify n
      case i of
          TyConI (DataD _ _ vs _ _) ->
              do argNames <- replicateM (length vs) (newName "a")
                 let args = map varT argNames
                     mkXml a = conT ''Xml `appT` a
                     ctxt = cxt $ map mkXml args
                     instanceHead = mkXml $ foldl appT (conT n) args
                     decs = [d|
                                toXml :: Xml a => a -> [Element]
                                toXml = transparentToXml

                                readXml :: (Monad m, Xml a)
                                        => Rigidity m -> [Element]
                                        -> Maybe ([Element], a)
                                readXml = transparentReadXml
                              |]
                 d <- instanceD' ctxt instanceHead decs
                 return [d]
          _ ->
              fail ("transparentXml: Not given a type constructor's name: " ++
                    show n)

-- Don't do any defaulting here, as these functions can be implemented
-- differently by the user. We do the defaulting elsewhere instead.
-- The t' type is thus not used.

defaultReadXml :: (Monad m, Xml t)
               => Rigidity m -> [Element] -> Maybe ([Element], t)
defaultReadXml r es = res
    where res = case readXml' r $ normalizeXml valType es of
                    Nothing -> Nothing
                    Just (es', v) -> Just (es', normalize v)
          valType = snd $ fromJust res

defaultReadXml' :: (Monad m, Xml t)
                => Rigidity m -> [Element] -> Maybe ([Element], t)
defaultReadXml' = readXmlWith readVersionedElement

readXmlWith :: Xml t
            => (Rigidity m -> Element -> Maybe t)
            -> Rigidity m
            -> [Element]
            -> Maybe ([Element], t)
readXmlWith f r@Rigid es = case es of
                               e : es' ->
                                   case f r e of
                                       Just v -> Just (es', v)
                                       Nothing -> Nothing
                               [] ->
                                   Nothing
readXmlWith f r@Flexible es = readXmlWith' [] es
    where readXmlWith' acc (x:xs)
           = case f r x of
                 Nothing -> readXmlWith' (x:acc) xs
                 Just v -> Just (reverse acc ++ xs, v)
          readXmlWith' _ [] = Nothing

readVersionedElement :: forall m t . (Monad m, Xml t)
                     => Rigidity m -> Element -> Maybe t
readVersionedElement r (Elem n es)
    = case getAttr typeAttr es of
      Nothing ->
          readElement r (Elem n es)
      Just (t, es')
       | t == typ resType ->
          case version resType of
          Nothing ->
              readElement r (Elem n es')
          Just v ->
              case getAttr versionAttr es' of
              Nothing -> readElement r (Elem n es')
              Just (v', es'')
               | v == v' -> readElement r (Elem n es'')
               | otherwise ->
                  case otherVersion resType of
                  NoOther ->
                      Nothing
                  Other (_ :: u) ->
                      case readVersionedElement r (Elem n es'') of
                      Just (res :: u) ->
                          Just (migrate res)
                      Nothing -> Nothing
       | otherwise ->
          Nothing
    where resType :: t
          resType = typeNotValue resType
readVersionedElement _ _ = Nothing

isTheAttr :: String -> Element -> Bool
isTheAttr a (Attr k _) = a == k
isTheAttr _ _          = False

getAttr :: String -> [Element] -> Maybe (String, [Element])
getAttr a es = case break (isTheAttr a) es of
                (prefix, Attr _ v : suffix) -> Just (v, prefix ++ suffix)
                _ -> Nothing

versionAttr :: String
versionAttr = "haskellTypeVersion"

typeAttr :: String
typeAttr = "haskellType"


readElement :: forall m t . (Monad m, Xml t) => Rigidity m -> Element -> Maybe t
readElement r (Elem n es) = res
    where resType = dataTypeOf xmlProxy (undefined :: t)
          res = case readConstr resType $ first toUpper n of
                Just c -> f c
                Nothing -> if endsWithNum n then readElement r (Elem (noNum n) es) else Nothing
          f :: Constr -> Maybe t
          f c =     let m :: m ([Element], t)
                        m = constrFromElements r c es
                    in case r of
                       Rigid -> case m of
                                    Just ([], x) -> Just x
                                    _ -> Nothing
                       Flexible -> case runIdentity m of
                                       -- We ignore left over elements
                                       (_, x) -> Just x
          endsWithNum m = head (reverse m) `elem` "0123456789"
          noNum  = reverse . dropWhile (`elem` "012344566789") . reverse 

readElement _ _ = Nothing

-- When just trying all the constructors of a type, if defaulting is
-- allowed we would always get the first constructor as all of its
-- arguments could be defaulted. Therefore we have the choice of
--  * accepting this
--  * turning off defaulting for this level only
--  * turning off defaulting recursively
-- We choose the second option, and thus have to duplicate
-- constrFromElements and readXml(D).
aConstrFromElements :: forall m t . (Monad m, Xml t)
                    => Rigidity m -> [Constr] -> [Element]
                    -> Maybe ([Element], t)
aConstrFromElements r cs es
 = msum [ constrFromElementsNoRootDefault r c es | c <- cs ]


constrFromElementsNoRootDefault :: forall m t . (Monad m, Xml t)
                                => Rigidity m -> Constr -> [Element]
                                -> Maybe ([Element], t)
constrFromElementsNoRootDefault r c es
 = do let st = ReadState { xmls = es }
          m :: ReadM Maybe t
          m = fromConstrM xmlProxy (readMXmlNoRootDefaultD dict r) c
      -- XXX Should we flip the result order?
      (x, st') <- runStateT m st
      return (xmls st', x)

constrFromElements :: forall m t . (Monad m, Xml t)
                   => Rigidity m -> Constr -> [Element]
                   -> m ([Element], t)
constrFromElements r c es
 = do let st = ReadState { xmls = es }
          m :: ReadM m t
          m = fromConstrM xmlProxy (readMXmlD dict r) c
      -- XXX Should we flip the result order?
      (x, st') <- runStateT m st
      return (xmls st', x)

type ReadM m = StateT ReadState m

data ReadState = ReadState {
                     xmls :: [Element]
                 }

getXmls :: Monad m => ReadM m [Element]
getXmls = do st <- get
             return $ xmls st

putXmls :: Monad m => [Element] -> ReadM m ()
putXmls xs = do st <- get
                put $ st { xmls = xs }

readMXml :: (Monad m, Xml a) => Rigidity m -> ReadM m a
readMXml r
 = do xs <- getXmls
      case readXml r xs of
          Nothing ->
              case r of
              Rigid -> fail "Cannot read value"
              Flexible -> return defaultValue
          Just (xs', v) ->
              do putXmls xs'
                 return v

readMXmlNoRootDefault :: (Monad m, Xml a) => Rigidity m -> ReadM Maybe a
readMXmlNoRootDefault r
 = do xs <- getXmls
      case readXml r xs of
          Nothing -> fail "Cannot read value"
          Just (xs', v) ->
              do putXmls xs'
                 return v

xmlAttr :: Name -> Q [Dec]
xmlAttr newTypeName
 = do i <- reify newTypeName
      case i of
          TyConI (NewtypeD _ n _ (NormalC c [(_, ConT t)]) _)
           | t == ''ByteString -> mkDecs n c t
          _ -> fail "xmlAttr: Didn't get what I wanted"

    where mkDecs n c t =
            do let x = mkName "x"
                   f = mkName "f"
                   cstr = stringL $ first toLower $ nameBase c
                   toFun = funD
                             'toXml
                             [clause
                                 [conP c [varP x]]
                                 (normalB [| [Attr $(litE cstr)
                                                   $ BS.unpack $(varE x)] |])
                                 []]

                   readFun = funD
                             'readXml
                             [clause
                                 []
                                 (normalB [| readXmlWith $(varE f) |])
                                 [readHelper]]

                   readHelper
                    = funD f
                           [
                            clause [conP 'Attr [litP cstr, (varP x)]]
                                   (normalB [| Just $ $(conE c)
                                                    $ BS.pack $(varE x) |])
                                   [],
                            clause [wildP]
                                   (normalB [| Nothing |])
                                   []
                           ]
               inst <- instanceD (cxt [])
                                 ( conT ''Xml `appT` conT n)
                                 [toFun, readFun]
               return [inst]

xmlShowCDatas :: [Name] -> Q [Dec]
xmlShowCDatas = liftM concat . mapM xmlShowCData

xmlShowCData :: Name -> Q [Dec]
xmlShowCData newTypeName
 = do d <- instanceD' (cxt [])
                      (conT ''Xml `appT` conT newTypeName)
                      [d|
                          toXml :: (Show a, Xml a) => a -> [Element]
                          toXml x = [CData $ show x]

                          readXml :: (Read a, Xml a)
                                  => Rigidity m -> [Element]
                                  -> Maybe ([Element], a)
                          readXml = readXmlWith f
                              where f _ (CData x)
                                     | [(v, "")] <- reads x = Just v
                                    f _ _ = Nothing
                        |]
      return [d]

xmlCDataLists :: [Name] -> Q [Dec]
xmlCDataLists = liftM concat . mapM xmlCDataList

xmlCDataList :: Name -> Q [Dec]
xmlCDataList newTypeName
 = do d <- instanceD' (cxt [])
                      (conT ''Xml `appT` (listT `appT` conT newTypeName))
                      [d|
                        toXml :: (Show a, Xml a) => [a] -> [Element]
                        toXml xs = [CData $ concat $ intersperse ","
                                          $ map show xs]

                        readXml :: (Read a, Xml a)
                                => Rigidity m -> [Element]
                                -> Maybe ([Element], [a])
                        readXml = readXmlWith f
                            where f _ (CData x) =
                                      let list = words $ noCommas x
                                          is = concatMap reads list
                                      in if length is == length list
                                         then Just $ map fst is
                                         else Nothing
                                  f _ _ = Nothing
                       |]
      return [d]

noCommas :: String -> String
noCommas = map (\x -> if x == ',' then ' ' else x)

typeNotValue :: Xml a => a -> a
typeNotValue t = error ("Type used as value: " ++ typeName)
    where typeName = dataTypeName (dataTypeOf xmlProxy t)

