{-# LANGUAGE TemplateHaskell, FlexibleInstances,
             UndecidableInstances, OverlappingInstances,
             MultiParamTypeClasses, CPP, FunctionalDependencies #-}

module Happstack.Data.HList (HasT, hlextract, hlupdate, (.&.),
                         (:&:),
                         Couple(..),Nil(..),CoupleClass,hMap,trans) where
-- HList useful with generic

import Happstack.Data.Xml
import Happstack.Data.Pairs
import Data.Generics as G
import Happstack.Data.HListBase

infixr 6 .&.
(.&.) :: a -> b -> Couple a b
(.&.) = Couple

type a :&: b = Couple b a



class CoupleClass a where
    toPairs' :: a -> Pairs
    fromPairs' :: Pairs -> Maybe a

instance (Eq a,Xml a, Show a, G.Data a,CoupleClass b) => CoupleClass (Couple a b) where
    toPairs' (Couple a b) = (toPairs a) ++ (toPairs' b)
    fromPairs' = const Nothing --doesn't make sense to unmix to make toPairs' . fromPairs' an identity

instance CoupleClass Nil where
    toPairs' _ = []
    fromPairs' _ = return Nil

instance (Xml a, Xml b) => Xml (Couple a b) where
    toXml (Couple a b) = (toXml a) ++ (toXml b)
    readXml r xml = do
                  (xml', a) <- readXml r xml
                  (xml'', b) <- readXml r xml'
                  return (xml'', Couple a b)

hlextract :: HasT a b => a -> b
hlextract = x

hlupdate :: HasT a b => a -> b -> a
hlupdate = u

class HasT a b where
    x :: a -> b
    u :: a -> b -> a

instance HasT (Couple a b) a  where
    x (Couple a _) = a
    u (Couple _ b) a = Couple a b

instance HasT (Couple a b) b where
    x (Couple _ b) = b
    u (Couple a _) b = Couple a b

-- Oleg's trick http://www.haskell.org/pipermail/haskell/2004-June/014176.html
class HasT' a b where 
    x' :: a -> b
    u' :: a -> b -> a

instance HasT' a b => HasT a b where
    x = x'
    u = u'

instance (HasT c a) => HasT' (Couple b c) a where
    x' (Couple _ b) = x b
    u' (Couple a b) c = Couple a (u b c)

class Trans ft a where
    trans :: ft -> a -> a

instance Trans (a->a) (Couple a b) where
    trans f (Couple a b) = Couple (f a) b

instance Trans (b->b) (Couple a b) where
    trans f (Couple a b) = Couple a (f b) 

class Trans' ft a where
    trans' :: ft -> a ->a

instance Trans' ft a => Trans ft a where
    trans = trans'

instance (Trans ft b) => Trans' ft (Couple a b) where
    trans' f (Couple a b) = Couple a (trans f b)

class HMap a b | a -> b where
    hMap::a->b

instance (HMap b d,CoupleClass b) => HMap (Couple a b) (Couple [a] d) where
    hMap (Couple a b) = Couple [a] $ hMap b 

instance HMap (Couple a Nil) (Couple [a] Nil) where
    hMap (Couple a Nil) = Couple [a] Nil
