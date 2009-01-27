{-# LANGUAGE TemplateHaskell, CPP #-}
module HAppS.State.Util
    ( -- * Random numbers
     getRandom, getRandomR,
      -- * TH helpers
     inferRecordUpdaters
    ) where

import Control.Concurrent.STM
import Control.Monad.State
import System.Random

import HAppS.State.Monad
import HAppS.State.Types

import Data.Char(toUpper)
import Language.Haskell.TH


-- Random numbers

-- | Get a random number.
getRandom :: Random a => AnyEv a
getRandom = do r <- sel evRandoms
               g <- liftSTM $ readTVar r
               let (x,g') = random g
               liftSTM $ writeTVar r g'
               return x

-- | Get a random number inside the range.
getRandomR :: Random a => (a,a) -> AnyEv a
getRandomR z = do r <- sel evRandoms
                  g <- liftSTM $ readTVar r
                  let (x,g') = randomR z g
                  liftSTM $ writeTVar r g'
                  return x

--------------------------------------------------------------
-- inferRecordUpdater
--------------------------------------------------------------


-- FIXME: Throw a decent error message if the input isn't a record.
-- | Infer updating functions for a record @a_foo :: component -> record -> record@ and
--   @withFoo = localState foo a_foo@.
inferRecordUpdaters :: Name -> Q [Dec]
inferRecordUpdaters typeName = do
    con <- decToSimpleRecord =<< nameToDec typeName
    let c name upd sel = 
            do let un = mkName ("a_"++ns)
                   wn = mkName ("with"++(toUpper (head ns):tail ns))
                   ns = nameBase name
               ud <- un `sdef` upd
               wd <- wn `sdef` (varE 'localState `appE` sel `appE` varE un)
               return [ud, wd]
    xs <- sequence $ zipWith3 c (fieldNames con) (updFuns con) (selFuns con)
    return $ concat xs


-- Utilities

decToSimpleRecord :: Dec -> Q Con
decToSimpleRecord (DataD _ _ _ [con] _)  = return con
decToSimpleRecord (DataD _ n _ _     _)  =
    fail ("Not a simple record (has multiple constructors): "++show n)
decToSimpleRecord (NewtypeD _ _ _ con _) = return con
decToSimpleRecord x = fail ("Wanted a simple record, got: "++show x)

nameToDec :: Name -> Q Dec
nameToDec ty = reify ty >>= un
    where un (TyConI d) = return $ d
          un _          = fail "nameToDec: expected TyCon"

-- | Create a list of selection functions for a record.
selFuns :: Con -> [ExpQ]
selFuns (RecC _ ts) = [ varE n | (n,_,_) <- ts ]

-- | Create a list of update functions for a record.
updFuns :: Con -> [ExpQ]
updFuns (RecC _ ts) = [ upd n | (n,_,_) <- ts ]
    where [x,y] = map mkName ["x","y"]
          upd f = lamE [varP x, varP y] $ rup f
          rup f = recUpdE (varE y) [return (f,VarE x)]


-- | Return field names
fieldNames (RecC _ ts) = [ n | (n,_,_) <- ts ]

-- | Simple definition
sdef vn ve = valD (varP vn) (normalB ve) []


--------------------------------------------------------------
-- inferRecordUpdater end
--------------------------------------------------------------

