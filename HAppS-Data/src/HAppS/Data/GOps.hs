module HAppS.Data.GOps where
import Data.Generics hiding (GT)
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
-- useful generic functions with better names
gSet v = gReplace (const v)
gReplace f x = everywhere (mkT f) x
--gFind1 x = let res = gFind x in if null res then Nothing else head res
gFind x = msum $ map return $ listify (const True) x
--gFindMb x = gFind x::(Typeable a) => Maybe a
gFind' x = fromJust $ gFind x
--Monad versions
gModify f = modify $ gReplace f
gAsk f = do st <- ask
            let y = gFind st 
            return $ maybe mzero f y

gGet f x = do st <- get
              let y = gFind st 
              return $ maybe mzero f y

