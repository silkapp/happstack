module HAppS.Data.GOps where
import Data.Generics hiding (GT)
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe

-- useful generic functions with better names

gSet :: (Data b, Typeable a) => a -> b -> b
gSet v = gReplace (const v)

gReplace :: (Typeable a, Data b) => (a -> a) -> b -> b
gReplace f x = everywhere (mkT f) x

gFind :: (MonadPlus m, Data a, Typeable b) => a -> m b
gFind x = msum $ map return $ listify (const True) x

gFind' :: (Data a, Typeable b) => a -> b
gFind' x = fromJust $ gFind x
--Monad versions

gModify :: (MonadState s m,Typeable a,Data s) => (a->a) -> m ()
gModify f = modify $ gReplace f

gAsk :: (Data r, Typeable a, MonadReader r m, MonadPlus n) =>
        (a -> n b) -> m (n b)
gAsk f = do st <- ask
            let y = gFind st 
            return $ maybe mzero f y

gGet :: (Data s, Typeable a, MonadState s m, MonadPlus n) =>
        (a -> n b) -> c -> m (n b)
gGet f _ = do st <- get
              let y = gFind st 
              return $ maybe mzero f y

