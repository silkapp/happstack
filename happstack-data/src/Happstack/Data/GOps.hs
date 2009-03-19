module Happstack.Data.GOps where
import Data.Generics hiding (GT)
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe

-- useful generic functions with better names

-- | @gSet x y@ will traveral @x@ and replace 
-- any instances of the type @a@ in its structure 
-- with @y@.
gSet :: (Data b, Typeable a) => a -> b -> b
gSet = gReplace . const

-- | @gReplace f b@ will traverse @x@ and will act on
-- any instance of the type @a@ in its structure with
-- the function @f@.
gReplace :: (Typeable a, Data b) => (a -> a) -> b -> b
gReplace f x = everywhere (mkT f) x

-- | @gFind a@ will extract any elements of type @b@ from
-- @a@'s structure in accordance with the MonadPlus
-- instance, e.g. Maybe Foo will return the first Foo
-- found while [Foo] will return the list of Foos found.
gFind :: (MonadPlus m, Data a, Typeable b) => a -> m b
gFind = msum . map return . listify (const True)

-- | Acts as gFind but will throw an exception if
-- nothing is found.
gFind' :: (Data a, Typeable b) => a -> b
gFind' = fromJust . gFind
--Monad versions

-- | A generalized modify that will apply the modification
-- function to the structure of the state.
gModify :: (MonadState s m,Typeable a,Data s) => (a->a) -> m ()
gModify = modify . gReplace

-- | A generalized ask that will traverse the
-- stored type of the MonadReader in an attempt to find
-- an @a@ and will then apply the provided function if
-- found.
gAsk :: (Data r, Typeable a, MonadReader r m, MonadPlus n) =>
        (a -> n b) -> m (n b)
gAsk f = do st <- ask
            let y = gFind st 
            return $ maybe mzero f y

-- | The equivalent of 'gAsk' for MonadState
gGet :: (Data s, Typeable a, MonadState s m, MonadPlus n) =>
        (a -> n b) -> c -> m (n b)
gGet f _ = do st <- get
              let y = gFind st 
              return $ maybe mzero f y