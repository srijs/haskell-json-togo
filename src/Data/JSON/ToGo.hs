module Data.JSON.ToGo
  ( Value'(..), apply, apply_
  ) where

import Data.Aeson (Value(..))
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H

import Control.Monad (MonadPlus, mzero, msum)

data Value' m a
  = Null'   (m a)
  | Bool'   (Bool -> m a)
  | Number' (Scientific -> m a)
  | String' (Text -> m a)
  | Array'  (Int -> Value' m a)
  | Object' (Text -> Value' m a)

instance Functor m => Functor (Value' m) where
  fmap g (Null' ma)  = Null'   $ fmap g ma
  fmap g (Bool' f)   = Bool'   $ fmap (fmap g) f
  fmap g (Number' f) = Number' $ fmap (fmap g) f
  fmap g (String' f) = String' $ fmap (fmap g) f
  fmap g (Array' f)  = Array'  $ fmap (fmap g) f
  fmap g (Object' f) = Object' $ fmap (fmap g) f

apply :: MonadPlus m => Value' m a -> Value -> m a
apply (Null' ma) Null = ma
apply (Bool' f) (Bool b) = f b
apply (Number' f) (Number n) = f n
apply (String' f) (String s) = f s
apply (Array' f) (Array v) = msum $ map (uncurry (apply . f)) (V.toList $ V.indexed v)
apply (Object' f) (Object h) = msum $ map (uncurry (apply . f)) (H.toList h)
apply _ _ = mzero

apply_ :: Monad m => Value' m a -> Value -> m ()
apply_ (Null' ma) Null = ma >> return ()
apply_ (Bool' f) (Bool b) = f b >> return ()
apply_ (Number' f) (Number n) = f n >> return ()
apply_ (String' f) (String s) = f s >> return ()
apply_ (Array' f) (Array v) = mapM_ (uncurry (apply_ . f)) (V.toList $ V.indexed v)
apply_ (Object' f) (Object ias) = mapM_ (uncurry (apply_ . f)) (H.toList ias)
apply_ _ _ = return ()
