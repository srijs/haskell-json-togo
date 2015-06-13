module Data.JSON.ToGo
  ( ValueM(..)
  , applyV, applyV_
  , applyP, applyP_
  ) where

import Data.JSON.ToGo.Parser

import Data.Aeson (Value(..))
import Data.Monoid (Monoid)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H

import Control.Monad (MonadPlus, mzero, msum)
import Control.Monad.Trans.Class (lift)

data ValueM m a
  = NullM   (m a)
  | BoolM   (Bool -> m a)
  | NumberM (Scientific -> m a)
  | StringM (Text -> m a)
  | ArrayM  (Int -> ValueM m a)
  | ObjectM (Text -> ValueM m a)
  | AnyM    (Value -> m a)

instance Monad m => Functor (ValueM m) where
  fmap g (NullM ma)  = NullM   $ ma >>= return.g
  fmap g (BoolM f)   = BoolM   $ fmap (>>= return.g) f
  fmap g (NumberM f) = NumberM $ fmap (>>= return.g) f
  fmap g (StringM f) = StringM $ fmap (>>= return.g) f
  fmap g (ArrayM f)  = ArrayM  $ fmap (fmap g) f
  fmap g (ObjectM f) = ObjectM $ fmap (fmap g) f
  fmap g (AnyM f)    = AnyM    $ fmap (>>= return.g) f

applyV :: MonadPlus m => ValueM m a -> Value -> m a
applyV (NullM ma)  Null       = ma
applyV (BoolM f)   (Bool b)   = f b
applyV (NumberM f) (Number n) = f n
applyV (StringM f) (String s) = f s
applyV (ArrayM f)  (Array v)  = msum $ map (uncurry (applyV . f)) (V.toList $ V.indexed v)
applyV (ObjectM f) (Object h) = msum $ map (uncurry (applyV . f)) (H.toList h)
applyV (AnyM f)    v          = f v
applyV _           _          = mzero

applyV_ :: Monad m => ValueM m a -> Value -> m ()
applyV_ (NullM ma)  Null         = ma >> return ()
applyV_ (BoolM f)   (Bool b)     = f b >> return ()
applyV_ (NumberM f) (Number n)   = f n >> return ()
applyV_ (StringM f) (String s)   = f s >> return ()
applyV_ (ArrayM f)  (Array v)    = mapM_ (uncurry (applyV_ . f)) (V.toList $ V.indexed v)
applyV_ (ObjectM f) (Object ias) = mapM_ (uncurry (applyV_ . f)) (H.toList ias)
applyV_ (AnyM f)    v            = f v >> return ()
applyV_ _           _            = return ()

applyP :: (Monad m, Monoid r) => ValueM m r -> ParserM m r
applyP (ArrayM f)  = parray  (applyP.f)
applyP (ObjectM f) = pobject (applyP.f)
applyP (NullM m)   = pbool   >>  lift m
applyP (BoolM f)   = pbool   >>= lift.f
applyP (NumberM f) = pnumber >>= lift.f
applyP (StringM f) = pstring >>= lift.f
applyP (AnyM f)    = pvalue  >>= lift.f

applyP_ :: Monad m => ValueM m a -> ParserM m ()
applyP_ = applyP . fmap (const ())
