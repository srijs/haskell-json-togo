{-# LANGUAGE DeriveFunctor #-}

module Data.JSON.ToGo
  ( Expr
  , evalToParser, evalToParser_
  , evalWithValue
  , matchNull, matchAny
  , matchBool, matchNumber, matchString
  , matchArray, matchObject
  ) where

import Data.JSON.ToGo.Parser

import Data.Aeson (Value(..))
import Data.Monoid (Monoid)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H

import Control.Monad (void, MonadPlus, mzero, msum)
import Control.Monad.Trans.Class (lift)

data ExprF m a r
  = NullM   (m a)
  | BoolM   (Bool -> m a)
  | NumberM (Scientific -> m a)
  | StringM (Text -> m a)
  | ArrayM  (Int -> r)
  | ObjectM (Text -> r)
  | AnyM    (Value -> m a)
  deriving Functor

newtype Fix f = Fix { unFix :: f (Fix f) }

type Algebra f a = f a -> a

cata :: Functor f => Algebra f a -> Fix f -> a
cata f = f . fmap (cata f) . unFix

newtype Expr m a = Expr { getFix :: Fix (ExprF m a) }

alg :: (Monad m, Monoid a) => Algebra (ExprF m a) (ParserM m a)
alg (NullM m) = lift m
alg (BoolM f) = pbool >>= lift . f
alg (NumberM f) = pnumber >>= lift . f
alg (StringM f) = pstring >>= lift . f
alg (AnyM f) = pvalue >>= lift . f
alg (ArrayM f) = parray f
alg (ObjectM f) = pobject f

alg_ :: (Monad m) => Algebra (ExprF m a) (ParserM m ())
alg_ (NullM m) = void $ lift m
alg_ (BoolM f) = void $ pbool >>= lift . f
alg_ (NumberM f) = void $ pnumber >>= lift . f
alg_ (StringM f) = void $ pstring >>= lift . f
alg_ (AnyM f) = void $ pvalue >>= lift . f
alg_ (ArrayM f) = parray_ f
alg_ (ObjectM f) = pobject_ f

valueAlg :: (MonadPlus m) => Algebra (ExprF m a) (Value -> m a)
valueAlg (NullM m) Null = m
valueAlg (BoolM f) (Bool b) = f b
valueAlg (NumberM f) (Number n) = f n
valueAlg (StringM f) (String s) = f s
valueAlg (AnyM f) v = f v
valueAlg (ArrayM f) (Array v) = msum . map (uncurry f) . V.toList $ V.indexed v
valueAlg (ObjectM f) (Object h) = msum . map (uncurry f) $ H.toList h
valueAlg _ _ = mzero

evalToParser :: (Monad m, Monoid a) => Expr m a -> ParserM m a
evalToParser = cata alg . getFix

evalToParser_ :: (Monad m) => Expr m a -> ParserM m ()
evalToParser_ = cata alg_ . getFix

evalWithValue :: (MonadPlus m) => Expr m a -> Value -> m a
evalWithValue = cata valueAlg . getFix

matchNull :: m a -> Expr m a
matchNull = Expr . Fix . NullM

matchBool :: (Bool -> m a) -> Expr m a
matchBool = Expr . Fix . BoolM

matchNumber :: (Scientific -> m a) -> Expr m a
matchNumber = Expr . Fix . NumberM

matchString :: (Text -> m a) -> Expr m a
matchString = Expr . Fix . StringM

matchAny :: (Value -> m a) -> Expr m a
matchAny = Expr . Fix . AnyM

matchArray :: (Int -> Expr m a) -> Expr m a
matchArray = Expr . Fix . ArrayM . fmap getFix

matchObject :: (Text -> Expr m a) -> Expr m a
matchObject = Expr . Fix . ObjectM . fmap getFix
