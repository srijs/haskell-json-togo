{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Data.JSON.ToGo.Parser where

import Prelude hiding (sequence)

import Data.Aeson (FromJSON, parseJSON)
import Data.Aeson.Parser (jstring, value)
import Data.Aeson.Types (Value, parseEither)
import Data.Attoparsec.ByteString.Char8 (skipSpace, char, string, scientific)
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import Data.ByteString (ByteString)
import Data.Monoid (Monoid, mempty, (<>))
import Data.Scientific (Scientific)
import Data.Text (Text)

import Control.Applicative ((<$), (<*), (<|>))
import Control.Monad (join)
import Control.Monad.Trans.Parser (ParserT, liftP)

type ParserM = ParserT ByteString

rP p = liftP . Atto.parse $ skipSpace >> p

parray :: (Monad m, Monoid r) => (Int -> ParserM m r) -> ParserM m r
parray f = rP (char '[') >> go 0 mempty
  where
    go idx r = do
      r' <- f idx
      join $ rP $
        (go (succ idx) (r <> r') <$ char ',') <|> (return (r <> r') <$ char ']')

pobject :: (Monad m, Monoid r) => (Text -> ParserM m r) -> ParserM m r
pobject f = rP (char '{') >> go mempty
  where
    go r = do
      key <- rP $ jstring <* (skipSpace >> char ':')
      r' <- f key
      join $ rP $
        (go (r <> r') <$ char ',') <|> (return (r <> r') <$ char '}')

parrayL :: Monad m => (Int -> ParserM m r) -> ParserM m [r]
parrayL = parray . fmap (fmap (:[]))

pobjectL :: Monad m => (Text -> ParserM m r) -> ParserM m [r]
pobjectL = pobject . fmap (fmap (:[]))

parray_ :: Monad m => (Int -> ParserM m r) -> ParserM m ()
parray_ = parray . fmap (fmap (const ()))

pobject_ :: Monad m => (Text -> ParserM m r) -> ParserM m ()
pobject_ = pobject . fmap (fmap (const ()))

pnull :: Monad m => ParserM m ()
pnull = rP $ () <$ string "null"

pbool :: Monad m => ParserM m Bool
pbool = rP $ False <$ string "false" <|> True <$ string "true"

pnumber :: Monad m => ParserM m Scientific
pnumber = rP scientific

pstring :: Monad m => ParserM m Text
pstring = rP jstring

pvalue :: Monad m => ParserM m Value
pvalue = rP value

parse :: (Monad m, FromJSON a) => ParserM m a
parse = fmap (parseEither parseJSON) pvalue >>= unwrap
  where unwrap (Left s) = fail s
        unwrap (Right r) = return r
