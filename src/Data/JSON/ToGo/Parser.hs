{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Data.JSON.ToGo.Parser (parse) where

import Prelude hiding (sequence)

import Data.JSON.ToGo (Value'(..))

import Data.Aeson.Parser (jstring)
import Data.Attoparsec.ByteString.Char8 (skipSpace, char, string, scientific)
import Data.ByteString (ByteString)
import Data.Conduit (Conduit, yield)
import Data.Conduit.Attoparsec (sinkParser)

import Control.Applicative (pure, (<$), (<$>), (<*), (*>), (<|>))
import Control.Monad (mzero, join)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Class (lift)

parse :: MonadThrow m => Value' m a -> Conduit ByteString m a
parse (Array' f) = sinkParser (skipSpace >> char '[') >> go 0
  where
    go idx = do
      parse (f idx)
      join $ sinkParser $ skipSpace >>
        (go (succ idx) <$ char ',') <|> (return () <$ char ']')
parse (Object' f) = sinkParser (skipSpace >> char '{') >> go
  where
    go = do
      key <- sinkParser $ skipSpace >>
        jstring <* (skipSpace >> char ':')
      parse (f key)
      join $ sinkParser $ skipSpace >>
        (go <$ char ',') <|> (return () <$ char '}')
parse v = sinkParser (skipSpace >> prim v) >>= lift >>= yield
  where
    prim (Null' ma)  = ma <$ string "null"
    prim (Bool' f)   = f <$> (string "false" *> pure False <|>
                              string "true"  *> pure True)
    prim (Number' f) = f <$> scientific
    prim (String' f) = f <$> jstring
    prim _           = mzero
