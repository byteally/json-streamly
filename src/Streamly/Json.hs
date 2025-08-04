{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
module Streamly.Json
  ( decodeFile
  , decodeFileWithBufferOf
  , decodeStream
  ) where

import           Control.Monad.Catch
import           Data.JsonStream.Parser
import           Data.Text ( Text )
import qualified Data.Text as T
import           Data.Word
import           Streamly.Data.Array
import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Data.Unfold as Unfold
import qualified Streamly.External.ByteString as Strict
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.FileSystem.File as File

-- Uses `defaultChunkSize`
decodeFile :: (MonadCatch m, Stream.MonadAsync m, Show a) => FilePath -> Parser a -> Stream.Stream m (Either Text a)
decodeFile fp p = flip decodeStream p $ File.readChunks fp

decodeFileWithBufferOf :: (MonadCatch m, Stream.MonadAsync m, Show a) => Int -> FilePath -> Parser a -> Stream.Stream m (Either Text a)
decodeFileWithBufferOf sz fp p = flip decodeStream p $ File.readChunksWith sz fp

decodeStream :: (MonadCatch m, Stream.MonadAsync m, Show a) => Stream.Stream m (Array Word8) -> Parser a -> Stream.Stream m (Either Text a)
decodeStream w8ArrStrm p = Stream.unfoldMany Unfold.fromList
                           $ Stream.map snd
                           $ Stream.postscanl' (\(po, _) w8Arr -> case po of
                                        ParseNeedData k ->
                                          case k $ Strict.fromArray w8Arr of
                                            po'@ParseYield {} -> consume po' []
                                            _ -> (k $ Strict.fromArray w8Arr, [])
                                        ParseFailed err -> (po, [Left $ T.pack err])
                                        po'@ParseDone {} -> error $ "Panic: Unexpected ParseDone @decodeStream: " <> show po'
                                        po'@ParseYield {} -> error $ "Panic: Unexpected ParseYield @decodeStream: " <> show po'
                                    ) (runParser p, []) w8ArrStrm
  where
    consume (ParseYield v po') acc = consume po' (Right v : acc)
    consume k acc = (k, reverse acc)
