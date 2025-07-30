{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
module Streamly.Json
  ( decodeFile
  , decodeFileWithBufferOf
  ) where

import           Control.Monad.Catch
import           Data.Either
import           Data.JsonStream.Parser
import           Data.Word
import           Streamly.Data.Array
import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Data.Unfold as Unfold
import qualified Streamly.External.ByteString as Strict
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.FileSystem.File as File

-- Uses `defaultChunkSize`
decodeFile :: (MonadCatch m, Stream.MonadAsync m) => FilePath -> Parser a -> Stream.Stream m a
decodeFile fp p = flip decodeStream p $ File.readChunks fp

decodeFileWithBufferOf :: (MonadCatch m, Stream.MonadAsync m) => Int -> FilePath -> Parser a -> Stream.Stream m a
decodeFileWithBufferOf sz fp p = flip decodeStream p $ File.readChunksWith sz fp

-- TODO: handle ParseFailed
decodeStream :: (MonadCatch m, Stream.MonadAsync m) => Stream.Stream m (Array Word8) -> Parser a -> Stream.Stream m a
decodeStream w8ArrStrm p = Stream.unfoldMany Unfold.fromList
                           $ Stream.mapMaybe (\case
                                          (_, Right mv) -> mv
                                          _ -> error "Panic: Unreachable pat Left"
                                          )
                           $ Stream.takeWhile (isRight . snd)
                           $ Stream.scanl' (\(po, _) w8Arr -> case po of
                                        ParseNeedData k -> (k $ Strict.fromArray w8Arr, Right Nothing)
                                        ParseFailed err -> (po, Left $ Just err)
                                        ParseDone _rst -> (po, Left Nothing)
                                        po'@ParseYield {} ->
                                          fmap (Right . Just) $ accParsedValues w8Arr po'
                                    ) (runParser p, Right Nothing) w8ArrStrm
  where    
    accParsedValues :: Array Word8 -> ParseOutput a -> (ParseOutput a, [a])
    accParsedValues w8Arr po = go po []
      where
        go (ParseYield v po') acc = go po' (v : acc)
        go (ParseNeedData k) acc = (k $ Strict.fromArray w8Arr, Prelude.reverse acc)
        go po' acc = (po', Prelude.reverse acc)
