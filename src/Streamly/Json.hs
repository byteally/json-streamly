{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
module Streamly.Json
  ( decodeFile
  , decodeFileWithBufferOf
  ) where

import Streamly.Prelude as S
import qualified Streamly.Data.Unfold as Unfold
import Data.JsonStream.Parser
import qualified Streamly.Internal.FileSystem.File as File
import Data.Word
import qualified Streamly.External.ByteString as Strict
import Streamly.Data.Array.Foreign
import Control.Monad.Catch
import Data.Either



-- Uses `defaultChunkSize`
decodeFile :: (IsStream t, MonadCatch m, MonadAsync m) => FilePath -> Parser a -> t m a
decodeFile fp p = flip decodeStream p $ File.toChunks fp

decodeFileWithBufferOf :: (IsStream t, MonadCatch m, MonadAsync m) => Int -> FilePath -> Parser a -> t m a
decodeFileWithBufferOf sz fp p = flip decodeStream p $ File.toChunksWithBufferOf sz fp

-- TODO: handle ParseFailed
decodeStream :: (IsStream t, MonadCatch m, MonadAsync m) => t m (Array Word8) -> Parser a -> t m a
decodeStream w8ArrStrm p = S.unfoldMany Unfold.fromList
                           $ S.mapMaybe (\case
                                          (_, Right mv) -> mv
                                          _ -> error "Panic: Unreachable pat Left"
                                          )
                           $ S.takeWhile (isRight . snd)
                           $ S.scanl' (\(po, _) w8Arr -> case po of
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
