{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import ClassyPrelude
import Data.Binary.Get

import qualified Data.Serialize.Get as Cereal

data Message = Msg Word32 ByteString Word32 Word32 ByteString

parseMessage :: Cereal.Get Message
parseMessage = fmap Msg Cereal.getWord32be <*>
  Cereal.getByteString 12 <*>
  Cereal.getWord32be <*>
  Cereal.getWord32be <*>
  (Cereal.remaining >>= Cereal.getByteString)
