{-# LANGUAGE BangPatterns                             #-}
{-# LANGUAGE DeriveDataTypeable                       #-}
{-# LANGUAGE DeriveGeneric                            #-}
{-# LANGUAGE ExistentialQuantification                #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving               #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE NoImplicitPrelude                        #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE PackageImports                           #-}
{-# LANGUAGE PatternGuards                            #-}
{-# LANGUAGE QuasiQuotes                              #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE RecordWildCards                          #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE StandaloneDeriving                       #-}
{-# LANGUAGE TupleSections                            #-}
{-# LANGUAGE TypeSynonymInstances                     #-}
{-# LANGUAGE ViewPatterns                             #-}

{-# OPTIONS -fwarn-unused-imports #-}

module Main
       where

import ClassyPrelude

import qualified Data.Serialize as Cereal

data Message = Message Word32 ByteString Word32 Word32 ByteString

parseMessage :: Cereal.Get Message
parseMessage = fmap Message Cereal.getWord32be <*>
  Cereal.getByteString 12 <*>
  Cereal.getWord32be <*>
  Cereal.getWord32be <*>
  (Cereal.remaining >>= Cereal.getByteString)


data MagicValue = MagicValue  -- (extensible)
  deriving (Eq, Show)

instance Cereal.Serialize MagicValue where
  get = Cereal.getBytes 4 >>= f
    where
      f "\xE9\xBE\xB4\xD9" = return MagicValue
      f bad = fail $ "Cereal.Serialize: bad magic value: " <> show bad

  put MagicValue = Cereal.putByteString "\xE9\xBE\xB4\xD9"


data MessageType =
       Version
     | Verack
     | Addr
     | Inv
     | Getdata
     | ObjectType ObjectType
  deriving (Eq, Ord, Show)

data ObjectType =
       Getpubkey
     | Pubkey
     | Msg
     | Broadcast
  deriving (Eq, Ord, Show)
