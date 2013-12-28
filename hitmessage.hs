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

import Data.Serialize as Cereal


data Message = Message MagicValue ByteString Word32 Word32 ByteString

parseMessage :: Cereal.Get Message
parseMessage = fmap Message Cereal.get <*>
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


newtype VInt = VInt Word64

instance Cereal.Serialize VInt where
  get = do
    b <- Cereal.getWord8
    case b of
      0xfd -> VInt . fromIntegral <$> Cereal.getWord16be
      0xfe -> VInt . fromIntegral <$> Cereal.getWord32be
      0xff -> fmap VInt Cereal.getWord64be
      a -> return $ VInt $ fromIntegral a

  put (VInt x) | x < 0xfd = putWord8 $ fromIntegral x
  put (VInt x) | x < 0xffff = putWord8 0xfd >> putWord16be (fromIntegral x)
  put (VInt x) | x < 0xffffffff = putWord8 0xfe >> putWord32be (fromIntegral x)
  put (VInt x) = putWord8 0xff >> putWord64be x

newtype VString = VString ByteString

instance Cereal.Serialize VString where
  get = do
    (VInt (fromIntegral -> len)) <- get
    VString <$> getByteString len

  put (VString x) = do
    let len = VInt $ fromIntegral $ length x
    put len
    putByteString x


newtype VIntList = VL [VInt]

instance Cereal.Serialize VIntList where
  get = do
    (VInt (fromIntegral -> len)) <- get
    VL <$> sequence (replicate len get)

  put (VL x) = do
    let len = VInt $ fromIntegral $ length x
    put len
    mapM_ put x



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
