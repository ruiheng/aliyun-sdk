module AliYun.Error where

import Import

import AliYun.Types

data ApiError = ApiError
  { errRequestId :: RequestId
  , errHostId    :: Text
  , errCode      :: Text
  , errMessage   :: Text
  }
  deriving (Show, Eq)

$(deriveJSON (defaultOptions { fieldLabelModifier = drop 3 }) ''ApiError)

data ApiError' = ApiError'
  { err'HttpCode :: Int
  , err'Action   :: Text
  , err'Error    :: ApiError
  }
  deriving (Show)


data DatagramError = DatagramError String
  deriving (Show)

instance Exception DatagramError

