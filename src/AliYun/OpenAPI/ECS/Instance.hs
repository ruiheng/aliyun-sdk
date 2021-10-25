{-# LANGUAGE ScopedTypeVariables #-}
module AliYun.OpenAPI.ECS.Instance where

-- {{{1 imports
import Import
-- import Control.Lens
-- import Data.Aeson.Lens
import qualified Data.Aeson.Text as A
import           Data.Aeson

import AliYun.Types
import AliYun.Error
import AliYun.Helpers
import AliYun.OpenAPI.Basic
import AliYun.OpenAPI.ECS
-- }}}1


-- | https://help.aliyun.com/document_detail/25687.html
data EcsStatus = EcsPending
               | EcsRunning
               | EcsStarting
               | EcsStopping
               | EcsStopped
               deriving (Show, Eq, Ord, Enum, Bounded)

instance ParamValue EcsStatus where
  toParamValue EcsPending  = "Pending"
  toParamValue EcsRunning  = "Running"
  toParamValue EcsStarting = "Starting"
  toParamValue EcsStopping = "Stopping"
  toParamValue EcsStopped  = "Stopped"

instance ToJSON EcsStatus where
  toJSON = toJSON . toParamValue

instance FromJSON EcsStatus where
  parseJSON = parseJsonParamValueEnumBounded "EcsStatus"

data RsDescribeInstanceStatus = RsDescribeInstanceStatus
  { rsdisStatus     :: EcsStatus
  , rsdisInstanceId :: EcsInstanceId
  }

$(deriveJSON (defaultOptions { fieldLabelModifier = drop 5 }) ''RsDescribeInstanceStatus)

instance ExtraNestedJsonKey [RsDescribeInstanceStatus] where
  extraNestedJsonKey _ = "InstanceStatus"


ecsDescribeInstanceStatus :: (HttpCallMonad env m)
                          => Maybe String
                          -> EcsRegionId
                          -> NonEmpty EcsInstanceId
                          -> m ( Either ApiError' (RequestId, [RsDescribeInstanceStatus]) )
ecsDescribeInstanceStatus url0 region_id inst_ids = do
  openApiCallJsonGetPagedList url api_ver action params0 (Proxy @ "InstanceStatuses")
  where
    url = fromMaybe ecsDefaultApiUrl url0
    api_ver = ecsApiVersion
    action = "DescribeInstanceStatus"
    instances_params = flip map (zip [1..] (toList inst_ids)) $
                              \ ( (idx :: Int), inst_id) -> ("InstanceId." <> tshow idx) &= inst_id

    params0 = [ "RegionId" &= region_id
              ] <> instances_params


ecsDescribeInstanceStatus1 :: (HttpCallMonad env m)
                           => Maybe String
                           -> EcsRegionId
                           -> EcsInstanceId
                           -> m ( Either ApiError' (RequestId, Maybe RsDescribeInstanceStatus) )
ecsDescribeInstanceStatus1 url0 region_id inst_id = runExceptT $ do
  (req_id, lst) <- ExceptT $ ecsDescribeInstanceStatus url0 region_id (pure inst_id)
  let m_st = find ((== inst_id) . rsdisInstanceId) lst
  pure (req_id, m_st)


newtype InnerIpAddressList = InnerIpAddressList { unInnerIpAddressList :: [Text] }

instance ToJSON InnerIpAddressList where
  toJSON (InnerIpAddressList ips) =
    object [ k .= ips ]
   where k = extraNestedJsonKey (Proxy @InnerIpAddressList)

instance FromJSON InnerIpAddressList where
  parseJSON = withObject "InnerIpAddressList" $ \ o ->
                fmap InnerIpAddressList $ o .: k
   where k = extraNestedJsonKey (Proxy @InnerIpAddressList)

instance ExtraNestedJsonKey InnerIpAddressList where
  extraNestedJsonKey _ = "IpAddress"


newtype PublicIpAddressList = PublicIpAddressList { unPublicIpAddressList :: [Text] }

instance ToJSON PublicIpAddressList where
  toJSON (PublicIpAddressList ips) =
    object [ k .= ips ]
   where k = extraNestedJsonKey (Proxy @PublicIpAddressList)

instance FromJSON PublicIpAddressList where
  parseJSON = withObject "PublicIpAddressList" $ \ o ->
                fmap PublicIpAddressList $ o .: k
   where k = extraNestedJsonKey (Proxy @PublicIpAddressList)

instance ExtraNestedJsonKey PublicIpAddressList where
  extraNestedJsonKey _ = "IpAddress"


newtype PrivateIpAddressList = PrivateIpAddressList { unPrivateIpAddressList :: [Text] }

instance ToJSON PrivateIpAddressList where
  toJSON (PrivateIpAddressList ips) =
    object [ k .= ips ]
   where k = extraNestedJsonKey (Proxy @PrivateIpAddressList)

instance FromJSON PrivateIpAddressList where
  parseJSON = withObject "PrivateIpAddressList" $ \ o ->
                fmap PrivateIpAddressList $ o .: k
   where k = extraNestedJsonKey (Proxy @PrivateIpAddressList)

instance ExtraNestedJsonKey PrivateIpAddressList where
  extraNestedJsonKey _ = "IpAddress"


-- | TODO: 不完整
data VpcAttributes = VpcAttributes
  { vpcPrivateIpAddress :: PrivateIpAddressList
  }

$(deriveJSON (defaultOptions { fieldLabelModifier = drop 3 }) ''VpcAttributes)


-- | TODO: 不完整
data RsDescribeInstance = RsDescribeInstance
  { rsdiInstanceId      :: EcsInstanceId
  , rsdiInstanceName    :: Text
  , rsdiInnerIpAddress  :: InnerIpAddressList
  , rsdiVpcAttributes   :: VpcAttributes
  , rsdiPublicIpAddress :: PublicIpAddressList
  }

$(deriveJSON (defaultOptions { fieldLabelModifier = drop 4 }) ''RsDescribeInstance)

instance ExtraNestedJsonKey [RsDescribeInstance] where
  extraNestedJsonKey _ = "Instance"


rsdiPublicIpAddress' :: RsDescribeInstance -> [Text]
rsdiPublicIpAddress' = unPublicIpAddressList . rsdiPublicIpAddress


ecsDescribeInstances :: (HttpCallMonad env m)
                     => Maybe String
                     -> EcsRegionId
                     -> NonEmpty EcsInstanceId
                     -> m ( Either ApiError' (RequestId, [RsDescribeInstance]) )
ecsDescribeInstances url0 region_id inst_ids = do
  openApiCallJsonGetChunkList url api_ver action params0 (Proxy @"Instances")
  where
    url = fromMaybe ecsDefaultApiUrl url0
    api_ver = ecsApiVersion
    action = "DescribeInstances"

    params0 = [ "RegionId" &= region_id
              , "InstanceIds" &= A.encodeToLazyText inst_ids
              ]


ecsDescribeInstance1 :: (HttpCallMonad env m)
                     => Maybe String
                     -> EcsRegionId
                     -> EcsInstanceId
                     -> m ( Either ApiError' (RequestId, Maybe RsDescribeInstance) )
ecsDescribeInstance1 url0 region_id inst_id = runExceptT $ do
  (req_id, lst) <- ExceptT $ ecsDescribeInstances url0 region_id (pure inst_id)
  let m_st = find ((== inst_id) . rsdiInstanceId) lst
  pure (req_id, m_st)


data RsStartInstance = RsStartInstance
  { rssiInstanceId     :: EcsInstanceId
  , rssiCurrentStatus  :: EcsStatus
  , rssiPreviousStatus :: EcsStatus
  , rssiCode           :: Text
  , rssiMessage        :: Text
  }

$(deriveJSON (defaultOptions { fieldLabelModifier = drop 4 }) ''RsStartInstance)

instance ExtraNestedJsonKey [RsStartInstance] where
  extraNestedJsonKey _ = "InstanceResponse"


ecsStartInstances :: (HttpCallMonad env m)
                  => Maybe String
                  -> EcsRegionId
                  -> NonEmpty EcsInstanceId
                  -> m ( Either ApiError' (RequestId, [RsStartInstance]) )
ecsStartInstances url0 region_id inst_ids = do
  openApiCallJsonGet url api_ver action params0 ((.: "InstanceResponses") >=> parseNestedInnerJson)
  where
    url = fromMaybe ecsDefaultApiUrl url0
    api_ver = ecsApiVersion
    action = "StartInstances"
    instances_params = flip map (zip [1..] (toList inst_ids)) $
                              \ ( (idx :: Int), inst_id) -> ("InstanceId." <> tshow idx) &= inst_id

    params0 = [ "RegionId" &= region_id
              ] <> instances_params



ecsStartInstance1 :: (HttpCallMonad env m)
                  => Maybe String
                  -> EcsRegionId
                  -> EcsInstanceId
                  -> m ( Either ApiError' (RequestId, Maybe RsStartInstance) )
ecsStartInstance1 url0 region_id inst_id = runExceptT $ do
  (req_id, lst) <- ExceptT $ ecsStartInstances url0 region_id (pure inst_id)
  let m_st = find ((== inst_id) . rssiInstanceId) lst
  pure (req_id, m_st)


ecsStopInstance :: (HttpCallMonad env m)
                => Maybe String
                -> EcsInstanceId
                -> m ( Either ApiError' (RequestId, ()) )
ecsStopInstance url0 inst_id = do
  openApiCallJsonGet url api_ver action params0 (const $ pure ())
  where
    url = fromMaybe ecsDefaultApiUrl url0
    api_ver = ecsApiVersion
    action = "StopInstance"

    params0 = [ "InstanceId" &= inst_id
              ]


-- vim: set foldmethod=marker:
