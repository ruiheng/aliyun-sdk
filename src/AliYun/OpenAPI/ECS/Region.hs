module AliYun.OpenAPI.ECS.Region where

-- {{{1 imports
import Import
-- import Control.Lens
-- import Data.Aeson.Lens
-- import qualified Data.Aeson as A

import AliYun.Types
import AliYun.Error
import AliYun.Helpers
import AliYun.OpenAPI.Basic
import AliYun.OpenAPI.ECS
-- }}}1


data Availability = Available
                  | SoldOut
               deriving (Show, Eq, Ord, Enum, Bounded)

instance ParamValue Availability where
  toParamValue Available = "available"
  toParamValue SoldOut   = "soldOut"

instance ToJSON Availability where
  toJSON = toJSON . toParamValue

instance FromJSON Availability where
  parseJSON = parseJsonParamValueEnumBounded "Availability"


data RsDescribeRegion = RsDescribeRegion
  { rsdrLocalName      :: Text
  , rsdrRegionEndpoint :: Text
  , rsdrRegionId       :: EcsRegionId
  }

$(deriveJSON (defaultOptions { fieldLabelModifier = drop 4 }) ''RsDescribeRegion)

instance ExtraNestedJsonKey [RsDescribeRegion] where
  extraNestedJsonKey _ = "Region"


ecsDescribeRegions :: (HttpCallMonad env m)
                   => Maybe String
                   -> m (Either ApiError' (RequestId, [RsDescribeRegion]))
ecsDescribeRegions url0 = do
  openApiCallJsonGet url api_ver action params0 ((.: "Regions") >=> parseNestedInnerJson)
  where
    url = fromMaybe ecsDefaultApiUrl url0
    api_ver = ecsApiVersion
    action = "DescribeRegions"
    params0 = []




-- vim: set foldmethod=marker:
