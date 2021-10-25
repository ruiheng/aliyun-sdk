module AliYun.OpenAPI.DNS where

-- {{{1 imports
import Import
import qualified Data.Text as T
import Data.List.Extra (splitAtEnd)
-- import Control.Lens
-- import Data.Aeson.Lens
-- import qualified Data.Aeson as A

import AliYun.Types
import AliYun.Error
import AliYun.Helpers
import AliYun.OpenAPI.Basic
-- }}}1


dnsDefaultApiUrl :: String
dnsDefaultApiUrl = "https://alidns.aliyuncs.com/"

dnsApiVersion :: ApiVersion
dnsApiVersion = ApiVersion "2015-01-09"


splitFullyQualifiedDomainName :: Text -> (Text, Text)
splitFullyQualifiedDomainName fqds = (intercalate "." rr_parts, intercalate "." domain_parts)
  where
    parts = T.splitOn "." fqds
    (rr_parts, domain_parts) = splitAtEnd 2 parts


-- | https://help.aliyun.com/document_detail/29805.html?spm=a2c4g.11186623.0.0.2594717fC6NDc2
data DnsRecordType = DnsRecord_A
                   | DnsRecord_AAAA
                   | DnsRecord_NS
                   | DnsRecord_MX
                   | DnsRecord_TXT
                   | DnsRecord_CNAME
                   | DnsRecord_SRV
                   | DnsRecord_CAA
                   | DnsRecord_RedirectUrl
                   | DnsRecord_ForwardUrl
                    deriving (Show, Eq, Ord, Enum, Bounded)

instance ParamValue DnsRecordType where
  toParamValue DnsRecord_A           = "A"
  toParamValue DnsRecord_AAAA        = "AAAAA"
  toParamValue DnsRecord_NS          = "NS"
  toParamValue DnsRecord_MX          = "MX"
  toParamValue DnsRecord_TXT         = "TXT"
  toParamValue DnsRecord_CNAME       = "CNAME"
  toParamValue DnsRecord_SRV         = "SRV"
  toParamValue DnsRecord_CAA         = "CAA"
  toParamValue DnsRecord_RedirectUrl = "REDIRECT_URL"
  toParamValue DnsRecord_ForwardUrl  = "FORWARD_URL"

instance ToJSON DnsRecordType where
  toJSON = toJSON . toParamValue

instance FromJSON DnsRecordType where
  parseJSON = parseJsonParamValueEnumBounded "DnsRecordType"


data RsDomainRecord = RsDomainRecord
  { rsDomainName :: Text
  , rsRR         :: Text
  , rsType       :: DnsRecordType
  , rsValue      :: Text
  , rsTTL        :: Int
  , rsPriority   :: Maybe Int
  , rsRecordId   :: DnsRecordId
  }

$(deriveJSON (defaultOptions { fieldLabelModifier = drop 2 }) ''RsDomainRecord)

instance ExtraNestedJsonKey [RsDomainRecord] where
  extraNestedJsonKey _ = "Record"


rsFullyQualifiedDomainName :: RsDomainRecord -> Text
rsFullyQualifiedDomainName (RsDomainRecord {..}) =
  case rsRR of
    ""  -> rsDomainName
    "@" -> rsDomainName
    _   -> rsRR <> "." <> rsDomainName


dnsDescribeDomainRecords :: (HttpCallMonad env m)
                         => Maybe String
                         -> Text
                         -> ParamKvList
                         -> m ( Either ApiError' (RequestId, [RsDomainRecord]) )
dnsDescribeDomainRecords url0 domain_name params0 = do
  openApiCallJsonGetPagedList url api_ver action params1 (Proxy @"DomainRecords")
  where
    url = fromMaybe dnsDefaultApiUrl url0
    api_ver = dnsApiVersion
    action = "DescribeDomainRecords"

    params1 = [ "DomainName" &= domain_name
              ] <> params0


dnsDescribeDomainRecordsExactType :: (HttpCallMonad env m)
                                  => Maybe String
                                  -> Text
                                  -> DnsRecordType
                                  -> ParamKvList
                                  -> m ( Either ApiError' (RequestId, [RsDomainRecord]) )
dnsDescribeDomainRecordsExactType url0 domain_name record_type params0 = do
  dnsDescribeDomainRecords url0 domain_name params1
  where
    params1 = [ "Type" &= record_type
              ] <> params0


dnsUpdateDomainRecord :: (HttpCallMonad env m)
                      => Maybe String
                      -> DnsRecordId
                      -> Text
                      -> DnsRecordType
                      -> Text
                      -> Int  -- ^ TTL
                      -> Maybe Int  -- ^ Priority
                      -> m ( Either ApiError' (RequestId, ()) )
dnsUpdateDomainRecord url0 record_id rr rtype value ttl m_priority = do
  openApiCallJsonGet url api_ver action params (const $ pure ())
  where
    url = fromMaybe dnsDefaultApiUrl url0
    api_ver = dnsApiVersion
    action = "UpdateDomainRecord"
    params = [ "RecordId" &= record_id
             , "RR" &= rr
             , "Type" &= rtype
             , "Value" &= value
             , "TTL" &= ttl
             ]
              <> catMaybes [ "Priority" &?= m_priority ]


-- vim: set foldmethod=marker:
