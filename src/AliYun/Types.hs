{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
module AliYun.Types where

-- {{{1 imports
import           Import
import           Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.Key as AK
import           Database.Persist.Sql  (PersistField (..), PersistFieldSql (..))
import           Text.Blaze.Html       (ToMarkup (..))
import           Text.Shakespeare.I18N (ToMessage (..))
import qualified Network.Wreq.Session as WS


import AliYun.Helpers

#if MIN_VERSION_classy_prelude(1, 5, 0)
import Control.Monad.Trans.Control
#endif
-- }}}1


#define NEWTYPE_TEXT_DERIVING \
  deriving (Show, Eq, Ord, Typeable, ToMessage, ToMarkup \
           , PersistField, PersistFieldSql \
           , ToJSON, FromJSON \
           , ParamValue \
           )

#define NEWTYPE_DEF(t1, un_t1, t2) newtype t1 = t1 { un_t1 :: t2 }

#define NEWTYPE_DEF_TEXT(t1, un_t1) NEWTYPE_DEF(t1, un_t1, Text) NEWTYPE_TEXT_DERIVING

#define INSTANCES_BY_SHOW_READ(t, un_t) \
instance ParamValue t where { toParamValue = tshow . un_t } ; \
instance ToJSON t where { toJSON = toJSON . un t }; \
instance FromJSON t where { \
  parseJSON (A.String s) = maybe mzero (return . t) $ readMay s ;\
  parseJSON v = fmap t $ parseJSON v; \
                          }

NEWTYPE_DEF_TEXT(AccessKeyId, unAccessKeyId)
NEWTYPE_DEF_TEXT(AccessKeySecret, unAccessKeySecret)

NEWTYPE_DEF_TEXT(Nonce, unNonce)
NEWTYPE_DEF_TEXT(ApiVersion, unApiVersion)
NEWTYPE_DEF_TEXT(RequestId, unRequestId)

NEWTYPE_DEF_TEXT(EcsInstanceId, unEcsInstanceId)
NEWTYPE_DEF_TEXT(EcsRegionId, unEcsRegionId)
NEWTYPE_DEF_TEXT(ZoneId, unZoneId)
NEWTYPE_DEF_TEXT(DnsRecordId, unDnsRecordId)


-- | CAUTION: 未 url-encode 时的签名字串，即 base64 得到的字串
NEWTYPE_DEF_TEXT(Signature, unSignature)

data ApiFormat = ApiJson
               | ApiXml
               deriving (Show, Eq, Ord, Enum, Bounded)

instance ParamValue ApiFormat where
  toParamValue ApiJson = "JSON"
  toParamValue ApiXml  = "XML"

instance ToJSON ApiFormat where
  toJSON = toJSON . toParamValue

data SignatureMethod = SignatureHmacSha1
                      deriving (Show, Eq, Ord, Enum, Bounded)

instance ParamValue SignatureMethod where
  toParamValue SignatureHmacSha1 = "HMAC-SHA1"

instance ToJSON SignatureMethod where
  toJSON = toJSON . toParamValue


data SignatureVersion = SignatureVer1
                        deriving (Show, Eq, Ord, Enum, Bounded)

instance ParamValue SignatureVersion where
  toParamValue SignatureVer1 = "1.0"

instance ToJSON SignatureVersion where
  toJSON = toJSON . toParamValue


data HttpMethod = HttpGet
                | HttpPost
                deriving (Show, Eq, Ord, Enum, Bounded)

instance ParamValue HttpMethod where
  toParamValue HttpGet = "GET"
  toParamValue HttpPost = "POST"


class HasAccessKeyId a where
  getAccessKeyId :: a -> AccessKeyId

instance HasAccessKeyId AccessKeyId where
  getAccessKeyId = id


class HasAccessKeySecret a where
  getAccessKeySecret :: a -> AccessKeySecret

instance HasAccessKeySecret AccessKeySecret where
  getAccessKeySecret = id


class HasWreqSession a where
    getWreqSession :: a -> WS.Session

instance HasWreqSession WS.Session where
    getWreqSession = id


type HttpCallBaseMonad m = ( MonadIO m, MonadLogger m, MonadBaseControl IO m )

type HttpCallMonad env m = ( HttpCallBaseMonad m
                           , MonadReader env m
                           , HasAccessKeyId env
                           , HasAccessKeySecret env
                           , HasWreqSession env
                           )

data HttpCallEnv = HttpCallEnv
  { hceAccessKeyId     :: AccessKeyId
  , hceAccessKeySecret :: AccessKeySecret
  , hceWreqSession     :: WS.Session
  }

instance HasAccessKeyId HttpCallEnv where
  getAccessKeyId = hceAccessKeyId

instance HasAccessKeySecret HttpCallEnv where
  getAccessKeySecret = hceAccessKeySecret

instance HasWreqSession HttpCallEnv where
  getWreqSession = hceWreqSession


#if MIN_VERSION_aeson(2, 0, 0)
type AesonKey = Key
#else
type AesonKey = Text
#endif

aesonKeyToText :: AesonKey -> Text
#if MIN_VERSION_aeson(2, 0, 0)
aesonKeyToText = AK.toText
#else
aesonKeyToText = id
#endif

aesonKeyFromText :: Text -> AesonKey
#if MIN_VERSION_aeson(2, 0, 0)
aesonKeyFromText = AK.fromText
#else
aesonKeyFromText = id
#endif


class ExtraNestedJsonKey a where
  -- | JSON 报文中，额外嵌套的一层 Key
  extraNestedJsonKey :: Proxy a -> AesonKey


parseNestedInnerJson :: forall a. (ExtraNestedJsonKey a, FromJSON a) => A.Value -> A.Parser a
parseNestedInnerJson = withObject (unpack $ aesonKeyToText k) (.: k)
  where k = extraNestedJsonKey (Proxy @a)


-- | 通用的带分页的结果返回撰文结构
-- 's' 是真正的数据所在的 json key
data GenericPagedResults (s :: Symbol) a = GenericPagedResults
  { gprPageNumber :: Int
  , gprPageSize   :: Int
  , gprTotalCount :: Int
  , gprRealList   :: a
  }

instance (FromJSON a, ExtraNestedJsonKey a, KnownSymbol s) => FromJSON (GenericPagedResults s a) where
  parseJSON = withObject "GenericPagedResults" $ \ o -> do
                gprPageNumber <- o .: "PageNumber"
                gprPageSize <- o .: "PageSize"
                gprTotalCount <- o .: "TotalCount"
                gprRealList <- o .: fromString (symbolVal (Proxy @s)) >>= parseNestedInnerJson
                pure (GenericPagedResults {..})

instance (ToJSON a, ExtraNestedJsonKey a, KnownSymbol s) => ToJSON (GenericPagedResults s a) where
  toJSON (GenericPagedResults {..}) =
    object [ "PageNumber" .= gprPageNumber
           , "PageSize" .= gprPageSize
           , "TotalCount" .= gprTotalCount
           , fromString (symbolVal (Proxy @s)) .= object [ k .= gprRealList ]
           ]
   where k = extraNestedJsonKey (Proxy @a)


-- | 通用的分批返回的结果返回撰文结构
-- 's' 是真正的数据所在的 json key
data GenericChunkResults (s :: Symbol) a = GenericChunkResults
  { gcrNextToken :: Maybe Text
  , gcrRealList   :: a
  }

instance (FromJSON a, ExtraNestedJsonKey a, KnownSymbol s) => FromJSON (GenericChunkResults s a) where
  parseJSON = withObject "GenericChunkResults" $ \ o -> do
                gcrNextToken <- o .:? "NextToken"
                gcrRealList <- o .: fromString (symbolVal (Proxy @s)) >>= parseNestedInnerJson
                pure (GenericChunkResults {..})

instance (ToJSON a, ExtraNestedJsonKey a, KnownSymbol s) => ToJSON (GenericChunkResults s a) where
  toJSON (GenericChunkResults {..}) =
    object $ catMaybes
            [ ("NextToken" .=) <$> gcrNextToken
            , pure $ fromString (symbolVal (Proxy @s)) .= object [ k .= gcrRealList ]
            ]
   where k = extraNestedJsonKey (Proxy @a)



-- vim: set foldmethod=marker:
