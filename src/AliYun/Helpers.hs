module AliYun.Helpers where

-- {{{1 imports
import           ClassyPrelude

#if MIN_VERSION_base(4, 13, 0)
import           Control.Monad (MonadFail(..))
#else
#endif

-- import           Control.Monad.Logger
-- import qualified Control.Monad.Logger.CallStack as LCS
import qualified Data.Aeson           as A
import qualified Data.Aeson.Types     as A
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString.Base64.URL as B64L
import qualified Data.ByteString.Char8      as C8
import           Data.List            ((!!))
-- import           Network.Wreq hiding (Proxy)
import           System.Random              (randomIO, randomRIO)
import           Text.XML
import           Text.XML.Cursor hiding (bool)

-- import           GHC.Stack (HasCallStack)
-- }}}1


class ParamValue a where
  toParamValue :: a -> Text

instance ParamValue String where
  toParamValue = fromString

instance ParamValue Text where
  toParamValue = id

instance ParamValue LText where
  toParamValue = toStrict

instance ParamValue Bool where
  toParamValue = bool "false" "true"

instance ParamValue Int where toParamValue = tshow


data SomeParamValue = forall a. ParamValue a => SomeParamValue a

instance ParamValue SomeParamValue where
  toParamValue (SomeParamValue v) = toParamValue v


(&=) :: ParamValue a => Text -> a -> (Text, SomeParamValue)
infix 3 &=
(&=) k v = (k, SomeParamValue v)


(&?=) :: ParamValue a => Text -> Maybe a -> Maybe (Text, SomeParamValue)
infix 3 &?=
(&?=) k v = (k &=) <$> v


parseEnumParamValueText :: (ParamValue a, Enum a, Bounded a) => Text -> Maybe a
parseEnumParamValueText = flip lookup table
  where table = map (toParamValue &&& id) [minBound .. maxBound]


parseJsonParamValueEnumBounded :: (ParamValue a, Enum a, Bounded a) => String -> A.Value -> A.Parser a
parseJsonParamValueEnumBounded name = A.withText name $ \ t -> do
  maybe (fail $ "unknown value string: " <> unpack t) return $ parseEnumParamValueText t


type ParamKvList = [(Text, SomeParamValue)]


nullTextAsNothing :: Applicative m => Maybe Text -> m (Maybe Text)
nullTextAsNothing mt =
  case mt of
    Just t | null t -> pure Nothing
           | otherwise -> pure $ Just t

    _ -> pure Nothing
-- | 生成随机字串: 字串使用base64相同的字符集
randomBase64String :: MonadIO m
                   => Int
                   -> m String
randomBase64String salt_len = liftIO $ do
  liftM (C8.unpack . take salt_len . B64L.encodeBase64' . pack) $
      replicateM gen_len randomIO
  where gen_len = salt_len  -- long enough


randomAlphaNumString :: MonadIO m
                     => Int
                     -> m String
randomAlphaNumString len = do
  liftIO $ replicateM len $ fmap (chars !!) $ randomRIO (0, chars_len - 1)
  where chars = ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9']
        chars_len = length chars


logSourceName :: Text
logSourceName = "aliyun-sdk"


fromJSON'Message :: (A.FromJSON a)
                 => A.Value
                 -> Either Text a
fromJSON'Message jv =
  case A.fromJSON jv of
    A.Success x -> return x
    A.Error err -> Left $ "Failed to parse JSON structure: " <> fromString err
                          <> "\ndata was:\n"
                          <> toStrict (decodeUtf8 $ AP.encodePretty jv)


getElementContent :: Cursor -> Name -> Either String Text
getElementContent cursor t =
    maybe (Left $ unpack $ "no such element: " <> nameLocalName t) Right $
        getElementContentMaybe cursor t

getElementContentMaybe :: Cursor -> Name -> Maybe Text
getElementContentMaybe cursor t =
    listToMaybe $ cursor $/ element t &// content


-- vim: set foldmethod=marker:
