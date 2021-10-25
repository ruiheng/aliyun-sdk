{-# LANGUAGE ScopedTypeVariables #-}
module AliYun.OpenAPI.Basic where

-- {{{1 import
import           Import
import           Control.Lens         hiding ((.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteArray             as BA
import qualified Data.ByteString.Base64 as B64
import qualified Data.Binary.Builder as BB
import           Network.HTTP.Types.URI (renderQueryText, urlEncode)
import           Network.Wreq hiding (Proxy)
-- import           Network.Wreq.Types   (Postable)
import qualified Network.Wreq.Session as WS
import qualified Text.XML as X
import qualified Text.XML.Cursor as X
import           Network.Wai.Parse (parseContentType)


import qualified Crypto.MAC.HMAC            as CN
import qualified Crypto.Hash                as CN

import AliYun.Types
import AliYun.Error
import AliYun.Helpers
-- }}}1


openApiSignature :: ApiVersion
                 -> ApiFormat
                 -> Nonce
                 -> UTCTime
                 -> (AccessKeyId, AccessKeySecret)
                 -> HttpMethod
                 -> ParamKvList
                 -> ((Signature, ParamKvList), (ByteString, ByteString))
openApiSignature api_ver format nonce t (key_id, key_secret) http_method params0 = ((signature, params_with_signature) , (bs_to_sign, canonical_qs))
  where
    signature = Signature $ B64.encodeBase64 $ BA.convert (CN.hmac (encodeUtf8 hmac_key) bs_to_sign :: CN.HMAC CN.SHA1)
    hmac_key = unAccessKeySecret key_secret <> "&" -- 不明白为什么要加多个 &

    bs_to_sign = encodeUtf8 (toParamValue http_method <> "&%2F&") <> urlEncode True canonical_qs -- 注意这里canonical_qs 被 url encode了两次

    canonical_qs = toStrict $ BB.toLazyByteString $
                      renderQueryText False $ flip map sorted_params $ \ (n, v) -> (n, Just (toParamValue v))

    sorted_params = sortWith fst $
            [ "SignatureMethod" &= SignatureHmacSha1
            , "SignatureVersion" &= SignatureVer1
            , "AccessKeyId" &= key_id
            , "SignatureNonce" &= nonce
            , "Version" &= api_ver
            , "Format" &= format
            , "Timestamp" &= formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%SZ")) t
            ] <> params0

    params_with_signature = ("Signature" &= signature) : sorted_params


-- | 生成随机 Nonce
makeNonce :: MonadIO m
          => Int
          -> m Nonce
makeNonce = fmap (Nonce . fromString) . randomBase64String


applyParamKvListInQs :: ParamKvList -> Options -> Options
applyParamKvListInQs kv_list opts = foldl' f opts kv_list
  where f o (k, SomeParamValue v) = o & param k .~ [ toParamValue v ]


-- | Check response is an error, or a normal one.
openApiResponseDetectError :: (MonadLogger m)
                           => Text
                           -> Response LByteString
                           -> m (Either ApiError' LByteString)
openApiResponseDetectError action r = runExceptT $ do
  withExceptT (ApiError' status_code action) $ do
    ExceptT $ openApiResponseDetectError' action
      (r ^. responseHeader "Content-Type")
      status_code
      response_body

  pure response_body
  where
    response_body = r ^. responseBody
    status_code = r ^. responseStatus . statusCode


openApiResponseDetectError' :: (MonadLogger m)
                            => Text
                            -> ByteString
                            -> Int
                            -> LByteString
                            -> m (Either ApiError ())
-- {{{1
openApiResponseDetectError' action content_type status_code response_body = runExceptT $ do
  case status_code of
    200 -> pure ()
    _ -> do
      case fst $ parseContentType content_type of
        "application/json" -> handle_json
        "application/xml" -> handle_xml
        "text/xml" -> handle_xml
        _ -> do
          $logErrorS logSourceName $ "Unknown HTTP response Content-Type: " <> decodeUtf8 content_type
          impureThrow $ DatagramError "Unknown HTTP Content-Type"

  where
    response_txt = toStrict $ decodeUtf8 response_body

    handle_json = do
      case A.eitherDecode response_body of
        Left err -> do
          $logErrorS logSourceName $
              "Failed to decode response as JSON: " <> fromString err
              <> ", action was '" <> action <> "'"
              <> ", response was:\n" <> response_txt

          impureThrow $ JSONError err

        Right jv -> do
          case fromJSON'Message jv of
            Left err -> do
              $logErrorS logSourceName $ "Could not parse response body to payload: " <> err
              impureThrow $ DatagramError (unpack err)

            Right api_error -> throwError api_error

    handle_xml = do
      case X.parseLBS X.def response_body of
        Left ex -> do
          $logErrorS logSourceName $
              "Failed to decode response as XML: " <> tshow ex
              <> ", action was '" <> action <> "'"
              <> ", response was:\n" <> response_txt
          impureThrow $ DatagramError $ "Invalid XML: " <> show ex

        Right doc -> do
          let cursor = X.fromDocument doc
          let get_ele_s = either (\ e -> impureThrow $ DatagramError (unpack e)) pure . getElementContent cursor
          api_error <- ApiError <$> fmap RequestId (get_ele_s "RequestId")
                                <*> get_ele_s "HostId"
                                <*> get_ele_s "Code"
                                <*> get_ele_s "Message"
          throwError api_error
-- }}}1


-- | Call OpenAPI by JSON format.
openApiCallJsonGet :: (HttpCallMonad env m)
                   => String
                   -> ApiVersion
                   -> Text
                   -> ParamKvList
                   -> (A.Object -> A.Parser a)
                   -> m (Either ApiError' (RequestId, a))
openApiCallJsonGet url api_ver action params0 parse_json_obj = do
  key_id <- asks getAccessKeyId
  key_secret <- asks getAccessKeySecret
  sess <- asks getWreqSession
  nonce <- makeNonce 16
  t <- liftIO getCurrentTime
  let ((_signature, params_with_signature) , (_bs_to_sign, _canonical_qs)) = openApiSignature api_ver format nonce t (key_id, key_secret) http_method call_params
  let opts = defaults & applyParamKvListInQs params_with_signature
                      & checkResponse .~ Just check_response_noop

  -- $logDebugS logSourceName $ "Params to send: " <> tshow opts

  runExceptT $ do
    lbs <- ExceptT $ liftIO ( WS.getWith opts sess url ) >>= openApiResponseDetectError action
    -- $logDebugS logSourceName $ "Response: " <> toStrict (decodeUtf8 lbs)

    case A.eitherDecode lbs of
      Left err -> do
          $logErrorS logSourceName $
              "Failed to decode JSON: " <> fromString err
              <> ", action was '" <> action <> "'"
              <> ", error was: " <> fromString err
              <> ", response was:\n" <> toStrict (decodeUtf8 lbs)
          liftIO $ throwIO $ DatagramError (unpack err)

      Right j_obj -> do
        case A.parse parse_json_obj j_obj of
          A.Error err -> do
            $logErrorS logSourceName $
                "Failed to parse JSON: " <> fromString err
                <> ", action was '" <> action <> "'"
                <> ", error was: " <> fromString err
                <> ", response was:\n" <> toStrict (decodeUtf8 lbs)

            liftIO $ throwIO $ DatagramError err

          A.Success real_result -> do
            case A.parseEither ( A..: "RequestId" ) j_obj  of
              Left err -> do
                $logErrorS logSourceName $
                    "Failed to parse JSON: no RequestId: " <> fromString err
                    <> ", action was '" <> action <> "'"
                    <> ", error was: " <> fromString err
                    <> ", response was:\n" <> toStrict (decodeUtf8 lbs)
                liftIO $ throwIO $ DatagramError err

              Right req_id -> pure (req_id, real_result)
  where
    format = ApiJson
    http_method = HttpGet
    call_params = ("Action" &= action) : params0
    check_response_noop _ _ = pure ()


-- | Call OpenAPI by JSON format, which API will response with a paged list (using PageNumber & PageSize).
openApiCallJsonGetPagedList :: forall env m a s. (HttpCallMonad env m, FromJSON a, ExtraNestedJsonKey a, Semigroup a, KnownSymbol s)
                            => String
                            -> ApiVersion
                            -> Text
                            -> ParamKvList
                            -> Proxy s
                            -> m (Either ApiError' (RequestId, a))
openApiCallJsonGetPagedList url api_ver action params0 _ = runExceptT $ go Nothing 1
  where
    -- 测试看来 PageSize 作为输入参数并不一定确定返回时的 PageSize
    -- 但有些接口如 DescribeInstances ，使用这个参数来确定分页方法
    page_size = 10 :: Int
    params1 = params0 <> [ "PageSize" &= page_size ]

    go prev_res pn = do
      let params2 = params1 <> [ "PageNumber" &= pn ]

      (req_id, (GenericPagedResults {..} :: GenericPagedResults s a)) <- ExceptT $ openApiCallJsonGet url api_ver action params2 (A.parseJSON . A.Object)
      -- $logDebug $ "gprPageSize=" <> tshow gprPageSize

      let expect_last_page = (gprTotalCount + gprPageSize - 1) `div` gprPageSize
      let new_res = fromMaybe gprRealList $ (<> gprRealList) <$> prev_res
      if expect_last_page <= gprPageNumber
         then pure (req_id, new_res)
         else go (Just new_res) (pn + 1 :: Int)


-- | Call OpenAPI by JSON format, which API will response with a paged list (using NextToken).
openApiCallJsonGetChunkList :: forall env m a s . (HttpCallMonad env m, FromJSON a, ExtraNestedJsonKey a, Semigroup a, KnownSymbol s)
                            => String
                            -> ApiVersion
                            -> Text
                            -> ParamKvList
                            -> Proxy s
                            -> m (Either ApiError' (RequestId, a))
openApiCallJsonGetChunkList url api_ver action params0 _ = runExceptT $ go Nothing Nothing
  where
    batch_size = 10 :: Int
    params1 = params0 <> [ "MaxResults" &= batch_size ]

    go prev_res m_next_token = do
      let params2 = params1 <> catMaybes [ "NextToken" &?= m_next_token ]

      (req_id, (GenericChunkResults {..} :: GenericChunkResults s a)) <- ExceptT $ openApiCallJsonGet url api_ver action params2 (A.parseJSON . A.Object)
      let new_res = fromMaybe gcrRealList $ (<> gcrRealList) <$> prev_res
      case join $ nullTextAsNothing gcrNextToken of
        Nothing -> pure (req_id, new_res)
        Just token2 -> go (Just new_res) (Just token2)


-- vim: set foldmethod=marker:
