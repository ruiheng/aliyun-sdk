module Main where

-- {{{1 imports
import           ClassyPrelude
#if MIN_VERSION_base(4, 13, 0)
import           Control.Monad (MonadFail(..))
#else
#endif
import           Control.Lens         hiding ((.=), argument)
import           Control.Monad.Trans.Except
import           Control.Monad.Logger
import           Data.Aeson (ToJSON(..))
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString.Lazy as LB
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.List.NonEmpty (some1, NonEmpty)
import qualified Data.Text as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Tree
import           Network.HTTP.Client   (streamFile)
import           Network.Wreq         (responseBody, responseHeader)
import qualified Network.Wreq.Session  as WS
import           Options.Applicative
import           System.IO             (hPutStrLn)
import           System.Log.FastLogger (LoggerSet, newStderrLoggerSet,
                                        pushLogStr)
import           System.Exit
import           Text.Show.Unicode (ushow)

import AliYun

#if MIN_VERSION_classy_prelude(1, 5, 0)
import Control.Monad.Trans.Control
#endif
-- }}}1


data ManageCmd = Regions
               | EcsInstanceStatus EcsRegionId (NonEmpty EcsInstanceId)
               | EcsStartInstance EcsRegionId (NonEmpty EcsInstanceId)
               | EcsStopInstance EcsInstanceId
               | EcsDescribeInstance EcsRegionId (NonEmpty EcsInstanceId)
               | DnsDescribeDomainRecords Text [(Text, Text)]
               | DnsUpdateDomainRecord DnsRecordId Text DnsRecordType Text Int (Maybe Int)
               deriving (Show)

data Options = Options
  { optVerbose         :: Int
  , optAccessKeyId     :: AccessKeyId
  , optAccessKeySecret :: AccessKeySecret
  , optCommand         :: ManageCmd
  }

optionsParse :: Parser Options
-- {{{1
optionsParse = Options
                <$> (option auto
                        $ long "verbose" <> short 'v' <> value 1
                        <> metavar "LEVEL"
                        <> help "Verbose Level (0 - 3)")
                <*> fmap (AccessKeyId . fromString) (strOption (long "key-id" <> short 'k' <> metavar "KEY_ID"))
                <*> fmap (AccessKeySecret . fromString) (strOption (long "key-secret" <> short 's' <> metavar "KEY_SECRET"))
                <*> manageCmdParser
-- }}}1


manageCmdParser :: Parser ManageCmd
manageCmdParser = subparser $
  command "regions"
    (info (helper <*> pure Regions)
          (progDesc "DescribeRegions")
    )
  <> command "ecs-instance-status"
      (info (helper <*> pure EcsInstanceStatus
                <*> fmap EcsRegionId (argument str (metavar "REGION_ID"))
                <*> some1 (fmap EcsInstanceId (argument str (metavar "ECS_INSTANCE_ID")))
            )
        (progDesc "DescribeInstanceStatus")
      )
  <> command "ecs-start-instance"
      (info (helper <*> pure EcsStartInstance
                <*> fmap EcsRegionId (argument str (metavar "REGION_ID"))
                <*> some1 (fmap EcsInstanceId (argument str (metavar "ECS_INSTANCE_ID")))
            )
        (progDesc "StartInstances")
      )
  <> command "ecs-stop-instance"
      (info (helper <*> pure EcsStopInstance
                <*> fmap EcsInstanceId (argument str (metavar "ECS_INSTANCE_ID"))
            )
        (progDesc "StartInstances")
      )
  <> command "ecs-describe-instance"
      (info (helper <*> pure EcsDescribeInstance
                <*> fmap EcsRegionId (argument str (metavar "REGION_ID"))
                <*> some1 (fmap EcsInstanceId (argument str (metavar "ECS_INSTANCE_ID")))
            )
        (progDesc "DescribeInstances")
      )
  <> command "dns-describe-records"
      (info (helper <*> pure DnsDescribeDomainRecords
                    <*> argument str (metavar "DOMAIN_NAME")
                    <*> some (argument nameValueParser (metavar "NAME=VALUE"))
            )
            (progDesc "DescribeDomainRecords")
      )
  <> command "dns-update-record"
      (info (helper <*> pure DnsUpdateDomainRecord
                    <*> fmap DnsRecordId (argument str (metavar "RECORD_ID"))
                    <*> argument str (metavar "RR")
                    <*> argument (enumStringReader "Record Type") (metavar "RECORD_TYPE")
                    <*> argument str (metavar "VALUE")
                    <*> argument auto (metavar "TTL")
                    <*> optional (argument auto (metavar "PRIORITY"))
            )
            (progDesc "DescribeDomainRecords")
      )


-- | parse 'name=value'
nameValueParser :: ReadM (Text, Text)
nameValueParser = do
  t <- str
  let (name, value1) = T.breakOn "=" t
  value <- maybe (readerError $ "no '=' found in " <> unpack t) return $ T.stripPrefix "=" value1
  when (null name) $ readerError $ "name part is empty in " <> unpack t
  return (name, value)


enumStringReader :: (Enum a, Bounded a, ParamValue a)
                 => String
                 -> ReadM a
enumStringReader type_prompt = do
  s <- str
  maybe (fail $ "cannot parse as " <> type_prompt <> ": " <> s)
        return
        (parseEnumParamValueText $ fromString s)


start :: (MonadLogger m, MonadIO m, MonadBaseControl IO m)
      => Options
      -> WS.Session
      -> m ()
-- {{{1
start opts sess = flip runReaderT http_call_env $ do
  case optCommand opts of
    Regions -> do
      err_or_res <- flip runReaderT http_call_env $ ecsDescribeRegions Nothing
      printResultOrExist err_or_res

    EcsInstanceStatus region_id inst_ids -> do
      err_or_res <- flip runReaderT http_call_env $ ecsDescribeInstanceStatus Nothing region_id inst_ids
      printResultOrExist err_or_res

    EcsStartInstance region_id inst_ids -> do
      err_or_res <- flip runReaderT http_call_env $ ecsStartInstances Nothing region_id inst_ids
      printResultOrExist err_or_res

    EcsStopInstance inst_id -> do
      err_or_res <- flip runReaderT http_call_env $ ecsStopInstance Nothing inst_id
      printResultOrExist err_or_res

    EcsDescribeInstance region_id inst_ids -> do
      err_or_res <- flip runReaderT http_call_env $ ecsDescribeInstances Nothing region_id inst_ids
      printResultOrExist err_or_res

    DnsDescribeDomainRecords domain_name kv_list -> do
      err_or_res <- flip runReaderT http_call_env $ dnsDescribeDomainRecords Nothing domain_name (map (uncurry (&=)) kv_list)
      printResultOrExist err_or_res

    DnsUpdateDomainRecord record_id rr rtype value ttl m_priority -> do
      err_or_res <- flip runReaderT http_call_env $ dnsUpdateDomainRecord Nothing record_id rr rtype value ttl m_priority
      printResultOrExist err_or_res
  where
    http_call_env = HttpCallEnv (optAccessKeyId opts) (optAccessKeySecret opts) sess
-- }}}1


printResultOrExist :: (ToJSON a, MonadIO m, MonadLogger m)
                   => Either ApiError' (RequestId, a) -> m ()
printResultOrExist err_or_res = do
  case err_or_res of
    Left err -> do
      $logError $ "Failed: " <> utshow err
      liftIO exitFailure

    Right (_req_id, resp) -> do
      putStrLn $ toStrict $ decodeUtf8 $ AP.encodePretty resp


start' :: Options -> IO ()
start' opts = do
  sess <- WS.newAPISession
  logger_set <- newStderrLoggerSet 0
  runLoggingT
      (start opts sess)
      (appLogger logger_set (optVerbose opts))


appLogger :: LoggerSet -> Int -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
-- {{{1
appLogger logger_set verbose loc src level ls = do
    let should_log = case level of
                        LevelOther {}   -> True
                        _               -> level `elem` lv_by_v verbose

    if should_log
        then pushLogStr logger_set $ defaultLogStr loc src level ls
        else return ()
    where
        lv_by_v lv
            | lv <= 0   = [ LevelError]
            | lv == 1   = [ LevelError, LevelWarn ]
            | lv == 2   = [ LevelError, LevelWarn, LevelInfo ]
            | otherwise = [ LevelError, LevelWarn, LevelInfo, LevelDebug ]
-- }}}1


main :: IO ()
-- {{{1
main = execParser opts >>= start'
  where
    opts = info (helper <*> optionsParse)
            ( fullDesc
                <> progDesc (unlines
                    [ "执行一些阿里云平台管理查询操作"
                    ])
                <> header "aliyun-manage - 阿里云管理查询小工具"
                )
-- }}}1


utshow :: Show a => a -> Text
utshow = fromString . ushow


-- vim: set foldmethod=marker:
