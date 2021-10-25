module AliYun.OpenAPI.BasicSpec where

-- {{{1
import ClassyPrelude
import Control.Monad.Logger
#if !MIN_VERSION_classy_prelude(1, 5, 0)
import Control.Exception (evaluate)
#endif
import Test.Hspec
import Data.Time
import Text.Shakespeare.Text (lt)

import AliYun
-- }}}1

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "openApiSignature" $ do
    it "works for example as in doc: https://help.aliyun.com/document_detail/25492.html" $ do
      let t = UTCTime (fromGregorian 2016 2 23) (timeOfDayToTime $ TimeOfDay 12 46 24)
      let key_id = AccessKeyId "testid"
      let key_secret = AccessKeySecret "testsecret"
      let api_ver = ApiVersion "2014-05-26"
      let nonce = Nonce "3ee8c1b8-83d3-44af-a94f-4e0ad82fd6cf"
      let format = ApiXml
      let http_method = HttpGet
      let params0 = [ "Action" &= asText "DescribeRegions" ]
      let ((signature, _params) , (bs_to_sign, _canonical_qs)) = openApiSignature api_ver format nonce t (key_id, key_secret) http_method params0
      bs_to_sign `shouldBe` "GET&%2F&AccessKeyId%3Dtestid%26Action%3DDescribeRegions%26Format%3DXML%26SignatureMethod%3DHMAC-SHA1%26SignatureNonce%3D3ee8c1b8-83d3-44af-a94f-4e0ad82fd6cf%26SignatureVersion%3D1.0%26Timestamp%3D2016-02-23T12%253A46%253A24Z%26Version%3D2014-05-26"
      unSignature signature `shouldBe` "OLeaidS1JvxuMvnyHOwuJ+uX5qY="


  describe "openApiResponseDetectError'" $ do
    it "works for JSON example as in doc: https://help.aliyun.com/document_detail/25491.html?spm=a2c4g.11186623.0.0.4c63363fT1iUSU" $ do
      let response_body = encodeUtf8 $
            [lt|
{
    "RequestId": "540CFF28-407A-40B5-B6A5-74Bxxxxxxxxx", /* 请求 ID */
    "HostId": "ecs.aliyuncs.com", /* 服务节点 */
    "Code": "MissingParameter.CommandId", /* 错误码 */
    "Message": "The input parameter “CommandId” that is mandatory for processing this request is not supplied." /* 错误信息 */
}
            |]

      let status_code = 400
      let content_type = "application/json"
      let action = "Dummy"
      let api_error = ApiError "540CFF28-407A-40B5-B6A5-74Bxxxxxxxxx" "ecs.aliyuncs.com" "MissingParameter.CommandId"
                            "The input parameter “CommandId” that is mandatory for processing this request is not supplied."

      fst (runIdentity (runWriterLoggingT $ openApiResponseDetectError' action content_type status_code response_body))
        `shouldBe` Left api_error

    it "works for XML example as in doc: https://help.aliyun.com/document_detail/25491.html?spm=a2c4g.11186623.0.0.4c63363fT1iUSU" $ do
      let response_body = encodeUtf8 $
            [lt|
<?xml version="1.0" encoding="UTF-8"?><!--结果的根结点-->
<Error>
    <RequestId>540CFF28-407A-40B5-B6A5-74Bxxxxxxxxx</RequestId> <!--请求 ID-->
    <HostId>ecs.aliyuncs.com</HostId> <!--服务节点-->
    <Code>MissingParameter.CommandId</Code> <!--错误码-->
    <Message>The input parameter “CommandId” that is mandatory for processing this request is not supplied.</Message> <!--错误信息-->
</Error>
            |]

      let status_code = 400
      let content_type = "application/xml"
      let action = "Dummy"
      let api_error = ApiError "540CFF28-407A-40B5-B6A5-74Bxxxxxxxxx" "ecs.aliyuncs.com" "MissingParameter.CommandId"
                            "The input parameter “CommandId” that is mandatory for processing this request is not supplied."

      fst (runIdentity (runWriterLoggingT $ openApiResponseDetectError' action content_type status_code response_body))
        `shouldBe` Left api_error
