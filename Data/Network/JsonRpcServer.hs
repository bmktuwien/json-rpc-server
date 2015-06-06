{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.Network.JsonRpcServer where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy           as BL
import qualified Data.HashMap.Strict            as HM
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp       (defaultSettings, runSettings,
                                                 setPort)
import           System.IO
import           System.Log.Formatter           (simpleLogFormatter)
import           System.Log.Handler             (setFormatter)
import           System.Log.Handler.Simple      (fileHandler, streamHandler)
import           System.Log.Logger              (Logger, Priority (..),
                                                 addHandler, getLogger)

data JsonRpcRequest = JsonRpcRequest
  { jrReqId     :: !Value
  , jrReqVer    :: !Value
  , jrReqMethod :: !T.Text
  , jrReqParams :: !Value
  } deriving (Show)

data JsonRpcResponse r =
    JsonRpcResult { jrResId     :: !Value
                  , jrResVer    :: !Value
                  , jrResResult :: r
                  }
  | JsonRpcError  { jrErrId      :: !Value
                  , jrErrVer     :: !Value
                  , jrErrCode    :: !Int
                  , jrErrMessage :: !T.Text
                  , jrErrData    :: !Value
                  }
  deriving (Show)

isValidJsonRpcVer :: Value -> Bool
isValidJsonRpcVer (String "2.0") = True
isValidJsonRpcVer (Null)         = True
isValidJsonRpcVer _              = False

instance FromJSON JsonRpcRequest where
  parseJSON (Object v) = do
    jver    <- v .:? "jsonrpc" .!= Null -- if null it's jsonrpc 1.0
    jid     <- v .:? "id" .!= Null      -- if null it's a notification
    method  <- v .:  "method"
    params  <- v .:? "params" .!= emptyObject

    unless (isValidJsonRpcVer jver) mzero

    case params of
      (Array _)  -> return ()
      (Object _) -> return ()
      _          -> mzero

    return $ JsonRpcRequest jid jver method params

  parseJSON _ = mzero

instance (FromJSON r) => FromJSON (JsonRpcResponse r) where
  parseJSON (Object v) = parseResult `mplus` parseError
    where
      parseResult = do
        jver   <- v .:? "jsonrpc" .!= Null
        jid    <- v .: "id"
        result <- v .: "result"

        unless (isValidJsonRpcVer jver) mzero

        return $ JsonRpcResult jid jver result

      parseError = do
        jver         <- v .:? "jsonrpc" .!= Null
        jid          <- v .: "id"
        errorObj     <- v .: "error"
        errorCode    <- errorObj .: "code"
        errorMessage <- errorObj .: "message"
        errorData    <- errorObj .:? "data" .!= Null

        unless (isValidJsonRpcVer jver) mzero

        return $ JsonRpcError jid jver errorCode errorMessage errorData

  parseJSON _ = mzero

instance ToJSON JsonRpcRequest where
  toJSON JsonRpcRequest{..} = object [ "jsonrpc" .= jrReqVer
                                     , "id"      .= jrReqId
                                     , "method"  .= jrReqMethod
                                     , "params"  .= jrReqParams
                                     ]

instance ToJSON r => ToJSON (JsonRpcResponse r) where
  toJSON (JsonRpcResult jid jver result) =
    object [ "jsonrpc" .= jver
           , "id"      .= jid
           , "result"  .= result
           ]

  toJSON (JsonRpcError jid jver errCode errMessage errData) =
    object [ "jsonrpc" .= jver
           , "id"      .= jid
           , "error"   .= errObj
           ]
    where
      errObj = object [ "code"    .= errCode
                      , "message" .= errMessage
                      , "data"    .= errData
                      ]

type JsonRPCFunc = Value -> JsonRpcResponse Value

data JsonRPCRoute = JsonRPCRoute
  { jrRouteDomain :: !T.Text
  , jrRouteMethod :: !T.Text
  , jrRouteFunc   :: JsonRPCFunc
  }

instance Show JsonRPCRoute where
  show JsonRPCRoute{..} =
    concat [ "Domain: "
           , T.unpack jrRouteDomain
           , "Method: "
           , T.unpack jrRouteMethod
           ]

type JsonRPCRouteMap = HM.HashMap (T.Text, T.Text) JsonRPCFunc

data LoggerType = FileLogger FilePath | StreamLogger Handle

data ServerSettings = ServerSettings
  { ssRootFolder   :: FilePath        -- ^ root folder to server static files from
  , ssRPCRoutes    :: [JsonRPCRoute]  -- ^ list of json rpc routes
  , ssPort         :: Int             -- ^ port number
  , ssTimeout      :: Int             -- ^ timeout value given in seconds
  , ssServerLogger :: Logger          -- ^ server logger
  }

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

mkRouteMap :: [JsonRPCRoute] -> JsonRPCRouteMap
mkRouteMap = HM.fromList . map f
  where
    f JsonRPCRoute{..} = ((jrRouteDomain, jrRouteMethod), jrRouteFunc)

jsonRPCRouterApp :: JsonRPCRouteMap -> Application
jsonRPCRouterApp routeMap request respond = do
  eRequest <- eitherDecode' . BL.fromStrict <$> requestBody request
  case eRequest of
   Left _ -> respond $ mkHTTPErrorResp status400
   Right JsonRpcRequest{..} -> do
     let domain = T.decodeUtf8 $ rawPathInfo request
         response = case HM.lookup (domain,jrReqMethod) routeMap of
           Just rpcFunc -> rpcFunc jrReqParams
           Nothing      -> JsonRpcError jrReqId jrReqVer (-32601)
                           "Method not found" Null

     respond . responseLBS status200 [] $ encode response

  where
    mkHTTPErrorResp status = responseLBS status [] BL.empty

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

serverApp :: ServerSettings -> Application
serverApp ServerSettings{..} request respond
    -- POST, let the router app handle it
  | requestMethod request == methodPost = handle errorHandler $
      jsonRPCRouterApp routerMap request respond
    -- GET, let the static file server handle it
  | requestMethod request == methodGet = handle errorHandler $
      staticApp staticServerSettings request respond

  | otherwise = respond $ mkHTTPErrorResp status405 -- method not allowed

  where
    routerMap = mkRouteMap ssRPCRoutes
    staticServerSettings = defaultFileServerSettings ssRootFolder

    mkHTTPErrorResp status = responseLBS status [] BL.empty

    errorHandler :: SomeException -> IO ResponseReceived
    errorHandler _ =
      respond $ mkHTTPErrorResp status500 -- internal server error

runServer :: ServerSettings -> IO ()
runServer settings@ServerSettings{..} = do
  runSettings warpSettings $ serverApp settings

  where
    warpSettings = setPort ssPort $ defaultSettings

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

defaultServerLogger :: LoggerType -> Priority -> IO Logger
defaultServerLogger loggerType prio = do
  handler <- case loggerType of
    FileLogger fp   -> fileHandler fp prio
    StreamLogger hd -> streamHandler hd prio

  logger  <- getLogger "server"

  let formatter = simpleLogFormatter "$time [$prio] $msg"

  return $ addHandler (setFormatter handler formatter) logger
