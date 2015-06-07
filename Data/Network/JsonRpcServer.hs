{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.Network.JsonRpcServer where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Char8          as B8
import qualified Data.ByteString.Lazy           as BL
import qualified Data.ByteString.Lazy.Char8     as BL8
import qualified Data.HashMap.Strict            as HM
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import           Data.Time.Clock                (diffUTCTime, getCurrentTime)
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp       (defaultSettings, runSettings,
                                                 setBeforeMainLoop, setOnClose,
                                                 setOnException, setPort)
import           System.IO
import           System.Log.Formatter           (simpleLogFormatter)
import           System.Log.Handler             (setFormatter)
import           System.Log.Handler.Simple      (fileHandler, streamHandler)
import           System.Log.Logger              (Logger, Priority (..),
                                                 getLogger, logL, removeHandler,
                                                 rootLoggerName, setHandlers,
                                                 setLevel, updateGlobalLogger)
import           Text.Printf                    (printf)

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

    unless (isValidJsonRpcVer jver) $
      fail "Invalid JSON RPC version"

    case params of
      (Array _)  -> return ()
      (Object _) -> return ()
      _          -> fail "Invalid parameter object"

    return $ JsonRpcRequest jid jver method params

  parseJSON _ = fail "Top level container is not an object"

instance (FromJSON r) => FromJSON (JsonRpcResponse r) where
  parseJSON (Object v) = parseResult `mplus` parseError
    where
      parseResult = do
        jver   <- v .:? "jsonrpc" .!= Null
        jid    <- v .: "id"
        result <- v .: "result"

        unless (isValidJsonRpcVer jver) $
          fail "Invalid JSON RPC version"

        return $ JsonRpcResult jid jver result

      parseError = do
        jver         <- v .:? "jsonrpc" .!= Null
        jid          <- v .: "id"
        errorObj     <- v .: "error"
        errorCode    <- errorObj .: "code"
        errorMessage <- errorObj .: "message"
        errorData    <- errorObj .:? "data" .!= Null

        unless (isValidJsonRpcVer jver) $
          fail "Invalid JSON RPC version"

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
  { jrRouteEndpoint :: !T.Text
  , jrRouteMethod   :: !T.Text
  , jrRouteFunc     :: JsonRPCFunc
  }

instance Show JsonRPCRoute where
  show JsonRPCRoute{..} =
    printf "JSON RPC Route: endpoint=%s method=%s"
    (T.unpack jrRouteEndpoint) (T.unpack jrRouteMethod)

type JsonRPCRouteMap = HM.HashMap (T.Text, T.Text) JsonRPCFunc

data LoggerType = FileLogger FilePath | StreamLogger Handle

data ServerSettings = ServerSettings
  { ssRootFolder :: FilePath        -- ^ root folder to server static files from
  , ssRPCRoutes  :: [JsonRPCRoute]  -- ^ list of json rpc routes
  , ssPort       :: Int             -- ^ port number
  , ssTimeout    :: Int             -- ^ timeout value given in seconds
  , ssLogger     :: Logger          -- ^ server logger
  }

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

mkRouteMap :: [JsonRPCRoute] -> JsonRPCRouteMap
mkRouteMap = HM.fromList . map f
  where
    f JsonRPCRoute{..} = ((jrRouteEndpoint, jrRouteMethod), jrRouteFunc)

jsonRPCRouterApp :: Logger -> JsonRPCRouteMap -> Application
jsonRPCRouterApp logger routeMap request respond = do
  reqBody  <- requestBody request

  case eitherDecode' . BL.fromStrict $ reqBody of
   Left err -> do
     logL logger ERROR $
        printf "JSON RPC request parse error:  %s" err
     respond $ mkHTTPErrorResp status400
   Right rpcReq@JsonRpcRequest{..} -> do
     let endpoint = T.decodeUtf8 $ rawPathInfo request
         rpcRes = case HM.lookup (endpoint, jrReqMethod) routeMap of
           Just rpcFunc -> rpcFunc jrReqParams
           Nothing      -> JsonRpcError jrReqId jrReqVer (-32601)
                           "Method not found" Null

     logRPCAction endpoint rpcReq rpcRes

     respond . responseLBS status200 [] $ encode rpcRes

  where
    mkHTTPErrorResp status = responseLBS status [] BL.empty

    logRPCAction endpoint JsonRpcRequest{..} JsonRpcResult{..} =
      logL logger INFO $
      printf "JSON RPC: id=%s endpoint=%s method=%s params=%s result=%s"
      (showJson jrReqId) (T.unpack endpoint) (T.unpack jrReqMethod)
      (showJson jrReqParams) (showJson jrResResult)

    logRPCAction endpoint JsonRpcRequest{..} JsonRpcError{..} =
      logL logger INFO $
      printf "JSON RPC: id=%s endpoint=%s method=%s params=%s error=(%d, %s)"
      (showJson jrReqId) (T.unpack endpoint) (T.unpack jrReqMethod)
      (showJson jrReqParams) jrErrCode (show jrErrMessage)

    showJson = BL8.unpack . encode

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

serverApp :: ServerSettings -> Application
serverApp ServerSettings{..} request respond = do
  startTS <- getCurrentTime
  appDispatcher $ wrappedRespond startTS

  where
    routerMap = mkRouteMap ssRPCRoutes

    staticServerSettings = defaultFileServerSettings ssRootFolder

    mkHTTPErrorResp status = responseLBS status [] BL.empty

    appDispatcher respond'
      -- POST, let the router app handle it
      | requestMethod request == methodPost =
          jsonRPCRouterApp ssLogger routerMap request respond'
      -- GET, let the static file server app handle it
      | requestMethod request == methodGet =
          staticApp staticServerSettings request respond'
      | otherwise = respond $ mkHTTPErrorResp status405 -- method not allowed

    wrappedRespond startTS response = do
      endTS <- getCurrentTime

      -- elapsed time in seconds
      let elapsed = (floor $ diffUTCTime endTS startTS) :: Integer

      -- Access log format: time elapsed remotehost status bytes method URL type
      logL ssLogger INFO $
        printf "%6d %10s %5d %5s %5s %10s"
        elapsed (show $ remoteHost request)
        (statusCode $ responseStatus response)
        -- TODO: warp never sets the content-length header...
        (showHeader hContentLength $ responseHeaders response)
        (B8.unpack $ requestMethod request)
        (B8.unpack $ rawPathInfo request)

      respond response

    showHeader headerName headers
      | Just header <- lookup headerName headers = B8.unpack header
      | otherwise = "-"

runServer :: ServerSettings -> IO ()
runServer settings@ServerSettings{..} = do
  -- remove root logger's default handler
  updateGlobalLogger rootLoggerName removeHandler

  runSettings warpSettings $ serverApp settings

  where
    warpSettings = setPort ssPort .
                   setBeforeMainLoop beforeMain .
                   setOnClose onClose .
                   setOnException onException' $
                   defaultSettings

    beforeMain =
      logL ssLogger INFO $
        printf "Server started listening on port %d" ssPort

    onClose sockAddr =
      logL ssLogger DEBUG $
        printf "Connection from %s closed" (show sockAddr)

    onException' (Just req) exception =
      logL ssLogger ERROR $
        printf "Server error when serving the request %s: %s"
        (show req) (show exception)

    onException' _ _ = return ()

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

mkLogger :: LoggerType -> Priority -> IO Logger
mkLogger loggerType prio = do
  handler <- case loggerType of
    FileLogger fp   -> fileHandler fp prio
    StreamLogger hd -> streamHandler hd prio

  logger  <- getLogger "server"

  let formatter = simpleLogFormatter "$time [$prio] $msg"

  return $ setHandlers [setFormatter handler formatter] .
           setLevel prio $
           logger
