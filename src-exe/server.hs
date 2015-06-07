import           Data.Network.JsonRpcServer
import           System.IO
import           System.Log.Logger          (Priority (..))

main :: IO ()
main = do
  logger <- mkLogger (StreamLogger stdout) DEBUG

  runServer $ ServerSettings "./htdocs" [] 8888 10 logger
