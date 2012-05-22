import Prelude
import Data.Maybe( listToMaybe )
import Yesod.Default.Config( AppConfig(..)
                           , DefaultEnv( Development )
                           )

import Difftimeline.Application( getApplication )
import Network.Wai.Handler.Launch( runUrlPort )
import System.Environment( getArgs )
import Yesod.Logger( defaultDevelopmentLogger )
import Network.Socket( socket
                     , Family( AF_INET )
                     , SocketType( Datagram )
                     , defaultProtocol
                     , aNY_PORT
                     , sClose
                     , bindSocket
                     , SockAddr( SockAddrInet )
                     , iNADDR_ANY
                     , PortNumber( PortNum )
                     , socketPort
                     )

-- | Ugly workaround to find a free port :
-- create a socket with any port as configuration and retrieve
-- it.
-- Not safe at all, as after that, a race condition can happen
-- in the system, leaving a wrong socket down the flow
findNextPort :: IO Int
findNextPort = do
    s <- socket AF_INET Datagram defaultProtocol
    bindSocket s (SockAddrInet aNY_PORT iNADDR_ANY)
    PortNum i <- socketPort s
    sClose s
    return $ fromIntegral i

main :: IO ()
main = do
    args <- getArgs
    usePort <- findNextPort
    let config = AppConfig {
            appEnv = Development,
            appPort = usePort,
            appRoot = ".",
            appHost = "",
            appExtra = ()
      }
    logger <- defaultDevelopmentLogger
    app <- getApplication (listToMaybe args) config logger
    runUrlPort usePort "" app

