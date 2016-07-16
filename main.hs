import Prelude

import Control.Monad( when )
import Data.List( foldl' )
import Difftimeline.Application( getApplication, Command( .. )  )

import Difftimeline.Import
import Network.Wai.Handler.Launch( runUrlPort )
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

import System.Environment( getArgs )
import System.Exit( exitSuccess )
import System.Console.GetOpt( OptDescr( Option )
                            , ArgDescr( ReqArg, NoArg )
                            , ArgOrder( Permute )
                            , getOpt
                            , usageInfo
                            )

import Network.Wai.Middleware.RequestLogger(logStdoutDev)
import Yesod.Default.Config( AppConfig(..)
                           , DefaultEnv( Development )
                           )
import Difftimeline.Application
import Difftimeline.RequestHandler
import Data.Proxy( Proxy( .. ) )
import Servant.Server( Server, runReaderTNat, enter, serve )

version :: String
version = "1.0"

-- | Type representing flag present on the command line
data Flag = Port String
          | Help
          | Quiet
          | Dev String
          deriving Eq

-- | Command line configuration
data Conf = Conf
    { confShowHelp :: !Bool
    , confPort     :: Maybe Int
    , confCommand  :: Command
    , confDevMode  :: Maybe FilePath
    , confVerbose  :: !Bool
    }

-- | Initial and default configuration
defaultConf :: Conf
defaultConf = Conf
    { confShowHelp = False
    , confPort     = Nothing
    , confCommand  = DiffWorking
    , confDevMode  = Nothing
    , confVerbose  = True
    }

-- | Command line description
options :: [OptDescr Flag]
options =
    [ Option "p" ["port"] (ReqArg Port "number")
                "Server port number (random by default)"
    , Option "" ["help"] (NoArg Help) "Show help (this screen)"
    , Option "d" ["dev"] (ReqArg Dev "Path") "Enable dev mode (load static file from current dir)"
    , Option "q" ["quiet"] (NoArg Quiet) "Hide request received by the Difftimeline server"
    ]

commandOfRest :: [String] -> Command
commandOfRest [] = DiffWorking
commandOfRest ("compare":c1:c2:_) = DiffCompare c1 c2
commandOfRest ("blame":f:_) = DiffBlame f
commandOfRest (a:_) = DiffFile a

parseArgs :: IO (Conf, [String])
parseArgs = do
    args <- getArgs
    let (opt, rest, _) = getOpt Permute options args
        cmd = commandOfRest rest
    return (foldl' configurator (defaultConf { confCommand = cmd }) opt, [])

     where configurator c Help = c{ confShowHelp = True }
           configurator c (Dev p) = c{ confDevMode = Just p }
           configurator c (Port n) = c { confPort = Just $ read n }
           configurator c Quiet = c { confVerbose = False }

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

helpText :: String
helpText =
    "Difftimeline v" ++ version ++ "\n" ++
    "usage: difftimeline [options] [file]\n" ++
    "       difftimeline [options] [folder]\n" ++
    "       difftimeline [options] compare branch1 branch2\n" ++
    "       difftimeline [otpions] blame file\n" ++
    "\n" ++
    "Without argument, difftimeline will start diff from the current\n" ++
    "directory to HEAD.\n" ++
    "\n" ++
    "With a file, begin to track file diff\n" ++
    "\n" ++
    "With the compare command, it will compare two branches\n" ++
    "\n\n" ++
    "Options :\n" ++
    "=========\n"

main :: IO ()
main = do
    (conf, _args) <- parseArgs

    when (confShowHelp conf)
         (do putStrLn $ usageInfo helpText options
             exitSuccess)

    usePort <- case confPort conf of
        Nothing -> findNextPort
        Just p -> return p

    let config = AppConfig {
            appEnv = Development,
            appPort = usePort,
            appRoot = ".",
            appHost = "",
            appExtra = ()
      }

    app <- getApplication (confDevMode conf) (confCommand conf) config
    finalApp <-
        if confVerbose conf then logStdoutDev <$> toWaiAppPlain app
        else toWaiAppPlain app
    let server = enter (runReaderTNat app) serveDifftimeline :: Server DifftimelineAPI

    runUrlPort usePort "" $ logStdoutDev $ serve (Proxy :: Proxy DifftimelineAPI) server
-- -}
