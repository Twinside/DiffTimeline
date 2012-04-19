import Prelude
import Yesod.Default.Config (--fromArgs, loadDevelopmentConfig
                             AppConfig(..), DefaultEnv( Development ))
-- import Yesod.Default.Main   (defaultMain)
import Settings             (Extra(..))
import Application          (getApplication)
import Network.Wai.Handler.Launch (run)
import System.Environment( getArgs )
-- import Network.Wai.Application.Static
import Yesod.Logger ( -- Logger, logBS, logString,
                     defaultDevelopmentLogger)
-- import Network.Wai.Middleware.RequestLogger (logCallbackDev)

main :: IO ()
main = do
    let config = AppConfig {
        appEnv = Development,
        appPort = 3000,
        appRoot = ".",
        appExtra = Extra {
                extraCopyright = "",
                extraAnalytics = Nothing
            }
      }
    -- config <- fromArgs parseExtra
    args <- getArgs
    case args of
        [] -> putStrLn "usage : Difftimeline <file>"
        (f:_) -> do
            logger <- defaultDevelopmentLogger
            app <- getApplication f config logger
            run app

