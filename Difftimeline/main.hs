import Prelude              (IO, ($), Maybe(..) )
import Yesod.Default.Config (--fromArgs, loadDevelopmentConfig
                             AppConfig(..), DefaultEnv( Development ))
-- import Yesod.Default.Main   (defaultMain)
import Settings             (Extra(..))
import Application          (getApplication)
import Network.Wai.Handler.Launch (run)
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
    logger <- defaultDevelopmentLogger
    app <- getApplication config logger
    run app

