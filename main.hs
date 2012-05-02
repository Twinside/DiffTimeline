import Prelude
import Yesod.Default.Config (AppConfig(..), DefaultEnv( Development ))
import Application          (getApplication)
import Network.Wai.Handler.Launch (run)
import System.Environment( getArgs )
import Yesod.Logger (defaultDevelopmentLogger)

main :: IO ()
main = do
    let config = AppConfig {
        appEnv = Development,
        appPort = 3000,
        appRoot = ".",
        appHost = "",
        appExtra = ()
      }
    args <- getArgs
    case args of
        [] -> do
            logger <- defaultDevelopmentLogger
            app <- getApplication Nothing config logger
            run app

        (f:_) -> do
            logger <- defaultDevelopmentLogger
            app <- getApplication (Just f) config logger
            run app

