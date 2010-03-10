{-# LANGUAGE QuasiQuotes #-}
import Yesod
import Yesod.Helpers.Static
import qualified Data.Object.Yaml
import qualified Safe.Failure

data DutchHug = DutchHug
    { settings :: Settings
    , templateGroup :: TemplateGroup
    }

data Settings = Settings
    { sApproot :: String
    , staticRoot :: String
    , staticDir :: String
    , templateDir :: String
    , portNumber :: Int
    }

settingsFile :: FilePath
settingsFile = "settings.yaml"

loadSettings :: IO Settings
loadSettings = do
    m <- Data.Object.Yaml.decodeFile settingsFile >>= fromMapping
    ar <- lookupScalar "approot" m
    sr <- lookupScalar "static-root" m
    sd <- lookupScalar "static-dir" m
    td <- lookupScalar "template-dir" m
    pn <- lookupScalar "port" m >>= Safe.Failure.read
    return $ Settings ar sr sd td pn

loadDutchHug :: IO DutchHug
loadDutchHug = do
    s <- loadSettings
    tg <- loadTemplateGroup $ templateDir s
    return $ DutchHug s tg

main :: IO ()
main = do
    datatype <- loadDutchHug
    app <- toWaiApp datatype
    basicHandler (portNumber $ settings datatype) app

instance Yesod DutchHug where
    resources = [$mkResources|
/:
    GET: homepageH
/static/*: serveStatic'
|]
    applyLayout = defaultApplyLayout

instance YesodApproot DutchHug where
    approot = sApproot . settings

instance YesodTemplate DutchHug where
    getTemplateGroup = templateGroup
    defaultTemplateAttribs y _ = return
        . setHtmlAttrib "approot" (approot y)
        . setHtmlAttrib "staticroot" (staticRoot $ settings y)

homepageH :: Handler DutchHug RepHtml
homepageH = templateHtml "homepage" return

serveStatic' :: Method -> [String]
             -> Handler DutchHug [(ContentType, Content)]
serveStatic' method pieces = do
    y <- getYesod
    let sd = staticDir $ settings y
    serveStatic (fileLookupDir sd) method pieces