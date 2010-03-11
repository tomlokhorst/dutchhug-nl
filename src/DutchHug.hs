{-# LANGUAGE QuasiQuotes #-}
import Yesod
import Yesod.Helpers.Static
import qualified Data.Object.Yaml
import qualified Safe.Failure

import Control.Monad
import Text.StringTemplate
import WikiScrape

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
/Meetings:
    GET: meetingsH
/DutchHugDay:
    GET: dutchHugDayH
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

setAttrib :: (ToSElem a) => String -> a -> HtmlTemplate -> HtmlTemplate
setAttrib k v (HtmlTemplate t) = HtmlTemplate $ setAttribute k v t

homepageH :: Handler DutchHug RepHtml
homepageH = templateHtml "homepage"
              (wikiTemplate "Intro" >=> wikiTemplate "Upcoming" >=> wikiTemplate "Previously")

dutchHugDayH :: Handler DutchHug RepHtml
dutchHugDayH = templateHtml "dutchhugday" (wikiTemplate "DutchHugDay")

meetingsH :: Handler DutchHug RepHtml
meetingsH = templateHtml "meetings" (wikiTemplate "Meetings")

wikiTemplate :: String -> (HtmlTemplate -> IO HtmlTemplate)
wikiTemplate name h = do
  s <- cachedScrape ("Dutch_HUG/" ++ name)
  return $ setAttrib name s h

serveStatic' :: Method -> [String]
             -> Handler DutchHug [(ContentType, Content)]
serveStatic' method pieces = do
    y <- getYesod
    let sd = staticDir $ settings y
    serveStatic (fileLookupDir sd) method pieces
