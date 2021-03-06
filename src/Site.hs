{-# LANGUAGE OverloadedStrings #-}

{-|

This is where all the routes and handlers are defined for your site. The
'site' function combines everything together and is exported by this module.

-}

module Site
  ( site
  ) where

import           Control.Applicative
import           Control.Monad.IO.Class
import qualified Data.ByteString as BS
import           Data.Text (pack, replace)
import           Data.Text.Encoding (encodeUtf8)
import           Snap.Extension.Heist
import           Snap.Util.FileServe
import           Snap.Types
import           Text.Templating.Heist
import           Text.XmlHtml (parseHTML, Document (..), Node(..))

import           Application
import           Router
import           Sitemap
import           WikiScrape

------------------------------------------------------------------------------
-- | Renders a page.
page :: Sitemap -> Application ()
page Home = do
  upcoming <- liftIO $ wikiTemplate "Upcoming"
  previously <- liftIO $ wikiTemplate "Previously"
  intro <- liftIO $ wikiTemplate "Intro"
  heistLocal
    (bindSplices
       [ ("upcoming", htmlSplice upcoming)
       , ("previously", htmlSplice previously)
       , ("intro", htmlSplice intro)
       ]
    )
    $ render "home"
page AboutUs = do
  aboutUs <- liftIO $ wikiPage "AboutUs"
  heistLocal
    (bindSplice "about-us" $ htmlSplice aboutUs)
    $ render "about-us"
page Meetings = do
  meetings <- liftIO $ wikiPage "Meetings"
  heistLocal
    (bindSplice "meetings" $ htmlSplice meetings)
    $ render "meetings"
page DutchHugDay = do
  dutchhugday <- liftIO $ wikiPage "DutchHugDay"
  heistLocal
    (bindSplice "dutchhugday" $ htmlSplice dutchhugday)
    $ render "dutchhugday"
page DhdUHac = do
  dhdUHac <- liftIO $ wiki "" "DHD_UHac"
  heistLocal
    (bindSplice "dhd-uhac" $ htmlSplice dhdUHac)
    $ render "dhd-uhac"

htmlSplice :: Monad m => String -> Splice m
htmlSplice html = do
  case parseHTML "HaskellWiki" (encodeUtf8 . f . pack $ html) of
    Left s  -> return [TextNode $ pack s]
    Right d -> return $ docContent d
  where
    -- Ugly hack to get page parsed
    f = replace "</img>" ""

wikiPage :: String -> IO String
wikiPage = wiki "Dutch_HUG/"

wikiTemplate :: String -> IO String
wikiTemplate = wiki "Template:Dutch_HUG/"

wiki :: String -> String -> IO String
wiki prefix name = cachedScrape (prefix ++ name)

------------------------------------------------------------------------------
-- | Redirect urls from old site to new versions
redirectOldUrls :: Application ()
redirectOldUrls = redirects $ pages ++ files
  where
    pages = concatMap f ps
    f (fn, tn) = [ ("/" `BS.append` fn, "/" `BS.append` tn)
                 , ("/" `BS.append` fn `BS.append` "/", "/" `BS.append` tn)
                 ]
    ps =
      [ ("AboutUs", "about-us"), ("Meetings", "meetings")
      , ("DutchHugDay", "dutchhugday")
      ]
    files = map (\fn -> ( "/static/dutchhugday-2010" `BS.append` fn
                        , "/media/dutchhugday-2010" `BS.append` fn))
                fs
    fs =
      [ "blazehtml.pdf", "clash.pdf"
      , "functional-programming-in-the-industry.pdf"
      , "functional-programming-typlab.pdf", "lightweight-monadic-regions.pdf"
      , "lightweight-program-inversion.pdf", "sirenial.pdf"
      , "why-haskell-does-not-matter.pdf"
      ]

redirects :: [(BS.ByteString, BS.ByteString)] -> Application ()
redirects = route . map (\(o, n) -> (o, redirect' n 301))

------------------------------------------------------------------------------
-- | The main entry point handler.
site :: Application ()
site =  router sitemap page
    <|> redirectOldUrls
    <|> serveDirectory "resources/static"

