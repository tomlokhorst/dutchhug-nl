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
import qualified Data.ByteString.Char8 as BS8
import           Data.Text (pack)
import           Snap.Extension.Heist
import           Snap.Util.FileServe
import           Snap.Types
import           Text.Templating.Heist
import           Text.XmlHtml (parseHTML, Document (HtmlDocument))

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
page s = do
  let nm = show s
  heistLocal (bindString "name" (pack nm)) $ render "page"

htmlSplice :: Monad m => String -> Splice m
htmlSplice html = do
  let Right (HtmlDocument _ _ ns) = parseHTML "wiki" (BS8.pack html)
  return ns

wikiPage :: String -> IO String
wikiPage = wiki ""

wikiTemplate :: String -> IO String
wikiTemplate = wiki "Template:"

wiki :: String -> String -> IO String
wiki prefix name = cachedScrape (prefix ++ "Dutch_HUG/" ++ name)

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

