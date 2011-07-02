{-# LANGUAGE OverloadedStrings #-}

{-|

This is where all the routes and handlers are defined for your site. The
'site' function combines everything together and is exported by this module.

-}

module Site
  ( site
  ) where

import           Control.Applicative
import qualified Data.ByteString as BS
import           Data.Text (pack)
import           Snap.Extension.Heist
import           Snap.Util.FileServe
import           Snap.Types
import           Text.Templating.Heist

import           Application
import           Router
import           Sitemap

------------------------------------------------------------------------------
-- | Renders a page.
page :: Sitemap -> Application ()
page s = do
  let nm = show s
  heistLocal (bindString "name" (pack nm)) $ render "page"

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

