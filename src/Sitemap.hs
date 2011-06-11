{-# LANGUAGE NoImplicitPrelude, TypeOperators, TemplateHaskell #-}

module Sitemap where

import Control.Category
import Web.Zwaluw
import Web.Zwaluw.TH

data Sitemap
  = Home
  | AboutUs
  | Meetings
  | DutchHugDay

$(deriveRouters ''Sitemap)

rHome, rAboutUs, rMeetings, rDutchHugDay :: Router a (Sitemap :- a)

sitemap :: Router r (Sitemap :- r)
sitemap = id / (rHome <> rAboutUs <> rMeetings <> rDutchHugDay)

