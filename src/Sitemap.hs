{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, TypeOperators
           , TemplateHaskell #-}

module Sitemap where

import Prelude (Show, Maybe, Int, ($))
import Control.Category
import Web.Zwaluw
import Web.Zwaluw.TH

data Sitemap
  = Home
  | AboutUs
  | Meetings
  | DutchHugDay
  | DhdUHac
  deriving Show

$(deriveRouters ''Sitemap)

rHome, rAboutUs, rMeetings, rDutchHugDay, rDhdUHac :: Router a (Sitemap :- a)

sitemap :: Router r (Sitemap :- r)
sitemap = id /
  (  rHome
  <> rAboutUs . "about-us"
  <> rMeetings . "meetings"
  <> rDutchHugDay . "dutchhugday"
  <> rDhdUHac . "dhd-uhac"
  )

optSlash :: Router r r
optSlash = opt $ lit "/"

