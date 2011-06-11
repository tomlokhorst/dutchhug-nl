{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, TypeOperators
           , TemplateHaskell #-}

module Sitemap where

import Prelude (Show)
import Control.Category
import Web.Zwaluw
import Web.Zwaluw.TH

data Sitemap
  = Home
  | AboutUs
  | Meetings
  | DutchHugDay
  deriving Show

$(deriveRouters ''Sitemap)

rHome, rAboutUs, rMeetings, rDutchHugDay :: Router a (Sitemap :- a)

sitemap :: Router r (Sitemap :- r)
sitemap = id /
  (  rHome
  <> rAboutUs . "about-us"
  <> rMeetings . "meetings"
  <> rDutchHugDay . "dutchhugday"
  )

