module WikiScrape where

import Control.Monad
import Data.List
import Network.HTTP
import System.IO
import System.Posix.Files
import System.Posix.Time
import Text.HTML.TagSoup

-- This code could really benefit from a proper parser!
scrape :: String -> IO String
scrape s = do
  body <- getResponseBody =<< simpleHTTP (getRequest $ "http://www.haskell.org/haskellwiki/" ++ s)
  let tags = parseTags body
  let ts = tags
            |> dropWhile (~/= TagOpen ("h1" :: String) [])
            |> dropWhile (not . tagComment (const True))
            |> stail
            |> dropWhile (tagText (const True))
            -- |> removeEditSections
            |> takeWhile (not . tagComment (const True))
            |> fixInternalLinks
            |> removeCloseTags
            |> removeEmptyAttrs
            |> renderTags
  return ts

infixl 5 |>

-- A little F# experiment :-)
(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

removeCloseTags :: [Tag String] -> [Tag String]
removeCloseTags = filter f
  where
    f (TagClose "hr")  = False
    f (TagClose "img") = False
    f _                = True

removeEmptyAttrs :: [Tag String] -> [Tag String]
removeEmptyAttrs = map f
  where
    f (TagOpen nm attrs) = TagOpen nm $ filter g attrs
    f t                = t
    g (_, "") = False
    g _       = True

fixInternalLinks :: [Tag String] -> [Tag String]
fixInternalLinks = map f
  where
    f (TagOpen "a"   xs) = TagOpen "a"   $ map g xs
    f (TagOpen "img" xs) = TagOpen "img" $ map g xs
    f t                  = t
    g ("href", v) = ("href", if "/" `isPrefixOf` v then "http://www.haskell.org" ++ v else v)
    g ("src",  v) = ("src",  if "/" `isPrefixOf` v then "http://www.haskell.org" ++ v else v)
    g x           = x

removeEditSections :: [Tag String] -> [Tag String]
removeEditSections = reverse . fst . foldl f ([], True)
  where
    f (ts, b) t = ( if b && not isOpen then t:ts else ts
                  , if b then not isOpen else isClose)
     where
       isOpen  = t ~== TagOpen ("div" :: String) [("class", "editsection")]
       isClose = t ~== TagClose ("div" :: String)

stail :: [a] -> [a]
stail []     = []
stail (_:xs) = xs

-- form Text.HTML.TagSoup.Match
tagText :: (str -> Bool) -> Tag str -> Bool
tagText p (TagText text) = p text
tagText _ _ = False

tagComment :: (str -> Bool) -> Tag str -> Bool
tagComment p (TagComment text) = p text
tagComment _ _ = False

-- Cached everything for a minute, not to be too hard on Haskell.org
-- Note: Ugly code ahead!
cachedScrape :: String -> IO String
cachedScrape s = do
  b <- fileExist fn
  when b $ do
    now <- epochTime
    fs  <- getFileStatus fn
    let t = modificationTime fs
    when (t + 60 < now) download
  when (not b) download
  fread fn
  where
    fn  = "cached/" ++ map (\c -> if c == '/' then '-' else c) s
    download = do
      bar <- scrape s
      fwrite fn bar
    fread f = do
      h <- openFile f ReadMode
      hSetEncoding h utf8
      ss <- hGetContents h
      return ss
    fwrite f ss = do
      h <- openFile f WriteMode
      hSetEncoding h utf8
      hPutStr h ss
      hClose h

