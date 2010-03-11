module WikiScrape where

import Control.Monad
import Network.HTTP
import System.IO
import System.Posix.Files
import System.Posix.Time
import Text.HTML.TagSoup

scrape :: String -> IO String
scrape s = do
  body <- getResponseBody =<< simpleHTTP (getRequest $ "http://haskell.org/haskellwiki/" ++ s)
  let tags = parseTags body
  let ts = tags
            |> dropWhile (/= TagOpen "h1" [("class", "pagetitle")])
            |> dropWhile (/= TagOpen "p" [("class", "subpages")])
            |> dropWhile (/= TagClose "p") |> stail
            |> removeEditSections
            |> takeWhile (/= TagOpen "div" [("class", "printfooter")])
            |> renderTags
  return ts

infixl 5 |>

-- A little F# experiment :-)
(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

removeEditSections :: [Tag String] -> [Tag String]
removeEditSections = reverse . fst . foldl f ([], True)
  where
    f (ts, b) t = ( if b && not isOpen then t:ts else ts
                  , if b then not isOpen else isClose)
     where
       isOpen  = t ~== TagOpen "div" [("class", "editsection")]
       isClose = t ~== TagClose "div"

stail :: [a] -> [a]
stail []     = []
stail (_:xs) = xs

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

