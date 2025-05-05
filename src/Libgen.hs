module Libgen where

import Data.Maybe (fromJust)
import Text.Regex.Posix
import Network.Curl
import Text.HTML.TagSoup

data Book = Book
  { bookId :: String
  , title :: String
  , author :: String
  , publisher :: String
  , year :: String
  , pages :: String
  , language :: String
  , size :: String
  , extension :: String
  , urls :: [String]
  } deriving Show

splitL :: Eq a => [a] -> a -> [[a]]
splitL [] _ = []
splitL xs y
  | y `elem` xs = if null curr
    then splitL rest y
    else curr:(splitL rest y)
  | otherwise = []
  where
    curr = takeWhile (/= y) xs
    rest = drop 1 . dropWhile (/= y) $ xs

-- TODO: get good
makeBook :: [String] -> Maybe Book
makeBook [] = Nothing
makeBook xs =
  let i = xs !! 0
      a = xs !! 1
      t = xs !! 2
      u = words . last $ xs
  in Just Book { bookId = i
               , title = t
               , author = a
               , publisher = ""
               , year = ""
               , pages = ""
               , language = ""
               , size = ""
               , extension = ""
               , urls = u
               }

findA :: [Tag String] -> String
findA [_] = []
findA [] = []
findA (x:y:xs)
  | isTagText y
    && fromTagText y == "GET"
    && x ~== TagOpen "a" [] = fromAttrib "href" x
  | y ~== TagOpen "h2" []
    && x ~== TagOpen "a" [] = "https://libgen.gs/" ++ fromAttrib "href" x
  | otherwise = findA (y:xs)

downloadBook :: String -> IO String
downloadBook url =
  curlGetString url [ CurlNoProgress True, CurlFollowLocation True ]
  >>= pure
  . findA
  . parseTags
  . snd

parseResponse :: String -> [Book]
parseResponse =
  map (fromJust . makeBook)
  . map (map unwords)
  . map (\tags -> splitL tags "\r\n\t\t\t\t")
  . (\tags -> splitL tags "[edit]")
  . map (\tag -> if isTagText tag
                 then fromTagText tag
                 else fromAttrib "href" tag
        )
  . filter (\t -> or [ isTagText t
                       && fromTagText t /= "\n\n"
                       && (not $ fromTagText t =~ "\\[[0-9]\\]" :: Bool)
                     , t ~== TagOpen "a" []
                       && fromAttrib "title" t /= ""
                       && fromAttrib "title" t /= "Libgen Librarian"
                     ]
           )
  . dropWhile (\t -> not $ t ~== TagOpen "tr" [])
  . dropWhile (\t -> not . and $ [ isTagText t
                                 , fromTagText t == "Edit"
                                 ]
              )
  . parseTags

searchBooks :: String -> IO [Book]
searchBooks req =
  curlGetString req [ CurlNoProgress True, CurlFollowLocation True ]
  >>= pure
  . parseResponse
  . snd

libgen :: String -> String -> String
libgen query searchType =
  "https://libgen.is/"
  ++ "search.php?req=" ++ query
  ++ "&open=0&view=simple&phrase=1&column=" ++ searchType
