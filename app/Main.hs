{-# LANGUAGE OverloadedStrings #-}

module Main ( main
            ) where

import Data.Maybe
import Control.Applicative
import qualified Data.Text as Text
import Data.Text (Text)

import Text.Printf
import Network.Curl
import Text.HTML.TagSoup

import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

data Action = Start
            | Default Text
            | Author Text
            | Title Text
            | Series Text
            | Select Text
            deriving (Read, Show, Eq)

data Book = Book { bookId :: String
                 , title :: String
                 , author :: String
                 , publisher :: String
                 , year :: String
                 , pages :: String
                 , language :: String
                 , size :: String
                 , extension :: String
                 }
          deriving Show

data Model = Model { booksList :: [Book]
                   }

initialModel :: Model
initialModel = Model { booksList = []
                     }

libgenBot :: BotApp Model Action
libgenBot = BotApp { botInitialModel = initialModel
                   , botAction = flip updateToAction
                   , botHandler = handleAction
                   , botJobs = []
                   }
  where
    updateToAction :: Model -> Update -> Maybe Action
    updateToAction _ = parseUpdate $
      Start       <$ command "start"
      <|> Default <$> command "default"
      <|> Author  <$> command "author"
      <|> Title   <$> command "title"           
      <|> Series  <$> command "series"
      <|> Select  <$> text
      <|> callbackQueryDataRead

    handleAction :: Action -> Model -> Eff Action Model
    handleAction action model
      | action == Start = model <# replyText startMessage
      | action == Select msg = model <# if null $ booksList model
                                                   then replyText "No option"
                                                   else replyText msg
      | otherwise = do
          let (query, searchType) = case action of          
                Default q -> (q, "def")
                Title   q -> (q, "title")
                Author  q -> (q, "author")
                Series  q -> (q, "series")
          let m = getBooks query searchType model
          m <# showBooksList m
            where
              msg = (\(Select t) -> t) action
              showBooksList model = reply (toReplyMessage "Seleccione:") { replyMessageReplyMarkup =
                                                                           Just ( SomeInlineKeyboardMarkup
                                                                                  . options
                                                                                  . booksList
                                                                                  $ model
                                                                                )
                                                                         }
        
    options :: [Book] -> InlineKeyboardMarkup
    options books = InlineKeyboardMarkup [[ callbackButton title title ]
                                         | (i, title) <- zip
                                                         (map (Text.pack . show . bookId) books)
                                                         (map showBook books)
                                         ]
    
    startMessage = Text.unlines
      [ "Lineas de texto de comando /start"
      ]

getBooks :: Text -> String -> Model -> Model
getBooks query typeSearch model = model { booksList = [ Book { bookId = show i
                                                             , title = t
                                                             , author = a                                                                                                                    }
                                                      | (i, t, a) <- zip3 [(0 :: Int) ..] ["AA", "BB", "CC"] ["a", "b", "c"]
                                                      ]
                                        }
showBook :: Book -> Text
showBook book = Text.unwords . map Text.pack $
  [ bookId book
  , "-"
  , title book
  , "-"
  , author book
  ]

bookInfo :: Book -> Text
bookInfo book = Text.unlines . map Text.pack $
  [ "Id: " ++ bookId book
  , "Title: " ++ title book
  , "Author: " ++ author book
  , "Publisher: " ++ publisher book
  , "Year: " ++ year book
  , "Pages: " ++ pages book
  , "Language: " ++ language book
  , "Size: " ++ size book
  , "File extension: " ++ extension book
  ]

-- main = curlGetString (url "cpp" "def") [CurlNoProgress True] >>= print . parseTags . snd
main = readFile "/home/jose/api/telegram_bot"
       >>= pure
       . Token
       . Text.pack
       . init
       >>= defaultTelegramClientEnv
       >>= startBot_ (conversationBot updateChatId libgenBot)

url query searchType =
  "https://libgen.is/"
  ++ "search.php?req=" ++ query
  ++ "&open=0&view=simple&phrase=1&column=" ++ searchType



{-

update
      | isJust $ updateInlineQuery update = do
          query <- updateInlineQuery update
          let opt = inlineQueryQuery query
          Just $ Select opt
      | otherwise
-}
