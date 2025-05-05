{-# LANGUAGE OverloadedStrings #-}

module Bot ( run
           ) where

import Libgen

import Data.Maybe
import Control.Applicative
import qualified Data.Text as Text
import Data.Text (Text)
import System.IO.Unsafe

import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

data Action
  = Start
  | Default Text
  | Author Text
  | Title Text
  | Series Text
  | Select Text
  | Download Text
  deriving (Read, Show, Eq)

data Model = Model
  { booksList :: [Book]
  , selectedBook :: Maybe Book
  }

initialModel :: Model
initialModel = Model
  { booksList = []
  , selectedBook = Nothing
  }

libgenBot :: BotApp Model Action
libgenBot = BotApp
  { botInitialModel = initialModel
  , botAction = flip updateToAction
  , botHandler = handleAction
  , botJobs = []
  }
  where
    updateToAction :: Model -> Update -> Maybe Action
    updateToAction _ = parseUpdate $
      Start       <$  command "start"
      <|> Default <$> command "default"
      <|> Author  <$> command "author"
      <|> Title   <$> command "title"           
      <|> Series  <$> command "series"
      <|> callbackQueryDataRead

    handleAction :: Action -> Model -> Eff Action Model
    handleAction action model = case action of
      Start -> model <# replyText startMessage
      Download _ -> model <# if isNothing $ selectedBook model
        then replyText $ Text.pack "No selected book."
        else do let url = (urls . fromJust . selectedBook $ model) !! (read . Text.unpack $ msg :: Int)
                let fileUrl = unsafePerformIO . downloadBook $ url
                replyText $ Text.pack fileUrl
      Select _ -> do
        let m = selectBook book model
        m <# do
          if isNothing $ selectedBook m
            then replyText $ Text.pack "No option."
            else do let b = fromJust $ selectedBook m
                    reply (toReplyMessage $ bookInfo b) { replyMessageParseMode = Just Markdown }
                    showBookMirrors $ urls b
      Default query -> do
        let m = getBooks query "def" model
        m <# showBooksList m
      Title query -> do
        let m = getBooks query "title" model
        m <# showBooksList m
      Author query -> do
        let m = getBooks query "author" model
        m <# showBooksList m
      Series query -> do
        let m = getBooks query "series" model
        m <# showBooksList m
      where
        msg = fromJust . getText $ action
        book = findBook (Text.unpack msg) $ booksList model
        
        showBooksList m =
          reply (toReplyMessage "Select a book:") { replyMessageReplyMarkup =
                                                      Just ( SomeInlineKeyboardMarkup
                                                             . booksInlineKeyboard
                                                             . booksList
                                                             $ m
                                                           )
                                                  }
        
        booksInlineKeyboard :: [Book] -> InlineKeyboardMarkup
        booksInlineKeyboard books = InlineKeyboardMarkup
          [[ actionButton t (Select i) ]
          | (i, t) <- zip
                      (map (Text.pack . bookId) books)
                      (map showBook books)
          ]

        showBookMirrors mirrors =
          reply (toReplyMessage "Select a download option:") { replyMessageReplyMarkup =
                                                                 Just ( SomeInlineKeyboardMarkup
                                                                        . bookMirrorsInlineKeyboard
                                                                        $ mirrors
                                                                      )
                                                             }

        bookMirrorsInlineKeyboard :: [String] -> InlineKeyboardMarkup
        bookMirrorsInlineKeyboard mirrors = InlineKeyboardMarkup
          [[ actionButton (Text.pack $ "Option " ++ show i) (Download $ Text.pack . show $ i) ]
          | (i, _) <- zip [(0 :: Int) ..] mirrors
          ]
    
    startMessage = Text.unlines
      [ "Lineas de texto de comando /start"
      ]

getText :: Action -> Maybe Text
getText (Select t) = Just t
getText (Download t) = Just t
getText _ = Nothing

getBooks :: Text -> String -> Model -> Model
getBooks query searchType model = model
  { booksList = unsafePerformIO . searchBooks . libgen (Text.unpack query) $ searchType
  }

selectBook :: Maybe Book -> Model -> Model
selectBook Nothing model = model { selectedBook = Nothing }
selectBook book model = model { selectedBook = book }

findBook :: String -> [Book] -> Maybe Book
findBook _ [] = Nothing
findBook i (b:bs)
  | i == bookId b = Just b
  | otherwise = findBook i bs

showBook :: Book -> Text
showBook book = Text.unwords . map Text.pack $
  [ bookId book
  , "-"
  , title book
  ]

bookInfo :: Book -> Text
bookInfo book = Text.unlines . map Text.pack $
  [ "*Book Information*"
  , "- *Id*: \"" ++ bookId book ++ "\""
  , "- *Title*: \"" ++ title book ++ "\""
  , "- *Author*: \"" ++ author book ++ "\""
  , "- *Publisher*: \"" ++ publisher book ++ "\""
  , "- *Year*: \"" ++ year book ++ "\""
  , "- *Pages*: \"" ++ pages book ++ "\""
  , "- *Language*: \"" ++ language book ++ "\""
  , "- *Size*: \"" ++ size book ++ "\""
  , "- *File extension*: \"" ++ extension book ++ "\""
  ]

run :: String -> IO ()
run token =
  pure token
  >>= pure
  . Token
  . Text.pack
  . init
  >>= defaultTelegramClientEnv
  >>= startBot_ (conversationBot updateChatId libgenBot)
