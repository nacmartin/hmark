{-# LANGUAGE PackageImports #-}
import Happstack.Server
import Database.HDBC
import Database.HDBC.Sqlite3
import Text.XHtml.Transitional hiding (dir)
import Text.Highlighting.Kate
import Data.Time (formatTime, getCurrentTime, UTCTime)
import System.Locale (defaultTimeLocale)
import Control.Monad (unless, msum, mzero, MonadPlus)
import "mtl" Control.Monad.Trans (MonadIO, liftIO)

main :: IO ()
main = do
  db <- handleSqlError $ connectSqlite3 "bookmarks.sql"
  createTableIfMissing db
  putStrLn "Starting Server"
  simpleHTTP nullConf{ port = 3000 } (bookmarkApp db)
  disconnect db

createTableIfMissing :: (IConnection a) => a -> IO ()
createTableIfMissing db = do
  tables <- handleSqlError $ getTables db
  unless ("bookmarks" `elem` tables) $ handleSqlError $ do
    putStrLn "Creating table"
    run db ("CREATE TABLE bookmarks (id INTEGER PRIMARY KEY AUTOINCREMENT," ++
        " url TEXT, timestamp DATE)") []
    commit db

data Bookmark = Bookmark { bookmarkId :: Integer
                         , bookmarkUrl :: String
                         , bookmarkTimestamp :: UTCTime}

nullBookmark :: Bookmark
nullBookmark = Bookmark { bookmarkId = undefined
                  , bookmarkUrl = ""
                  , bookmarkTimestamp = undefined}

saveBookmarkToDb :: (IConnection d, MonadIO m)
              => d -> Bookmark -> m Integer

saveBookmarkToDb db bookmark = do
  let query = "INSERT INTO bookmarks(url, timestamp)" ++
              " VALUES(?, ?)"
  t <- liftIO getCurrentTime
  let vals = [toSql (bookmarkUrl bookmark), toSql t]
  liftIO $ withTransaction db $ \d -> run d query vals
  [[uid]] <- liftIO $ quickQuery db "select last_insert_rowid()" []
  return (fromSql uid)

getBookmarkFromDb :: (IConnection d, MonadIO m, MonadPlus m)
            => d -> Integer -> m Bookmark
getBookmarkFromDb db uid = do
  bookmarks <- liftIO $ handleSqlError $
              quickQuery db "SELECT * FROM bookmarks ORDER BY id DESC LIMIT 1" []
  case bookmarks of
     ([_,tit,ts]:_) ->
           return Bookmark { bookmarkId = uid
                           , bookmarkUrl = fromSql tit
                           , bookmarkTimestamp = fromSql ts}
     _ -> mzero

bookmarkApp :: (IConnection a) => a -> ServerPart Response
bookmarkApp db = msum
    [ dir "nueva" $ withData (addBookmark db) 
    , nullDir >> (showBookmark db) ]

addBookmark :: IConnection d => d -> Bookmark -> ServerPart Response
addBookmark db bookmark = do
      uid <- saveBookmarkToDb db bookmark
      ok $ toResponse $ "Thans for your bookmark "++bookmarkUrl bookmark;

showBookmark :: IConnection d => d -> ServerPart Response
showBookmark db = do
  bookmark <- getBookmarkFromDb db 1
  seeOther (bookmarkUrl bookmark) (toResponse "")

instance FromData Bookmark where
  fromData = do
    burl <- look "url"
    return nullBookmark{ bookmarkUrl = burl}



--showBookmarkForm :: ServerPart Response
--showBookmarkForm = ok $ toResponse $ BookmarkForm [] nullBookmark
--
--bookmarkForm :: [String] -> bookmark -> Html
--bookmarkForm errors bookmark = gui "/" <<
--    [ ulist ! [theclass "errors"] << map (li <<) errors
--    , label << "Title "
--    , textfield "title" ! [size "50", value $ bookmarkTitle bookmark]
--    , label << "Syntax "
--    , select ! [name "syntax", value $ bookmarkSyntax bookmark] <<
--        map (\l -> option ! [value l] << l) ("":languages)
--    , submit "update" "Save"
--    , br
--    , textarea ! [name "contents", rows "20", cols "76"] <<
--        bookmarkContents bookmark ]
