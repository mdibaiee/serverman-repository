{-# LANGUAGE NamedFieldPuns #-}
module Main (call, main) where
  import System.Serverman.Types
  import System.Serverman.Utils hiding (execute)
  import Types

  import qualified Database.MongoDB as DB
  import qualified Data.ByteString.Char8 as BS
  import Data.List hiding (delete)
  import qualified Data.Text as T
  import Control.Monad
  import Control.Monad.State hiding (liftIO)
  import System.IO.Error

  help :: App String
  help = return $
          mkHelp "mongodb [--options]"
                  [ ("--database <name>", "database name, default: serverman")
                  , ("--user <username>", "database username, default: serverman")
                  , ("--password <password>", "database password, default: serverman")
                  , ("--host <domain>", "database hostname")
                  , ("--port <num>", "database port number")
                  , ("--dummy-data", "insert dummy data into database")]

  call :: Service -> App ()
  call s@(Service { name, version, service })= do
    (AppState { arguments }) <- get

    let params@(DatabaseParams { database, dummyData, user, pass, host }) = toDBParams arguments
        run = do
          when dummyData $ do
            clearCollection
            insertToCollection
            return ()
    
    liftIO $ do
      result <- tryIOError $ DB.connect (DB.readHostPort host)

      case result of
        Right pipe -> do
          e <- DB.access pipe DB.master (T.pack database) run

          DB.close pipe
        Left err -> do
          putStrLn $ show err
          putStrLn $ "[Error] could not connect to MongoDB server " ++ host

    where
      clearCollection = DB.delete (DB.select [] (T.pack collectionName))
        where (collectionName, _, _) = dummy

      insertToCollection = DB.insertMany (T.pack collectionName) records
        where
          (collectionName, definitions, rows) = dummy
          records = map (\row -> zipWith (\def value -> def DB.=: row) (map T.pack definitions) row) rows

  main :: IO ()
  main = return ()
