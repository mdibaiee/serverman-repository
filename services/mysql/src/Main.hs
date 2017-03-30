{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (call, main) where
  import System.Serverman.Types
  import System.Serverman.Utils hiding (execute)
  import Types

  import "mysql-haskell" Database.MySQL.Base
  import qualified Data.ByteString.Char8 as BS
  import qualified Data.ByteString.Lazy as BL
  import qualified Data.Text as T
  import Data.List
  import Control.Monad
  import Control.Monad.State hiding (liftIO)

  help :: App String
  help = return $
          mkHelp "mysql [--options]"
                  [ ("--database <name>", "database name, default: serverman")
                  , ("--user <username>", "database username, default: serverman")
                  , ("--password <password>", "database password, default: serverman")
                  , ("--host <domain>", "database hostname")
                  , ("--port <num>", "database port number")
                  , ("--dummy-data", "insert dummy data into database")]

  call :: Service -> App ()
  call s@(Service { name, version, service }) = do
    (AppState { arguments }) <- get

    let params@(DatabaseParams { database, dummyData, user, pass, host, port }) = toDBParams arguments

    servermanPort <- usingPort port

    liftIO $ do
      conn <- connect $ defaultConnectInfo { ciUser = BS.pack user, ciPassword = BS.pack pass, ciHost = host, ciPort = read servermanPort }

      print $ renderParams "CREATE DATABASE IF NOT EXISTS ?" [One (MySQLText $ T.pack database)]

      execute conn "CREATE DATABASE IF NOT EXISTS ?;" [One (MySQLText $ T.pack database)]

      when dummyData $ do
        let (tableName, _, _) = dummy

        execute_ conn createDummyTables
        execute_ conn clearTable
        execute_ conn insertToDummyTables

        putStrLn $ "Created dummy table '" ++ tableName ++ "' and filled it with data."
        return ()

    clearPort servermanPort

    return ()

  clearTable :: Query
  clearTable = renderParams "DELETE FROM ?" [One (MySQLBytes $ BS.pack tableName)]
    where (tableName, _, _) = dummy

  createDummyTables :: Query
  createDummyTables = createTable dummy
    where
      createTable (tableName, columns, rows) = renderParams "CREATE TABLE IF NOT EXISTS ? (?)" [One (MySQLBytes $ BS.pack tableName), Many (map columnDef columns)]
      columnDef "children" = MySQLText "children INT"
      columnDef "birth_date" = MySQLText "birth_date DATETIME"
      columnDef "gender" = MySQLText "gender ENUM('Male', 'Female')"
      columnDef name = MySQLBytes (BL.toStrict . fromQuery $ renderParams "? VARCHAR(255)" [One (MySQLBytes $ BS.pack name)])

  insertToDummyTables :: Query
  insertToDummyTables = insertTable dummy
    where
      insertTable (tableName, _, rows) = renderParams "INSERT INTO ? VALUES (?)" [One (MySQLBytes $ BS.pack tableName), Many (map insertRow rows)]
      insertRow row = MySQLBytes (BL.toStrict . fromQuery $ renderParams "?" [Many $ map (MySQLBytes . BS.pack) row])
    


  main :: IO ()
  main = return ()
