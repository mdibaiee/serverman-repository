{-# LANGUAGE NamedFieldPuns #-}
module Main (call, main) where
  import System.Serverman.Types
  import System.Serverman.Utils
  import System.Serverman.Log
  import Types

  import System.Directory hiding (writable)
  import System.IO
  import System.IO.Error
  import System.FilePath
  import System.Process
  import Control.Concurrent.Async
  import Control.Monad
  import Control.Monad.Free
  import Data.List
  import Data.Either
  import Control.Monad.State hiding (liftIO)
  import System.Posix (setOwnerAndGroup, getFileStatus, fileOwner)

  help :: App String
  help = return $
          mkHelp "vsftpd [--options]"
                  [ ("--directory <path>", "directory to serve, default: /srv/ftp/serverman/")
                  , ("--user <username>", "ftp server username, default: serverman")
                  , ("--password <password>", "ftp server password, default: serverman")
                  , ("--port <num>", "ftp server port number, default: 20")
                  , ("--anonymous", "allow anonymous connections, default: False")
                  , ("--anonymous-write", "allow anonymous writes, default: False")
                  , ("--writable", "allow writes to the ftp server, default: True")
                  , ("--recreate-user", "if the specified username exists, delete and create it again, otherwise leave it intact")]

  call :: Service -> App ()
  call s@Service { name, version, service }= do
    AppState { os, arguments } <- get

    let params@FileSharingParams { directory, port, user, pass, anonymous, anonymousWrite, writable, recreateUser } = toFSParams arguments

    let content = show params
        config = "/etc/"
        original = config </> "vsftpd.conf"
        userList = config </> "vsftpd-serverman-user-list"

    when recreateUser $ void $ executeRoot "userdel" [user] "" True

    (Right opensslResponse) <- execute "openssl" ["passwd", "-1", pass] "" True
    let encryptedPassword = head . lines $ opensslResponse

    executeRoot "groupadd" ["-f", "ftp"] "" False
    executeRoot "useradd" [user, "-d", directory, "-G", "ftp", "-p", encryptedPassword] "" False

    ftpId <- getGroupId (Just "ftp")
    userId <- getUserId (Just user)

    liftIO $ do
      execIfExists original $
        renameFileIfMissing original (original ++ ".backup")

      writeFile original content
      writeFile userList user
      
      createDirectoryIfMissing True directory

      setOwnerAndGroup directory userId ftpId
      writeFile (directory </> "serverman-sample") "Hello from Serverman!"

    result <- restartService "vsftpd"
    case result of
      Left err -> return ()
      Right _ ->
        info "restarted vsftpd"

  main :: IO ()
  main = return ()
