{-# LANGUAGE NamedFieldPuns #-}
module Main (call, main) where
  import System.Serverman.Types
  import System.Serverman.Utils
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
  import Control.Monad.State


  call :: Service -> App ()
  call s@(Service { name, version, service })= do
    (AppState { os, arguments }) <- get

    let params@(FileSharingParams { directory, port, user, pass, anonymous, anonymousWrite, writable, recreateUser }) = toFSParams arguments

    let content = show params
        config = "/etc/"
        original = config </> "vsftpd.conf"
        userList = config </> "vsftpd-serverman-user-list"

    when recreateUser $ executeRoot "userdel" [user] "" True >> return ()

    (Right opensslResponse) <- execute "openssl" ["passwd", "-1", pass] "" True
    let encryptedPassword = head . lines $ opensslResponse

    executeRoot "useradd" [user, "-d", directory, "-G", "ftp", "-p", encryptedPassword] "" True

    liftIO $ do
      renameFileIfMissing original (original ++ ".backup")
      writeFile original content
      writeFile userList user

    result <- restartService "vsftpd"
    case result of
      Left err -> return ()
      Right _ ->
        liftIO $ putStrLn $ "restarted vsftpd"

  main :: IO ()
  main = return ()
