{-# LANGUAGE NamedFieldPuns #-}
module Types ( FileSharingParams (..)
             , toFSParams) where
  import System.Serverman.Utils
  import Data.Default.Class

  toFSParams :: [(String, Maybe String)] -> FileSharingParams
  toFSParams (("directory", Just value):xs) = (toFSParams xs) { directory = value }
  toFSParams (("user", Just value):xs) = (toFSParams xs) { user = value }
  toFSParams (("pass", Just value):xs) = (toFSParams xs) { pass = value }
  toFSParams (("port", Just value):xs) = (toFSParams xs) { port = value }
  toFSParams (("writable", Nothing):xs) = (toFSParams xs) { writable = True }
  toFSParams (("anonymous", Nothing):xs) = (toFSParams xs) { anonymous = True }
  toFSParams (("anonymous-write", Nothing):xs) = (toFSParams xs) { anonymousWrite = True }
  toFSParams (("recreate-user", Nothing):xs) = (toFSParams xs) { recreateUser = True }
  toFSParams (_:xs) = (toFSParams xs)
  toFSParams _ = def

  data FileSharingParams = FileSharingParams { directory      :: FilePath
                                             , user           :: String
                                             , pass           :: String
                                             , port           :: String
                                             , writable       :: Bool
                                             , anonymous      :: Bool
                                             , anonymousWrite :: Bool
                                             , recreateUser   :: Bool
                                             } deriving (Eq)

  instance Default FileSharingParams where
    def = FileSharingParams { directory      = "/srv/ftp/serverman" 
                            , user           = "serverman"
                            , pass           = "serverman"
                            , port           = "20"
                            , writable       = True
                            , anonymous      = False
                            , anonymousWrite = False
                            , recreateUser   = False
                            } 
  instance Show FileSharingParams where
    show (FileSharingParams { directory, user, pass, port, writable, anonymous, anonymousWrite }) =
      let boolToEnglish True  = "YES"
          boolToEnglish False = "NO"
      in 
        keyvalue [ ("anonymous_enable", boolToEnglish anonymous)
                 , ("write_enable", boolToEnglish writable)
                 , ("allow_writeable_chroot", boolToEnglish writable)
                 , ("anon_upload_enable", boolToEnglish anonymousWrite)
                 , ("anon_mkdir_write_enable", boolToEnglish anonymousWrite)
                 , ("listen", "YES")
                 , ("userlist_enable", "YES")
                 , ("userlist_file", "/etc/vsftpd-serverman-user-list")
                 , ("userlist_deny", "NO")
                 , ("local_root", directory)
                 , ("xferlog_enable", "YES")
                 , ("local_enable", "YES")
                 , ("pam_service_name", "ftp")] "="
