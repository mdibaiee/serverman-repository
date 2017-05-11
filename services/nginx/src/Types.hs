{-# LANGUAGE NamedFieldPuns #-}
module Types ( ServerType (..)
             , ServerParams (..)
             , toServerParams) where

  import System.Serverman.Types
  import System.Serverman.Utils

  import Data.Default.Class

  toServerParams :: [(String, Maybe String)] -> ServerParams
  toServerParams (("directory", Just value):xs) = (toServerParams xs) { directory = value, serverType = Static }
  toServerParams (("domain", Just value):xs) = (toServerParams xs) { domain = value }
  toServerParams (("port", Just value):xs) = (toServerParams xs) { port = value }
  toServerParams (("forward", Just value):xs) = (toServerParams xs) { forward = value, serverType = PortForwarding  }
  toServerParams (("email", Just value):xs) = (toServerParams xs) { email = value }
  toServerParams (("ssl", Nothing):xs) = (toServerParams xs) { ssl = True }
  toServerParams (("directory-listing", Nothing):xs) = (toServerParams xs) { directoryListing = True }
  toServerParams (_:xs) = toServerParams xs
  toServerParams _ = def

  data ServerType = Static | PortForwarding deriving (Show, Eq)
  data ServerParams = ServerParams { directory        :: FilePath
                                   , domain           :: String
                                   , port             :: String
                                   , forward          :: String
                                   , email            :: String
                                   , ssl              :: Bool
                                   , directoryListing :: Bool
                                   , serverType       :: ServerType
                                   } deriving (Eq)

  instance Default ServerParams where
    def = ServerParams { directory        = "/var/www/html"
                       , domain           = "localhost"
                       , port             = "80"
                       , forward          = ""
                       , email            = ""
                       , ssl              = False
                       , directoryListing = False
                       , serverType       = Static }

  instance Show ServerParams where
    show ServerParams { directory, domain, port, forward, email, ssl, serverType, directoryListing } =
      let redirect
            | ssl = block "server" $
                      semicolon $
                          keyvalue [ ("listen", "80")
                                   , ("listen", "[::]:80")
                                   , ("server_name", domain)
                                   , ("rewrite", "^ https://$server_name$request_uri? permanent")
                                   ] " "
            | otherwise = []
          https
            | ssl = [ ("ssl_certificate", "/etc/letsencrypt/live/" ++ domain ++ "/fullchain.pem")
                    , ("ssl_certificate_key", "/etc/letsencrypt/live/" ++ domain ++ "/privkey.pem")
                    , ("include", "ssl.conf")]
            | otherwise = []

          listen = if ssl then "443 ssl" else port

          base = [ ("server_name", domain)
                  , ("listen", listen)
                  , ("listen", "[::]:" ++ listen)
                  , ("index", "index.html index.html index.php")
                  , ("autoindex", if directoryListing then "on" else "off")
                  ] ++ https
      in 
        case serverType of
          Static -> 
            block "server" 
              (semicolon $
                keyvalue (base ++ [("root", directory)]) " ")
            ++ "\n" ++ redirect

          PortForwarding -> 
            let proxyBlock = block "location /" $
                                semicolon $
                                  keyvalue [ ("proxy_pass", "http://127.0.0.1:" ++ forward)
                                           , ("proxy_set_header", "X-Forwarded-Host $host")
                                           , ("proxy_set_header", "X-Forwarded-Server $host")
                                           , ("proxy_set_header", "X-Forwarded-For $proxy_add_x_forwarded_for")
                                           ] " "
            in block "server" $
                 semicolon (keyvalue base " ")
                 ++ proxyBlock ++ "\n" ++ redirect
