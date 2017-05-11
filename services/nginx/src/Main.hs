{-# LANGUAGE NamedFieldPuns #-}
module Main (call, main) where
  import System.Serverman.Types
  import System.Serverman.Utils
  import System.Serverman.Log
  import Types

  import System.Directory
  import System.IO
  import System.IO.Error
  import System.FilePath
  import System.Process
  import Control.Concurrent.Async
  import Control.Concurrent
  import Control.Monad
  import Control.Monad.State hiding (liftIO)
  import Control.Monad.Free
  import Data.List
  import System.Posix (setOwnerAndGroup, getFileStatus, fileOwner)

  main :: IO ()
  main = return ()

  help :: App String
  help = return $
          mkHelp "nginx [--options]"
                  [ ("--directory <path>", "(static) directory to serve, default: /var/www/html")
                  , ("--domain <domain>", "domain name to listen on, default: localhost")
                  , ("--port <num>", "port number to listen on, default: 80")
                  , ("--forward <num>", "(forward) port number to forward to")
                  , ("--email <email>", "(ssl) email to register SSL certificate on")
                  , ("--ssl", "(ssl) generate an SSL certificate using letsencrypt")
                  , ("--directory-listing", "(static) enable directory indexing")]

  call :: Service -> App ()
  call _ = 
    do
      AppState { arguments } <- get
      let params@ServerParams { ssl, domain, port, directory, serverType, email } = toServerParams arguments

      done <- progressText "setting up nginx configuration"

      verbose $ show params

      -- Turn SSL off at first, because we have not yet received a certificate
      let content = if ssl then show (params { ssl = False, port = "80" }) else show params
          config = "/etc/nginx/"
          mainConfig = "/etc/nginx/nginx.conf"
          parent = config </> "serverman-configs"
          path = parent </> domain
          targetDir = directory
          sampleFile = targetDir </> "serverman.txt"

          createCert path cmd = do
            verbose $ "creating certificate in " ++ path ++ " using command " ++ cmd
            result <- executeRoot cmd ["certonly", "--webroot", "--webroot-path", directory, "-d", domain, "--email", email, "--agree-tos", "-n"] "" False
            case result of
              Left _ -> when (cmd == "letsencrypt") $ createCert path "certbot"
              Right stdout -> do
                write stdout

                unless ("error" `isInfixOf` stdout) $ do
                  verbose $ "writing params to " ++ path
                  liftIO $ writeFile path (show params)
                  restart
                  return ()

      verbose $ "creating directories " ++ targetDir ++ ", " ++ parent

      liftIO $ do
        createDirectoryIfMissing True targetDir
        createDirectoryIfMissing True parent

      verbose $ "adding include statement to " ++ mainConfig ++ " pointing to " ++ parent
      liftIO $ writeIncludeStatementIfMissing mainConfig parent

      when ssl $ do
        let sslPath = config </> "ssl.conf"
        verbose $ "writing SSL configuration to " ++ sslPath

        liftIO $ writeFileIfMissing sslPath nginxSSL

        info $ "wrote ssl configuration to " ++ sslPath

      done
      liftIO $ writeFile path content
      info $ "wrote your configuration file to " ++ path

      liftIO $ writeFile sampleFile "Hello from serverman!"
      info $ "wrote a sample file to " ++ sampleFile ++ ", you should be able to access it through " ++ domain ++ ":" ++ port ++ "/serverman.txt"
        
      restart

      when ssl $ do
        done <- progressText "creating SSL certificate"
        let dhparamPath = "/etc/ssl/certs/dhparam.pem"
        dhExists <- liftIO $ doesFileExist dhparamPath

        unless dhExists $ do
          verbose "creating dhparam using openssl"
        
          dhparam <- executeRoot "openssl" ["dhparam", "-out", dhparamPath, "2048"] "" True
          return ()

        case serverType of
          Static -> do
            letsencrypt <- createCert path "letsencrypt"
            done

            return ()
          _ -> do
            info "you should use letsencrypt to create a certificate for your domain"
            write $ "and put it in /etc/letsencrypt/live/" ++ domain ++ "/fullchain.pem"
            write "my suggestion is running this command:"
            write $ "sudo letsencrypt certonly --webroot --webroot-path <YOUR_APPLICATION_DIRECTORY> -d " ++ domain 

        write "for more information, see: https://certbot.eff.org/"

      return ()
    where
      restart = do
        result <- restartService "nginx"
        case result of
          Left err -> return ()
          Right _ -> info "restarted nginx"

      writeIncludeStatementIfMissing path target = do
        content <- readFile path

        let statement = "include " ++ target ++ "/*;"

        unless (statement `isInfixOf` content) $ do
          let newContent = appendAfter content "http {" (indent statement)

          writeFile path newContent

  nginxSSL = "# from https://cipherli.st/\n\
\# and https://raymii.org/s/tutorials/Strong_SSL_Security_On_nginx.html\n\
\\n\
\ssl_protocols TLSv1 TLSv1.1 TLSv1.2;\n\
\ssl_prefer_server_ciphers on;\n\
\ssl_ciphers 'EECDH+AESGCM:EDH+AESGCM:AES256+EECDH:AES256+EDH';\n\
\ssl_ecdh_curve secp384r1;\n\
\ssl_session_cache shared:SSL:10m;\n\
\ssl_session_tickets off;\n\
\ssl_stapling on;\n\
\ssl_stapling_verify on;\n\
\resolver 8.8.8.8 8.8.4.4 valid=300s;\n\
\resolver_timeout 5s;\n\
\# Disable preloading HSTS for now.  You can use the commented out header line that includes\n\
\# the 'preload' directive if you understand the implications.\n\
\#add_header Strict-Transport-Security 'max-age=63072000; includeSubdomains; preload';\n\
\add_header Strict-Transport-Security 'max-age=63072000; includeSubdomains';\n\
\add_header X-Frame-Options DENY;\n\
\add_header X-Content-Type-Options nosniff;\n\
\\n\
\ssl_dhparam /etc/ssl/certs/dhparam.pem;\n"
