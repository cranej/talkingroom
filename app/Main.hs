{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Backend
import           Control.Applicative             ((<$>))
import           Control.Monad                   (unless,when)
import           Control.Monad.IO.Class          (liftIO)
import qualified Data.ByteString                 as B
import           Data.ByteString.Char8           (pack, unpack)
import           Data.SecureMem
import           Data.Text.Lazy                  (Text)
import           Lib
import           Network.HTTP.Types.Status       (unauthorized401)
import           Network.Wai                     (Request)
import           Network.Wai.Middleware.HttpAuth
import           Text.Blaze.Html.Renderer.Text
import           Web.Scotty

renderMarkdownFile :: FilePath -> IO Text
renderMarkdownFile path = (renderHtml . markdownToHtml .unpack) <$> B.readFile path

getPostFilePath :: String -> FilePath
getPostFilePath s = "testdata/" ++ s ++ ".md"

password :: SecureMem
password = secureMemFromByteString "password01!"

auth :: (Request -> IO Bool) -> ActionM () -> ActionM ()
auth doAuth action = do
    r <- request
    authenticated <- liftIO $ doAuth r
    unless authenticated $ do
        status unauthorized401
        setHeader "WWW-Authenticate" "Basic realm=\"Incoming\""
        html "<h1>Authentication required.</h1>"
    when authenticated action

doAuth :: Request -> IO Bool
doAuth r = do
     return False

main :: IO ()
main = scotty 3000 $ do
    get "/:post" $ do
        fname <- getPostFilePath <$> param "post" `rescue` (\err -> return "index")
        c <- liftIO . renderMarkdownFile $ fname
        html c

    -- middleware $ basicAuth (\u p -> return $ u == "user" && secureMemFromByteString p == password) "what is this for?"

    get "/admin/secret" $ auth doAuth $ do
        text "You must be authenticated!"
