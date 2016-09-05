{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Backend
import           Control.Applicative             ((<$>))
import           Control.Monad                   (unless, when)
import           Control.Monad.IO.Class          (liftIO)
import           Crypto.Hash
import qualified Data.ByteString                 as B
import           Data.ByteString.Base64          (decodeLenient)
import           Data.ByteString.Char8           (pack, unpack,split)
import           Data.SecureMem
import           Data.Text.Lazy                  (Text)
import           Lib
import           Network.HTTP.Types.Status       (unauthorized401)
import           Network.Wai                     (Request, requestHeaders)
import           Text.Blaze.Html.Renderer.Text
import           Web.Scotty

renderMarkdownFile :: FilePath -> IO Text
renderMarkdownFile path = (renderHtml . markdownToHtml .unpack) <$> B.readFile path

getPostFilePath :: String -> FilePath
getPostFilePath s = "testdata/" ++ s ++ ".md"

password :: SecureMem
password = secureMemFromByteString "54de7f606f2523cba8efac173fab42fb7f59d56ceff974c8fdb7342cf2cfe345"

hashPassword :: B.ByteString -> B.ByteString
hashPassword s = pack . show $ hs
  where hs = (hash s) :: Digest SHA256

auth :: (Request -> IO Bool) -> ActionM () -> ActionM ()

auth doAuth action = do
    r <- request
    authenticated <- liftIO $ doAuth r
    unless authenticated $ do
        status unauthorized401
        setHeader "WWW-Authenticate" "Basic realm=\"Authentication Required\""
        html "<h1>Authentication required.</h1>"
    when authenticated action

doAuth :: Request -> IO Bool
doAuth r = do
    case [v | (k, v) <- requestHeaders r, k == "Authorization" ] of
        [value] -> return $ check (decode value)
        _       -> return False

    where
        decode = split ':' . decodeLenient . head . drop 1 . split ' '
        check ["admin", pwd] = (secureMemFromByteString . hashPassword $ pwd) == password
        check _ = False

main :: IO ()
main = scotty 3000 $ do
    get "/:post" $ do
        fname <- getPostFilePath <$> param "post" `rescue` (\err -> return "index")
        c <- liftIO . renderMarkdownFile $ fname
        html c

    get "/admin/secret" $ auth doAuth $ do
        text "You must be authenticated!"
