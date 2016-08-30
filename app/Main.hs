{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative           ((<$>))
import           Control.Monad.IO.Class        (liftIO)
import qualified Data.ByteString               as B
import           Data.ByteString.Char8         (pack, unpack)
import           Data.Text.Lazy                (Text)
import           Lib
import Backend
import           Text.Blaze.Html.Renderer.Text
import           Web.Scotty

renderMarkdownFile :: FilePath -> IO Text
renderMarkdownFile path = (renderHtml . markdownToHtml .unpack) <$> B.readFile path

getPostFilePath :: String -> FilePath
getPostFilePath s = "testdata/" ++ s ++ ".md"

main :: IO ()
main = scotty 3000 $ do
    get "/:post" $ do
        fname <- getPostFilePath <$> param "post" `rescue` (\err -> return "index")
        c <- liftIO . renderMarkdownFile $ fname
        html c
