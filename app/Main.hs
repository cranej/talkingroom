{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative           ((<$>))
import           Control.Monad.IO.Class        (liftIO)
import qualified Data.ByteString               as B
import           Data.ByteString.Char8         (pack, unpack)
import           Data.Text.Lazy                (Text)
import           Lib
import           Text.Blaze.Html.Renderer.Text
import           Web.Scotty

renderMarkdownFile :: FilePath -> IO Text
renderMarkdownFile path = (renderHtml . markdownToHtml .unpack) <$> B.readFile path

main :: IO ()
main = scotty 3000 $ do
    get "/" $ do
        c <- liftIO . renderMarkdownFile $ "readme.md"
        html c
