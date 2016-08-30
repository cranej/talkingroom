module Lib
    ( markdownToHtml
    , slugify
    ) where

import Text.Pandoc
import Text.Pandoc.Error (handleError)
import Text.Blaze.Html (Html)
import Data.Set (Set)
import Data.Char (isSpace, isLetter)
import qualified Data.Set as Set

markdownToHtml :: String -> Html
markdownToHtml =
  writeHtml def {writerHtml5 = True, writerHighlight = True} .
  handleError .
  readMarkdown def {readerExtensions = markdownExtensions}

markdownExtensions :: Set Extension
markdownExtensions =
  Set.union githubMarkdownExtensions $ Set.fromList [Ext_yaml_metadata_block, Ext_definition_lists]

slugify :: String -> String
slugify s = foldr f "" s
  where
    f c acc
      | isSpace c = '-':acc
      | isLetter c = c:acc
      | otherwise = acc

