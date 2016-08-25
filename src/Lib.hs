module Lib
    ( markdownToHtml
    ) where

import Text.Pandoc
import Text.Pandoc.Error (handleError)
import Text.Blaze.Html (Html)
import Data.Set (Set)
import qualified Data.Set as Set

markdownToHtml :: String -> Html
markdownToHtml =
  writeHtml def {writerHtml5 = True, writerHighlight = True} .
  handleError .
  readMarkdown def {readerExtensions = markdownExtensions}

markdownExtensions :: Set Extension
markdownExtensions =
  Set.union githubMarkdownExtensions $ Set.fromList [Ext_yaml_metadata_block, Ext_definition_lists]

