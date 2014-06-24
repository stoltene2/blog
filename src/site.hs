--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Control.Applicative
import Control.Monad
import           Data.Map (member)
import Data.Typeable
import Data.Binary
import Data.Monoid
import Data.Maybe

import System.FilePath.Posix  (takeBaseName,takeDirectory,(</>),splitFileName)


feedConfiguration :: FeedConfiguration
feedConfiguration =  FeedConfiguration
    { feedTitle       = "Programming, Design, Architecture and Fun"
    , feedDescription = "I explore program design, large scale applications, and solid design especially with FP."
    , feedAuthorName  = "Eric Stolten"
    , feedAuthorEmail = "eric@stolten.net"
    , feedRoot        = "http://eric.stolten.net"
    }

main :: IO ()
main = hakyll $ do

--------------------------------------------------------------------------------
-- Assets
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

--------------------------------------------------------------------------------
-- Individual blog posts
    match "posts/*" $ do
        route $ setExtension ".html"
        compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/post.html"    postCtx
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/default.html" postCtx
          >>= relativizeUrls
   

--------------------------------------------------------------------------------
-- Archive page
        
    create ["archive"] $ do
        route (setExtension ".html")
        compile $ do
            posts <- recentFirst =<< loadAllPublished "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    constField "blogLink" "active"           `mappend`
                    constField "homeLink" ""                 `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

--------------------------------------------------------------------------------
-- Index page            
    match "pages/home.html" $ do
        route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension ".html"
        compile $ do
            let homeCtx =
                  constField "title" "Home"                `mappend`
                  constField "homeLink" "active"           `mappend`
                  constField "blogLink" ""                 `mappend`
                  defaultContext

            getResourceBody
                >>= applyAsTemplate homeCtx
                >>= loadAndApplyTemplate "templates/default.html" homeCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
-- XML Feed generation

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"

            posts <- fmap (take 10) . recentFirst =<< loadAllPublishedSnapshots "posts/*"

            renderAtom feedConfiguration feedCtx posts



------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"   `mappend`
  constField "blogLink" "active" `mappend`
  constField "homeLink" ""       `mappend`
  defaultContext

--------------------------------------------------------------------------------

loadAllPublished :: (Typeable a, Binary a) => Pattern -> Compiler [Item a]
loadAllPublished p = filterM published =<< loadAll p
  where
    published i = isJust <$> getMetadataField (itemIdentifier i) "published"


loadAllPublishedSnapshots :: (Typeable a, Binary a) => Pattern -> Compiler [Item a]
loadAllPublishedSnapshots p = filterM published =<< loadAllSnapshots p "content"
  where
    published i = isJust <$> getMetadataField (itemIdentifier i) "published"