{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad

import Data.Typeable
import Data.Binary

import Data.Monoid
import Data.Maybe

import qualified Data.Map as M (Map, lookup)

import Hakyll

import Image (resizeImageCompiler, PNG(..), JPG(..))


feedConfiguration :: FeedConfiguration
feedConfiguration =  FeedConfiguration
    { feedTitle       = "Programming the smart way."
    , feedDescription = "I explore program design, large scale applications, and solid design especially with FP."
    , feedAuthorName  = "Eric Stolten"
    , feedAuthorEmail = "eric@stolten.net"
    , feedRoot        = "http://eric.stolten.net"
    }


config :: Configuration
config = defaultConfiguration
    { deployCommand = "rsync --checksum -av \
                      \_site/* stolten.net:/var/www/eric.stolten.net"
    }


-- make compile item with * -> Compile (Item ByteString)
main :: IO ()
main = hakyllWith config $ do

--------------------------------------------------------------------------------
-- Assets
    match "images/*.jpg" $ do
        route   idRoute
        compile (resizeImageCompiler JPG 900)

    match "images/*.png" $ do
        route idRoute
        compile (resizeImageCompiler PNG 900)

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
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/post.html"    postCtx
          >>= loadAndApplyTemplate "templates/default.html" postCtx
          >>= relativizeUrls

--------------------------------------------------------------------------------
-- Blog directories
{-
Here is the general goal of blog directories

For <article-name>/article.markdown

1. Create posts/<article-name>/index.html
2. Copy css directly
3. Copy js directly
4. Copy images directly

-}

    matchMetadata "posts/*/index.markdown" published $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

        match "posts/*/images/*.jpg" $ do
            route idRoute
            compile (resizeImageCompiler JPG 900)

        match "posts/*/images/*.png" $ do
            route idRoute
            compile (resizeImageCompiler PNG 900)


        match "posts/*/js/*" $ do
            route idRoute
            compile copyFileCompiler

        match "posts/*/css/*" $ do
            route idRoute
            compile compressCssCompiler

--------------------------------------------------------------------------------
-- Archive page

    create ["archive"] $ do
        route (setExtension ".html")
        compile $ do
            posts <- recentFirst =<< loadAllPublished ("posts/**/*.markdown" .||. "posts/*.markdown")
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    constField "blogLink" "active"           `mappend`
                    constField "homeLink" ""                 `mappend`
                    constField "aboutLink" ""                `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

--------------------------------------------------------------------------------
-- Index page
    match "pages/index.html" $ do
        route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension ".html"
        compile $ do
            let homeCtx =
                  constField "title" "Home"                `mappend`
                  constField "homeLink" "active"           `mappend`
                  constField "blogLink" ""                 `mappend`
                  constField "aboutLink" ""                `mappend`
                  defaultContext

            getResourceBody
                >>= applyAsTemplate homeCtx
                >>= loadAndApplyTemplate "templates/default.html" homeCtx
                >>= relativizeUrls


    match "pages/about.md" $ do
        route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension ".html"
        compile $ do
            let homeCtx =
                  constField "title" "About"                `mappend`
                  constField "homeLink" ""                  `mappend`
                  constField "blogLink" ""                  `mappend`
                  constField "aboutLink" "active"           `mappend`
                  defaultContext

            pandocCompiler
                >>= applyAsTemplate homeCtx
                >>= loadAndApplyTemplate "templates/default.html" homeCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
-- XML Feed generation

    create ["atom.xml"] (feedRule renderAtom)
    create ["rss.xml"] (feedRule renderRss)


------------------------------------------------------------------------------

routeToRoot :: Rules ()
routeToRoot = route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension ".html"

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y"   `mappend`
    constField "blogLink" "active" `mappend`
    constField "homeLink" ""       `mappend`
    constField "aboutLink" ""      `mappend`
    defaultContext

--------------------------------------------------------------------------------

loadAllPublished :: (Typeable a, Binary a) => Pattern -> Compiler [Item a]
loadAllPublished p = filterM isPublished =<< loadAll p


isPublished :: (Typeable a, Binary a) => Item a -> Compiler Bool
isPublished ident = do
  val <- fromMaybe "false" <$> getMetadataField (itemIdentifier ident) "published"
  return (val == "true")

published :: M.Map String String -> Bool
published md = Just "true" == pub
  where pub = "published" `M.lookup` md


loadAllPublishedSnapshots :: (Typeable a, Binary a) => Pattern -> Compiler [Item a]
loadAllPublishedSnapshots p = filterM isPublished =<< loadAllSnapshots p "content"


feedRule :: (Writable a, Binary a1, Binary a, Typeable a1, Typeable a)
            => (FeedConfiguration -> Context String -> [Item a1] -> Compiler (Item a))
            -> Rules ()

feedRule f = do
    route idRoute
    compile $ do
        let feedCtx = postCtx `mappend` teaserField "description" "content" `mappend` bodyField "description"
        posts <- fmap (take 10) . recentFirst =<< loadAllPublishedSnapshots "posts/*"
        f feedConfiguration feedCtx posts
