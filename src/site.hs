{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad

import Data.Monoid ((<>))
import Data.Typeable
import Data.Binary

import Data.Maybe

import qualified Data.HashMap.Strict as M (lookup)

import qualified Data.Aeson as A (Value(..))
import Hakyll

import Image.Resize (resizeImageCompiler, PNG(..), JPG(..))


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
    { providerDirectory = "./site"
    , deployCommand = "rsync --checksum -av \
                      \_site/* stolten.net:/var/www/eric.stolten.net"
    }


-- make compile item with * -> Compile (Item ByteString)
main :: IO ()
main = hakyllWith config $ do
    -- Assets
    match "images/*.jpg" $ do
        route   idRoute
        compile (resizeImageCompiler JPG 900)

    match "images/*.png" $ do
        route idRoute
        compile $ resizeImageCompiler PNG 900 >>= withItemBody (unixFilterLBS "pngquant" ["-"])

    match "js/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

--------------------------------------------------------------------------------
-- Individual blog posts
-- Blog directories
--
{-
Here is the general goal of blog directories

For <article-name>/article.markdown

1. Create posts/<article-name>/index.html
2. Copy css directly
3. Copy js directly
4. Copy images directly

-}

    pages "posts"


    pages "drafts"
    -- Next, abstract posts and add drafts directory that are not part of published
--------------------------------------------------------------------------------
-- Archive page

    create ["archive"] $ do
        route (setExtension ".html")
        compile $ do
            posts <- recentFirst =<< loadAllPublished "posts/**/*.markdown"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

--------------------------------------------------------------------------------
-- Index page
    match "pages/index.html" $ do
        route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension ".html"
        compile $ getResourceBody
          >>= applyAsTemplate defaultContext
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls


    match "pages/about.md" $ do
        route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension ".html"
        compile $ pandocCompiler
          >>= applyAsTemplate defaultContext
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
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
postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext


--------------------------------------------------------------------------------
-- Abstract way to generate a post from a generic root directory

pages :: String -> Rules()
pages base = do
  match (relativeGlob "/*/index.markdown") $ do
    route $ setExtension "html"

    compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match (relativeGlob "/*/images/*.jpg") $ do
            route idRoute
            compile (resizeImageCompiler JPG 900)

    match (relativeGlob "/*/images/*.png") $ do
            route idRoute
            compile $ resizeImageCompiler PNG 900 >>= withItemBody (unixFilterLBS "pngquant" ["-"])

    match (relativeGlob "/*/js/*") $ do
            route idRoute
            compile copyFileCompiler

    match (relativeGlob "/*/css/*") $ do
            route idRoute
            compile compressCssCompiler

   where
     relativeGlob glob = fromGlob (base <> glob)

--------------------------------------------------------------------------------

loadAllPublished :: (Typeable a, Binary a, Show a) => Pattern -> Compiler [Item a]
loadAllPublished p = filterM isPublished =<< loadAll p


isPublished :: (Typeable a, Binary a, Show a) => Item a -> Compiler Bool
isPublished ident = do
  val <- fromMaybe "false" <$> getMetadataField (itemIdentifier ident) "published"
  return (val == "true")


published :: Metadata -> Bool
published md = pub
  where pub = case "published" `M.lookup` md of
                Just (A.Bool t) -> t
                _      -> False


loadAllPublishedSnapshots :: (Typeable a, Binary a, Show a) => Pattern -> Compiler [Item a]
loadAllPublishedSnapshots p = filterM isPublished =<< loadAllSnapshots p "content"


feedRule :: (Writable a, Binary a1, Binary a, Typeable a1, Typeable a, Show a, Show a1)
            => (FeedConfiguration -> Context String -> [Item a1] -> Compiler (Item a))
            -> Rules ()

feedRule f = do
    route idRoute
    compile $ do
        let feedCtx = postCtx `mappend` teaserField "description" "content" `mappend` bodyField "description"
        posts <- fmap (take 10) . recentFirst =<< loadAllPublishedSnapshots "posts/*"
        f feedConfiguration feedCtx posts
