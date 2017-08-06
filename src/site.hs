{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid ((<>))
import Data.Typeable
import Data.Binary

import Hakyll

import Image.Resize (resizeImageCompiler, PNG(..), JPG(..))

--------------------------------------------------------------------------------
-- * Add robots.txt and exclude drafts
-- * Add teaser field to all pages and load snapshots on main page
-- * Add html5 tags
-- * Read configuration data from yaml files

-- * Move blog to S3 bucket
-- * Generate sitemap page
-- * Get SSL certificate?
-- * Add twitter style meta data tags to each page and add to plop
-- * Add bulma for styling
-- * Profile the page for inefficiencies


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


main :: IO ()
main = hakyllWith config $ do
  match "robots.txt" $ do
    route   idRoute
    compile copyFileCompiler

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

  pages "posts"
  pages "drafts"

  -- Archive page
  create ["archive"] $ do
    route (setExtension ".html")
    compile $ do
      posts <- recentFirst =<< loadAll  "posts/**/*.markdown"
      let archiveCtx = listField "posts" postCtx (return posts) `mappend` defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  match "pages/index.html" $ do
    routePagesToRoot
    compile $ getResourceBody
      >>= applyAsTemplate defaultContext
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  match "pages/about.md" $ do
    routePagesToRoot
    compile $ pandocCompiler
      >>= applyAsTemplate defaultContext
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  match "templates/*" $ compile templateCompiler

  -- Feed generation
  create ["atom.xml"] (feedRule renderAtom)
  create ["rss.xml"] (feedRule renderRss)


------------------------------------------------------------------------------
routePagesToRoot :: Rules ()
routePagesToRoot = route $
  gsubRoute "pages/" (const "") `composeRoutes` setExtension ".html"


postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext


--------------------------------------------------------------------------------
-- Abstract way to generate a post from a generic root directory

pages :: String -> Rules()
pages base =
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

feedRule :: (Writable a, Binary a1, Binary a, Typeable a1, Typeable a, Show a, Show a1)
            => (FeedConfiguration -> Context String -> [Item a1] -> Compiler (Item a))
            -> Rules ()

feedRule f = do
  route idRoute
  compile $ do
    let feedCtx = postCtx `mappend` teaserField "description" "content" `mappend` bodyField "description"
    posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots  "posts/*" "content"
    f feedConfiguration feedCtx posts
