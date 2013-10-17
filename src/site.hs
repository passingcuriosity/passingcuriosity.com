{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative (empty, (<$>))
import           Control.Monad (liftM)
import           Data.List (intersperse, intercalate)
import qualified Data.Map as M
import           Data.Monoid (mappend, mconcat)
import           System.FilePath
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A

import           Hakyll hiding (defaultContext)
import           Text.Pandoc.Options


--------------------------------------------------------------------------------

-- | Site configuration
hakyllConf :: Configuration
hakyllConf = defaultConfiguration
  { deployCommand = "rsync -ave 'ssh' _site/ passingcuriosity.com:/var/www/passingcuriosity.com/htdocs"
  }

-- | Pandoc reader options.
readerOptions :: ReaderOptions
readerOptions = defaultHakyllReaderOptions

-- | Pandoc write options.
writerOptions :: WriterOptions
writerOptions = defaultHakyllWriterOptions
    { writerHTMLMathMethod = MathJax ""
    }

-- | Feed configuration.
feedConf :: FeedConfiguration
feedConf = FeedConfiguration
    { feedTitle       = "Passing Curiosity"
    , feedDescription = ""
    , feedAuthorName  = "Thomas Sutton"
    , feedAuthorEmail = "me@thomas-sutton.id.au"
    , feedRoot        = "http://passingcuriosity.com"
    }

--------------------------------------------------------------------------------

main :: IO ()
main = hakyllWith hakyllConf $ do
    -- Copy static resources
    match ("img/*" .||. "favicon.ico" .||. "files/**") $ do
        route   idRoute
        compile copyFileCompiler

    -- Compress CSS
    match "css/**.css" $ do
        route   idRoute
        compile compressCssCompiler

    -- Compress JS
    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- Compile standard pages
    match (fromList ["contact.md", "about.md"]) $ do
      route   $ routeFileToDirectory
      compile $ do
        pandocCompiler
            >>= saveSnapshot "content"
            >>= return . fmap demoteHeaders
            >>= loadAndApplyTemplate "templates/page.html" defaultContext
            >>= saveSnapshot "post"
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- Build tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    match "references.bib" $ do
      route $ idRoute
      compile $ biblioCompiler

    match "references.csl" $ do
      route $ idRoute
      compile $ cslCompiler

    -- Posts
    match "posts/*" $ do
        route   $ routePosts
        compile $ do

          postCompiler 
            >>= saveSnapshot "content"
            >>= return . fmap demoteHeaders
            >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
            >>= saveSnapshot "post"
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- Generate archives.
    match "archives.md" $ do
      -- RSS feed
      version "rss" $ do
        route   $ routeFileToDirectory `composeRoutes` setExtension "rss"
        compile $ loadAllSnapshots "posts/*" "content"
          >>= fmap (take 10) . recentFirst
          >>= renderRss (feedConf) (feedCtx tags)

      -- Atom feed
      version "atom" $ do
        route   $ routeFileToDirectory `composeRoutes` setExtension "xml"
        compile $ loadAllSnapshots "posts/*" "content"
          >>= fmap (take 10) . recentFirst
          >>= renderAtom (feedConf) (feedCtx tags)

      route $ routeFileToDirectory
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let indexCtx =
              listField "posts" (postCtx tags) (return posts) `mappend`
              field "atom" (fmap (maybe empty toUrl) . getRoute . (setVersion $ Just "atom") . itemIdentifier) `mappend`
              field "rss" (fmap (maybe empty toUrl) . getRoute . (setVersion $ Just "rss") . itemIdentifier) `mappend`
              defaultContext

        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= return . renderPandoc
          >>= loadAndApplyTemplate "templates/index.html" defaultContext
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

    -- Generate tag indexes, with RSS and Atom feeds.
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag

        -- RSS feed
        version "rss" $ do
            route   $ routeTags `composeRoutes` setExtension "rss"
            compile $ loadAllSnapshots pattern "content"
                >>= fmap (take 10) . recentFirst
                >>= renderRss (feedConf) (feedCtx tags)

        -- Atom feed
        version "atom" $ do
            route   $ routeTags `composeRoutes` setExtension "xml"
            compile $ loadAllSnapshots pattern "content"
                >>= fmap (take 10) . recentFirst
                >>= renderAtom (feedConf) (feedCtx tags)

        -- Plain HTML version
        route routeTags
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let number = length posts
            let ctx = constField "title" title `mappend`
                      constField "tag" tag `mappend`
                      constField "number" (show number) `mappend`
                      listField "posts" (postCtx tags) (return posts) `mappend`
                      field "atom" (fmap (maybe empty toUrl) . getRoute . (setVersion $ Just "atom") . itemIdentifier) `mappend`
                      field "rss" (fmap (maybe empty toUrl) . getRoute . (setVersion $ Just "rss") . itemIdentifier) `mappend`
                      tagCtx tag tags

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    -- Paginated archives.
    pages <- buildPaginateWith 20 (\i-> fromFilePath $ ("archives/" ++ show i ++ ".html")) "posts/*"
    paginateRules pages $ \n pattern -> do
        route $ routeFileToDirectory
        compile $ do
          posts <- recentFirst =<< loadAll pattern
          let indexCtx =
                constField "page" (show n) `mappend`
                constField "page" (show n) `mappend`
                listField "posts" (postCtx tags) (return posts) `mappend`
                paginateContext pages `mappend`
                defaultContext

          makeItem ""
            >>= loadAndApplyTemplate "templates/archive.html" indexCtx
            >>= loadAndApplyTemplate "templates/default.html" indexCtx
            >>= relativizeUrls

    match "tag.md" $ do
      route $ routeFileToDirectory
      compile $ do
        let ctx = tagCtx "" tags

        getResourceBody
          >>= applyAsTemplate ctx
          >>= return . renderPandoc
          >>= loadAndApplyTemplate "templates/tags.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls
        
    match "index.md" $ do
      route $ setExtension "html"
      compile $ do
        posts <- fmap (take 10) . recentFirst =<< loadAll "posts/*"
        let indexCtx =
              listField "posts" (postCtx tags) (return posts) `mappend`
              defaultContext

        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= return . renderPandoc
          >>= loadAndApplyTemplate "templates/index.html" defaultContext
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

    -- Compile templates
    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------

-- | Route files to directory indexes.
routeFileToDirectory :: Routes
routeFileToDirectory = customRoute fileToDirectory
  where fileToDirectory :: Identifier -> FilePath
        fileToDirectory ident = let p = toFilePath ident
                                    (dir,fn) = splitFileName p
                                    bn = dropExtension fn
                                in joinPath [dir, bn, "index.html"]

-- | Route dated posts.
routePosts :: Routes
routePosts = customRoute fileToDirectory
  where fileToDirectory :: Identifier -> FilePath
        fileToDirectory ident = let p = toFilePath ident
                                    fn = takeFileName p
                                    bn = drop 11 $ dropExtension fn
                                    y = take 4 fn
                                in joinPath [y, bn, "index.html"]

-- | Route tag pages.
routeTags :: Routes
routeTags = customRoute tagPath
    where tagPath ident = let p = toFilePath ident
                              fn = takeFileName p
                              t = dropExtension fn
                          in joinPath ["tag", t, "index.html"]

-- | Absolute url to the resulting item
strippedUrlField :: String -> Context a
strippedUrlField key = field key $
    fmap (maybe empty strippedUrl) . getRoute . itemIdentifier
    where strippedUrl = dropFileName . toUrl

--------------------------------------------------------------------------------

-- | Build a post template context.
postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ modificationTimeField "mtime" "%U"
    , dateField "date" "%B %e, %Y"
    , dateField "datetime" "%Y-%m-%d"
    , tagsField' "tags" tags
    , defaultContext
    ]

-- | Build a page template context.
pageCtx :: Context String
pageCtx = defaultContext

-- | Build a tag template context.
tagCtx :: String -> Tags -> Context String
tagCtx tag tags =
  tagCloudField' "tagcloud" 75.0 300.0 tags `mappend`
  defaultContext

tagCloudField' key minSize maxSize tags =
  tagCloudFieldWith key makeLink cat minSize maxSize tags
  where
    cat = (intercalate " ")
    makeLink minSize maxSize tag url count min' max' =
      -- Show the relative size of one 'count' in percent
      let diff     = 1 + fromIntegral max' - fromIntegral min'
          relative = (fromIntegral count - fromIntegral min') / diff
          size     = floor $ minSize + relative * (maxSize - minSize) :: Int
      in renderHtml $
         H.a ! A.style (toValue $ "font-size: " ++ show size ++ "%")
             ! A.href (toValue $ (++ "/") $ joinPath $ init $ splitDirectories url)
             $ toHtml tag

-- | Build a feed template context.
--
-- XXXTODO: Add categories, etc.
feedCtx :: Tags -> Context String
feedCtx _ = mconcat
    [ bodyField "description"
    , defaultContext
    ]

defaultContext :: Context String
defaultContext =
    tocField      "contents" `mappend`
    bodyField     "body"     `mappend`
    metadataField            `mappend`
    strippedUrlField "url"   `mappend`
    pathField     "path"     `mappend`
    titleField    "title"    `mappend`
    constField    "author" (feedAuthorName feedConf) `mappend`
    missingField

--------------------------------------------------------------------------------
-- Fields

-- | If the "toc" field is defined in the context, replace it with the table
-- of contents markup generated by Pandoc.
tocField :: String -> Context String
tocField name = field name $ \item -> do
  value <- getMetadataField (itemIdentifier item) name
  case value of
    Nothing -> empty
    Just v -> if null v
              then empty
              else tocCompiler >>= return . itemBody

-- | Custom "tags" context to process tag URLs.
tagsField' :: String -> Tags -> Context a
tagsField' = tagsFieldWith
             getTags
             simpleRenderLink
             (mconcat . intersperse ", ")
  where
    simpleRenderLink :: String -> (Maybe FilePath) -> Maybe H.Html
    simpleRenderLink _   Nothing         = Nothing
    simpleRenderLink tag (Just filePath) =
      Just $ H.a ! A.href (toValue $ dropFileName $ toUrl filePath) $ toHtml tag

--------------------------------------------------------------------------------
-- Compilers
--------------------------------------------------------------------------------
postCompiler = bibtexCompiler "references.csl" "references.bib"

bibtexCompiler sid bid = do 
  csl <- load sid
  bib <- load bid
  getResourceBody 
    >>= readPandocBiblio readerOptions (Just csl) bib
    >>= return . (writePandocWith writerOptions)

-- | Compile a post to its table of contents.
tocCompiler :: Compiler (Item String)
tocCompiler = pandocCompilerWith
  readerOptions
  writerOptions
    { writerTableOfContents = True
    , writerTemplate = "$toc$"
    , writerStandalone = True
    }
