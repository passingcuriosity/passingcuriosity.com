{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative (empty, (<$>))
import           Control.Monad (liftM, zipWithM_)
import           Data.List (intersperse, intercalate, sortBy)
import qualified Data.Map as M
import           Data.Monoid (mappend, mconcat, mempty)
import           System.FilePath
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A

-- For pagination.
import           Data.Time
import           Data.Time.Clock
import           Data.Time.Format
import           System.Locale (defaultTimeLocale)

import           Hakyll hiding (defaultContext)
import           Text.Pandoc.Options

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Site configuration
hakyllConf :: Configuration
hakyllConf = defaultConfiguration
  { deployCommand = "rsync -ave 'ssh' _site/ passingcuriosity.com:/var/www/passingcuriosity.com/htdocs"
  }


-- | Number of posts to list on a page.
pageSize :: Int
pageSize = 10

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
    , feedRoot        = "http://passingcuriosity.com/"
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

    -- Compile templates
    match "templates/*" $ compile templateCompiler

    -- Compile standard pages
    match (fromList ["contact.md", "about.md"]) $ do
      route   $ routeFileToDirectory
      compile $ do
        pandocCompiler
            >>= saveSnapshot "content"
            >>= return . fmap demoteHeaders
            >>= loadAndApplyTemplate "templates/page.html" defaultContext
            >>= saveSnapshot "post"
            >>= loadAndApplyTemplate "templates/_default.html" defaultContext
            >>= relativizeUrls

    -- Build tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")
    let postCtx = postContext tags
    let feedCtx = feedContext tags
    let tagCtx = tagContext tags

    -- Posts
    --
    -- @todo Store a "teaser" snapshot of post content.
    match "posts/*" $ do
        route   $ routePosts
        compile $ do
          postCompiler 
            >>= saveSnapshot "content"
            >>= return . fmap demoteHeaders
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= saveSnapshot "post"
            >>= loadAndApplyTemplate "templates/_default.html" postCtx
            >>= relativizeUrls

    -- Generate tag indexes, with RSS and Atom feeds.
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag

        -- RSS feed
        version "rss" $ do
            route   $ routeTags `composeRoutes` setExtension "rss"
            compile $ loadAllSnapshots pattern "content"
                >>= fmap (take 10) . recentFirst
                >>= renderRss (feedConf) feedCtx

        -- Atom feed
        version "atom" $ do
            route   $ routeTags `composeRoutes` setExtension "xml"
            compile $ loadAllSnapshots pattern "content"
                >>= fmap (take 10) . recentFirst
                >>= renderAtom (feedConf) feedCtx

        -- Plain HTML version
        route routeTags
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let number = length posts
            let ctx = constField "title" title `mappend`
                      constField "tag" tag `mappend`
                      constField "number" (show number) `mappend`
                      listField "posts" postCtx (return posts) `mappend`
                      field "atom" (fmap (maybe empty toUrl) . getRoute . (setVersion $ Just "atom") . itemIdentifier) `mappend`
                      field "rss" (fmap (maybe empty toUrl) . getRoute . (setVersion $ Just "rss") . itemIdentifier) `mappend`
                      tagCtx tag

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/_default.html" ctx
                >>= relativizeUrls

    -- Build a tag index page.
    match "tag.md" $ do
      route $ routeFileToDirectory
      compile $ do
        let ctx = tagCtx ""

        getResourceBody
          >>= applyAsTemplate ctx
          >>= return . renderPandoc
          >>= loadAndApplyTemplate "templates/tags.html" ctx
          >>= loadAndApplyTemplate "templates/_default.html" ctx
          >>= relativizeUrls

{-
    -- Generate archives.
    match "archives.md" $ do
      -- RSS feed
      version "rss" $ do
        route   $ routeFileToDirectory `composeRoutes` setExtension "rss"
        compile $ loadAllSnapshots "posts/*" "content"
          >>= fmap (take 10) . recentFirst
          >>= renderRss (feedConf) feedCtx

      -- Atom feed
      version "atom" $ do
        route   $ routeFileToDirectory `composeRoutes` setExtension "xml"
        compile $ loadAllSnapshots "posts/*" "content"
          >>= fmap (take 10) . recentFirst
          >>= renderAtom (feedConf) feedCtx

      route $ routeFileToDirectory
      compile $ do
        posts <- fmap (take 10) . recentFirst =<< loadAll "posts/*"
        let indexCtx =
              listField "posts" postCtx (return posts) `mappend`
              defaultContext

        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= return . renderPandoc
          >>= loadAndApplyTemplate "templates/index.html" defaultContext
          >>= loadAndApplyTemplate "templates/_default.html" indexCtx
          >>= relativizeUrls
-}

    -- Paginated archives.
    paginate pageSize $ \index maxIndex postIds -> do
        let id = fromFilePath $ if index == 1
                                then "archives/index.html"
                                else "archives/" ++ (show index) ++ "/index.html"
        create [id] $ do
            route idRoute
            compile $ do
              posts <- mapM load postIds
              let archiveCtx =
                      listField "posts" postCtx (return posts) `mappend`
                      paginationField (\i -> if (i == 1) then "/archives/" else "/archives/" ++ (show i) ++ "/") index maxIndex `mappend`
                      sectionField "archive" `mappend`
                      constField "title" ("Archives " ++ (show index))            `mappend`
                      field "navlinkolder" (\_ -> return $ indexNavLink index 1 maxIndex) `mappend`
                      field "navlinknewer" (\_ -> return $ indexNavLink index (-1) maxIndex) `mappend`
                      defaultContext

              makeItem ""
                  >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                  >>= loadAndApplyTemplate "templates/_default.html" archiveCtx
                  >>= relativizeUrls


    -- Generate index.
    match "index.md" $ do
      route $ setExtension "html"
      compile $ do
        posts <- fmap (take pageSize) . recentFirst =<< loadAll "posts/*"
        let indexCtx =
              listField "posts" postCtx (return posts) `mappend`
              sectionField "home" `mappend`
              defaultContext

        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= return . renderPandoc
          >>= loadAndApplyTemplate "templates/index.html" indexCtx
          >>= loadAndApplyTemplate "templates/_default.html" indexCtx
          >>= relativizeUrls

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
postContext :: Tags -> Context String
postContext tags = mconcat
    [ modificationTimeField "mtime" "%U"
    , dateField "date" "%B %e, %Y"
    , dateField "datetime" "%Y-%m-%d"
    , tagsField' "tags" tags
    , sectionField "archive"
    , defaultContext
    ]

-- | Build a tag template context.
tagContext :: Tags -> String  -> Context String
tagContext tags tag =
  tagCloudField' "tagcloud" 75.0 300.0 tags `mappend`
  sectionField "tag" `mappend`
  defaultContext

-- | Build a feed template context.
--
-- XXXTODO: Add categories, etc.
feedContext :: Tags -> Context String
feedContext _ = mconcat
    [ bodyField "description"
    , defaultContext
    ]

-- | Build a page template context.
pageCtx :: Context String
pageCtx = defaultContext

-- | Default context to use in the site.
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

-- | Make a tag cloud.
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

-- | Context to de-/activate menu entries.
sectionField :: String -> Context a
sectionField s = constField "section" s `mappend`
                 constField ("section_" ++ s) s

--------------------------------------------------------------------------------
-- Compilers
--------------------------------------------------------------------------------
postCompiler = pandocCompiler

-- | Compile a post to its table of contents.
tocCompiler :: Compiler (Item String)
tocCompiler = pandocCompilerWith
  readerOptions
  writerOptions
    { writerTableOfContents = True
    , writerTemplate = "$toc$"
    , writerStandalone = True
    }


--------------------------------------------------------------------------------
-- Pagination
--------------------------------------------------------------------------------

-- | Paginate site posts.
paginate :: Int -- ^ Items per page.
         -> (Int -> Int -> [Identifier] -> Rules ()) -- ^ Process page, page count and list of items.
         -> Rules ()
paginate itemsPerPage rules = do
    identifiers <- getMatches "posts/*"
 
    let sorted = reverse $ sortBy byDate identifiers
        chunks = chunk itemsPerPage sorted
        maxIndex = length chunks
        pageNumbers = take maxIndex [1..]
        process i is = makePatternDependency (fromList is) >>= (\x -> rulesExtraDependencies [x] $ rules i maxIndex is)
    zipWithM_ process pageNumbers chunks
        where
            byDate id1 id2 =
                let fn1 = takeFileName $ toFilePath id1
                    fn2 = takeFileName $ toFilePath id2
                    parseTime' fn = parseTime defaultTimeLocale "%Y-%m-%d" $ intercalate "-" $ take 3 $ splitAll "-" fn
                in compare ((parseTime' fn1) :: Maybe UTCTime) ((parseTime' fn2) :: Maybe UTCTime)

-- Generate a naviagtion link.
indexNavLink :: Int -- ^ Current index.
             -> Int -- ^ Direction.
             -> Int -- ^ Max index.
             -> String
indexNavLink i d m = let n = i + d
                         url = if (n == 1)
                               then "/archives/"
                               else "/archives/" ++ (show n) ++ "/"
                     in if (n == 0 || n == m)
                        then ""
                        else "<a href='" ++ url ++ "'>Page " ++ (show n) ++ "</a>"

data Page = Page { pageTitle :: String
                 , pageUrl :: String
                 , pageClass :: String
                 , pageNumber :: String
                 }

-- | Define context for pagination links.
--
-- * paginationLinkFirst
-- * paginationLinkPrevious
-- * paginationLinkNext
-- * paginationLinkLast
-- * paginationCurrent
-- * paginationMaximum
--
-- Return mzero if (i == m).
paginationField :: (Int -> String) -- ^ URL pattern
                -> Int -- ^ Current index
                -> Int -- ^ Maximum index
                -> Context a
paginationField url i m = if (1 == m)
                          then mempty
                          else let n = i + 1
                                   p = i - 1
                                   next = if (n <= m) then constField "pagination-next-url" (url n) else mempty
                                   prev = if (1 <= p) then constField "pagination-prev-url" (url p) else mempty
                               in constField "pagination-current-page" (show i) `mappend`
                                  constField "pagination-maximum-page" (show m) `mappend`
                                  constField "pagination-first-url" (url 1) `mappend`
                                  constField "pagination-last-url" (url m) `mappend`
                                  listField "pagination-links" pageContext (sequence $ map makeItem $ makePages url i m) `mappend`
                                  prev `mappend` next
  where
    pageContext :: Context Page
    pageContext = field "title" (\item -> return $ pageTitle $ itemBody item) `mappend`
                  field "url" (\item -> return $ pageUrl $ itemBody item) `mappend`
                  field "class" (\item -> return $ pageClass $ itemBody item) `mappend`
                  field "number" (\item -> return $ pageNumber $ itemBody item)

    makePages :: (Int -> String) -> Int -> Int -> [Page]
    makePages url i m = let s = max (i - (number `div` 2)) 1
                            p = [1..m]
                        in map (makePage i) p
      where 
        number = 5
        makePage i p = Page ("Page " ++ show p) (url p) (if (i == p) then "active" else "") (show p)

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Split a list into chunks.
chunk :: Int -- ^ Max items per chunk
      -> [a] -- ^ Items
      -> [[a]]
chunk _ [] = []
chunk i xs = let (hs,xs') = splitAt i xs
             in hs:(chunk i xs')
