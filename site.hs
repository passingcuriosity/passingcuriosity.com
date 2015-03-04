{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Hashable
import           Data.List
import           Data.Monoid
import           Hakyll                          hiding (defaultContext)
import           System.FilePath
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as BH
import qualified Text.Blaze.Html5.Attributes     as BA
import           Text.Pandoc

{-# ANN module ("HLint: ignore Use liftM" :: String) #-}

--------------------------------------------------------------------------------
-- * Configuration
--------------------------------------------------------------------------------

twitterUser :: String
twitterUser = "@thsutton"

-- | Site configuration
hakyllCfg :: Configuration
hakyllCfg = defaultConfiguration
  { deployCommand = "rsync -rlpve 'ssh' _site/ thomas@passingcuriosity.com:/srv/passingcuriosity.com/htdocs"
  }

feedCfg :: FeedConfiguration
feedCfg = FeedConfiguration
    { feedTitle = "Passing Curiosity: Latest posts"
    , feedDescription = "Latest posts from Passing Curiosity."
    , feedAuthorName = "Thomas Sutton"
    , feedAuthorEmail = "me@thomas-sutton.id.au"
    , feedRoot = "http://passingcuriosity.com/"
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

main :: IO ()
main = hakyllWith hakyllCfg $ do
    match "templates/*" $
        compile templateCompiler

    match ("files/**" .||. "favicon.*") $ do
        route   idRoute
        compile copyFileCompiler

    match "assets/css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "assets/js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "assets/fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "assets/img/*" $ do
        route   idRoute
        compile copyFileCompiler

    --
    -- Tags
    --
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")
    let postCtx = postContext tags
    let feedCtx = feedContext tags
    let tagCtx = tagContext tags
    let defaultCtx = defaultContext tags

    --
    -- Content
    --

    match "posts/*" $ do
        route routePosts
        compile $ do
            images <- getImages

            let ctx = imageField "image" images <>
                      postCtx

            pandocCompiler
                >>= return . fmap demoteHeaders
                >>= loadAndApplyTemplate "templates/post.html" ctx
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "about.md" $ do
        route routeFileToDirectory
        compile $ do
            images <- getImages

            let ctx = sectionField "about" <>
                      imageField "image" images <>
                      defaultCtx

            pandocCompiler
                >>= loadAndApplyTemplate "templates/page.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "contact.md" $ do
        route routeFileToDirectory
        compile $ do
            images <- getImages

            let ctx = sectionField "contact" <>
                      imageField "image" images <>
                      defaultCtx

            pandocCompiler
                >>= loadAndApplyTemplate "templates/page.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "index.md" $ do
        route   $ setExtension "html"
        compile $ do
            images <- getImages
            posts <- fmap (take 3) . recentFirst =<<
                loadAll ("posts/*" .&&. hasNoVersion)

            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    imageField "image" images <>
                    sectionField "home" <>
                    defaultCtx

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= return . renderPandoc
                >>= loadAndApplyTemplate "templates/page.html"   indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "tags.md" $ do
        route routeFileToDirectory
        compile $ do
            images <- getImages

            let ctx = tagCloudField' "tagcloud" 75.0 300.0 tags <>
                      sectionField "tags" <>
                      imageField "image" images <>
                      defaultCtx

            getResourceBody
                >>= applyAsTemplate ctx
                >>= return . renderPandoc
                >>= loadAndApplyTemplate "templates/page.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    --
    -- Generated
    --

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
            renderAtom feedCfg feedCtx posts

    paginated_archives <- buildPaginateWith
        (sortRecentFirst >=> return . paginateEvery pageSize)
        "posts/*"
        (\n -> fromFilePath $ if n == 1 then "archives.md" else "archives" </> show n <> ".md")

    paginateRules paginated_archives $ \page_number pattern -> do
        route routeFileToDirectory
        compile $ do
            images <- getImages
            posts <- recentFirst =<< loadAll pattern

            let title = if page_number == 1
                    then "Archives"
                    else "Archives: Page " <> show page_number
            let ctx =
                    constField "title" title <>
                    constField "layout" "page" <>
                    sectionField "archive" <>
                    imageField "image" images <>
                    listField "posts" postCtx (return posts) <>
                    paginateContext paginated_archives page_number <>
                    defaultCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    tagsRules tags $ \tag tag_pattern -> do

        let title = "Posts tagged " <> tag

        -- Atom feed
        version "atom" $ do
            route $ routeTags (tag <> ".xml")
            compile $ loadAllSnapshots tag_pattern "content"
                >>= fmap (take 10) . recentFirst
                >>= renderAtom feedCfg feedCtx

        -- Atom feed
        version "rss" $ do
            route $ routeTags (tag <> ".rss")
            compile $ loadAllSnapshots tag_pattern "content"
                >>= fmap (take 10) . recentFirst
                >>= renderRss feedCfg feedCtx

        paginated_tags <- buildPaginateWith
            (sortRecentFirst >=> return . paginateEvery pageSize)
            tag_pattern
            (\n -> fromFilePath $ if n == 1
                then "tags" </> tag </> "index.html"
                else "tags" </> tag </> show n </> "index.html")

        paginateRules paginated_tags $ \page_number pattern -> do
            route idRoute
            compile $ do
                all_posts <- recentFirst =<< loadAll (tag_pattern .&&. hasNoVersion)
                let number = length (all_posts :: [Item String])

                images <- getImages
                posts <- recentFirst =<< loadAll (pattern .&&. hasNoVersion)

                let excerpt = "There are " <> show number <> " posts tagged with "
                            <> tag <> "."
                let ctx = constField "title" title <>
                        constField "tag" tag <>
                        constField "number" (show number) <>
                        constField "excerpt" excerpt <>
                        imageField "image" images <>
                        listField "posts" postCtx (return posts) <>
                        field "rss_feed" (fmap (maybe empty toUrl) . getRoute . setVersion (Just "rss") . itemIdentifier) `mappend`
                        field "atom_feed" (fmap (maybe empty toUrl) . getRoute . setVersion (Just "atom") . itemIdentifier) `mappend`
                        paginateContext paginated_tags page_number <>
                        tagCtx tag

                makeItem ""
                    >>= loadAndApplyTemplate "templates/tag.html" ctx
                    >>= loadAndApplyTemplate "templates/default.html" ctx
                    >>= relativizeUrls

--------------------------------------------------------------------------------
-- * Routing
--------------------------------------------------------------------------------

-- | Route files to directory indexes.
routeFileToDirectory :: Routes
routeFileToDirectory = customRoute fileToDirectory
  where fileToDirectory :: Identifier -> FilePath
        fileToDirectory ident = let p = toFilePath ident
                                    (dir,fn) = splitFileName p
                                    bn = dropExtension fn
                                in joinPath [dir, bn, "index.html"]

-- | Route posts to @$year$/$title$/index.html@.
routePosts :: Routes
routePosts = customRoute fileToDirectory
  where
    fileToDirectory :: Identifier -> FilePath
    fileToDirectory ident =
        let p = toFilePath ident
            fn = takeFileName p
            bn = drop 11 $ dropExtension fn
            y = take 4 fn
        in joinPath [y, bn, "index.html"]

-- | Route tag pages.
routeTags :: String -> Routes
routeTags bn = customRoute tagPath
    where tagPath ident = let p = toFilePath ident
                              fn = takeFileName p
                              t = dropExtension fn
                          in joinPath ["tags", t, bn]

--------------------------------------------------------------------------------
-- * Contexts
--------------------------------------------------------------------------------

postContext :: Tags -> Context String
postContext tags = mconcat
    [ modificationTimeField "mdate" "%B %e, %Y"
    , modificationTimeField "mdatetime" "%Y-%m-%d"
    , dateField "date" "%B %e, %Y"
    , dateField "datetime" "%Y-%m-%d"
    , tagsField' "tags" tags
    , sectionField "archive"
    , constField    "author" (feedAuthorName feedCfg)
    , constField    "author-meta" (feedAuthorName feedCfg)
    , defaultContext tags
    ]

feedContext :: Tags -> Context String
feedContext tags =
    postContext tags <>
    bodyField "description"

tagContext :: Tags -> b -> Context String
tagContext tags _ =
    tagCloudField' "tagcloud" 75.0 300.0 tags <>
    sectionField "tags" <>
    constField "layout" "page" <>
    defaultContext tags

defaultContext :: Tags -> Context String
defaultContext _ =
    tocField      "contents" <>
    bodyField     "body"     <>
    metadataField            <>
    strippedUrlField "url"   <>
    pathField     "path"     <>
    titleField    "title"    <>
    titleField    "title-meta" <>

    constField "twitter_card" "summary" <>
    constField "twitter_site" twitterUser <>
    constField "twitter_creator" twitterUser <>
    titleField "twitter_title" <>

    sectionField  "page" <>
    functionField "dropFileName" dropFN <>
    functionField "first" firstFN <>
    missingField
  where
    dropFN :: [String] -> Item a -> Compiler String
    dropFN [fn] _ = return . dropFileName $ fn
    dropFN _ _ = error "Called dropFileName with no arguments"

    firstFN :: [String] -> Item a -> Compiler String
    firstFN ss _ = case ss of
        [] -> error "Called first with no arguments"
        _  -> return . worker $ ss
      where
        worker [] = mempty
        worker (h:r) | null h    = worker r
                     | otherwise = h

-- ** Fields

tagsField' :: String -> Tags -> Context a
tagsField' =
    tagsFieldWith
        getTags
        simpleRenderLink
        (mconcat . intersperse ", ")
  where
    simpleRenderLink :: String -> Maybe FilePath -> Maybe BH.Html
    simpleRenderLink tag Nothing         =
      Just . (BH.a ! BA.href (toValue . dropFileName $ "/" </> "tags" </> tag </> "index.html")) $ toHtml tag
    simpleRenderLink tag (Just filePath) =
      Just . (BH.a ! BA.href (toValue . dropFileName $ "/" </> filePath)) $ toHtml tag

-- | Context to de-/activate menu entries.
sectionField :: String -> Context a
sectionField s = constField "section" s <>
                 constField ("section_" <> s) s

-- | Make a tag cloud.
tagCloudField' :: String -> Double -> Double -> Tags -> Context a
tagCloudField' key =
  tagCloudFieldWith key makeLink unwords
  where
    makeLink minSize maxSize tag _url count min' max' =
      -- Show the relative size of one 'count' in percent
      let diff     = 1 + fromIntegral max' - fromIntegral min'
          relative = (fromIntegral count - fromIntegral min') / diff
          size     = floor $ minSize + relative * (maxSize - minSize) :: Int
      in renderHtml
         . (BH.a ! BA.style (toValue $ "font-size: " <> show size <> "%")
                 ! BA.href (toValue . dropFileName $ "/" </> "tags" </> tag </> "index.html"))
         $ toHtml tag

-- | Absolute url to the resulting item
strippedUrlField :: String -> Context a
strippedUrlField key = field key $
    fmap (maybe empty strippedUrl) . getRoute . itemIdentifier
  where
    strippedUrl = dropFileName . toUrl

-- | If the "toc" field is defined in the context, replace it with the table
-- of contents markup generated by Pandoc.
tocField :: String -> Context String
tocField name = field name $ \item -> do
  value <- getMetadataField (itemIdentifier item) name
  case value of
    Nothing -> empty
    Just v -> if null v
              then empty
              else itemBody <$> tocCompiler

-- | Select an image and include the URL.
imageField :: String -> [FilePath] -> Context a
imageField name [] = field name (const empty)
imageField name files = field name $ \item -> do
    let m = length files
    let f = toFilePath . itemIdentifier $ item
    let c = hash f `mod` m
    let img = files !! c
    return img

-- * Compilers

-- | Compile a post to its table of contents.
tocCompiler :: Compiler (Item String)
tocCompiler = pandocCompilerWith
  readerOptions
  writerOptions
    { writerTableOfContents = True
    , writerTemplate = "$toc$"
    , writerStandalone = True
    }

-- | Load images for use with 'imageField'.
getImages :: Compiler [FilePath]
getImages =
    fmap (toFilePath . itemIdentifier) <$>
    (loadAll "assets/img/site-*" :: Compiler [Item CopyFile])
