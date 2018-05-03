{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Lazy            as LBS
import           Data.Char                       (toLower)
import           Data.Hashable
import           Data.List
import qualified Data.Map                        as M
import           Data.Maybe
import           Data.Monoid
import           Data.Set                        (Set)
import qualified Data.Set                        as S
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Hakyll                          hiding (defaultContext)
import           System.FilePath
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as BH
import qualified Text.Blaze.Html5.Attributes     as BA
import qualified Text.HTML.TagSoup               as TS
import           Text.Pandoc
import           Text.URI                        (URI)
import qualified Text.URI                        as URI


{-# ANN module ("HLint: ignore Use liftM" :: String) #-}

--------------------------------------------------------------------------------
-- * Configuration
--------------------------------------------------------------------------------

amazonUSTag :: String
amazonUSTag = "passingcuriosity-20"

amazonAUTag :: String
amazonAUTag = "passingcurios-22"

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
    , feedRoot = "https://passingcuriosity.com"
    }

specialFeed :: String -> FeedConfiguration
specialFeed cat = feedCfg
    { feedTitle = "Passing Curiosity: Posts tagged " <> cat
    , feedDescription = "Latest posts tagged with " <> cat <> " from Passing Curiosity."
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
        compile $ getResourceLBS >>= withItemBody (unixFilterLBS "./bin/js-opt" [])

    match "assets/fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "assets/img/*" $ do
        version "large" $ do
            route   $ routeImage "large"
            compile $ getResourceLBS >>= withItemBody (unixFilterLBS "./bin/jpeg-opt" [])

        version "small" $ do
            route   $ routeImage "small"
            compile $ getResourceLBS >>= withItemBody (unixFilterLBS "./bin/jpeg-scale" [])

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

    match "errors/*" $ do
        route $ setExtension "html"
        compile $ do
            let ctx = defaultCtx

            contentCompiler
                >>= return . fmap demoteHeaders
                >>= loadAndApplyTemplate "templates/error.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx

    match "posts/*" $ do
        route routePosts
        compile $ do
            let ctx = postCtx

            contentCompiler
                >>= return . fmap demoteHeaders
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/post.html" ctx
                >>= relativizeUrls
                >>= loadAndApplyTemplate "templates/default.html" ctx

    match "about.md" $ do
        route routeFileToDirectory
        compile $ do
            let ctx = sectionField "about" <>
                      defaultCtx

            contentCompiler
                >>= loadAndApplyTemplate "templates/page.html" ctx
                >>= relativizeUrls
                >>= loadAndApplyTemplate "templates/default.html" ctx

    match "contact.md" $ do
        route routeFileToDirectory
        compile $ do
            let ctx = sectionField "contact" <>
                      defaultCtx

            contentCompiler
                >>= loadAndApplyTemplate "templates/page.html" ctx
                >>= relativizeUrls
                >>= loadAndApplyTemplate "templates/default.html" ctx

    match "index.md" $ do
        route   $ setExtension "html"
        compile $ do
            posts <- fmap (take 3) . recentFirst =<<
                loadAll ("posts/*" .&&. hasNoVersion)

            let indexCtx =
                    numberedListField "posts" postCtx (return posts) <>
                    sectionField "home" <>
                    defaultCtx

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= renderPandoc
                >>= loadAndApplyTemplate "templates/page.html"   indexCtx
                >>= relativizeUrls
                >>= loadAndApplyTemplate "templates/default.html" indexCtx

    match "tags.md" $ do
        route routeFileToDirectory
        compile $ do
            let ctx = tagCloudField' "tagcloud" 75.0 300.0 tags <>
                      sectionField "tags" <>
                      tagdensityField "terms" tags <>
                      defaultCtx

            getResourceBody
                >>= applyAsTemplate ctx
                >>= renderPandoc
                >>= loadAndApplyTemplate "templates/page.html" ctx
                >>= relativizeUrls
                >>= loadAndApplyTemplate "templates/default.html" ctx

    --
    -- Generated
    --

    create ["feed.rss"] $ do
        route idRoute
        compile $ do
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
            renderRss feedCfg feedCtx posts

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
            posts <- recentFirst =<< loadAll pattern

            let title = if page_number == 1
                    then "Archives"
                    else "Archives: Page " <> show page_number
            let ctx =
                    constField "title" title <>
                    constField "layout" "page" <>
                    sectionField "archive" <>
                    numberedListField "posts" postCtx (return posts) <>
                    paginateContext paginated_archives page_number <>
                    defaultCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" ctx
                >>= relativizeUrls
                >>= loadAndApplyTemplate "templates/default.html" ctx

    tagsRules tags $ \tag tag_pattern -> do
        let title = "Posts tagged " <> tag
        let slug = slugify tag

        paginated_tags <- buildPaginateWith
            (sortRecentFirst >=> return . paginateEvery pageSize)
            tag_pattern
            (\n -> fromFilePath $ if n == 1
                then "tags" </> slug </> "index.html"
                else "tags" </> slug </> show n </> "index.html")

        let p1_id = paginatePage paginated_tags 1
        let path = "tags" </> slug </> slug
        let feed = specialFeed tag

        paginateRules paginated_tags $ \page_number pattern -> do
            -- If this is the FIRST page, create feeds.
            when (1 == page_number) $ do
                -- Atom feed
                version "atom" $ do
                    route $ customRoute (const $ path <> ".xml")
                    compile $ loadAllSnapshots tag_pattern "content"
                        >>= fmap (take 10) . recentFirst
                        >>= renderAtom feed feedCtx

                -- RSS feed
                version "rss" $ do
                    route $ customRoute (const $ path <> ".rss")
                    compile $ loadAllSnapshots tag_pattern "content"
                        >>= fmap (take 10) . recentFirst
                        >>= renderRss feed feedCtx

            -- Make the current page.
            route idRoute
            compile $ do
                all_posts <- recentFirst =<< loadAll (tag_pattern .&&. hasNoVersion)
                let number = length (all_posts :: [Item String])

                posts <- recentFirst =<< loadAll (pattern .&&. hasNoVersion)

                let excerpt = "There are " <> show number <> " posts tagged with "
                            <> tag <> "."
                let ctx = constField "title" title <>
                        constField "tag" tag <>
                        constField "number" (show number) <>
                        constField "excerpt" excerpt <>
                        numberedListField "posts" postCtx (return posts) <>
                        tagFeedCtx p1_id <>
                        paginateContext paginated_tags page_number <>
                        tagCtx tag

                makeItem ""
                    >>= loadAndApplyTemplate "templates/tag.html" ctx
                    >>= relativizeUrls
                    >>= loadAndApplyTemplate "templates/default.html" ctx

paginatePage :: Paginate -> PageNumber -> Maybe Identifier
paginatePage pag pageNumber
    | pageNumber < 1                      = Nothing
    | pageNumber > (M.size . paginateMap $ pag) = Nothing
    | otherwise                           = Just $ paginateMakeId pag pageNumber

tagFeedCtx :: Maybe Identifier -> Context a
tagFeedCtx ident = case ident of
    Nothing  -> mempty
    Just pid -> feedField "rss" pid <> feedField "atom" pid
  where
    feedField name an_id = field
        (name <> "_feed")
        (\_ -> fmap (maybe empty toUrl) . getRoute . setVersion (Just name) $ an_id)

--------------------------------------------------------------------------------
-- * Routing
--------------------------------------------------------------------------------

routeImage :: String -> Routes
routeImage size = customRoute fn
  where
    fn :: Identifier -> FilePath
    fn i = let (p,f) = splitFileName $ toFilePath i
           in p </> size </> f

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
    bodyField     "body"     <>
    metadataField            <>
    strippedUrlField "url"   <>
    pathField     "path"     <>
    titleField    "title"    <>
    titleField    "title-meta" <>

    imageField "image" <>

    -- Default to main feeds
    constField "atom_feed" "/atom.xml" <>
    constField "rss_feed" "/feed.rss" <>

    sectionField  "page" <>
    functionField "dropFileName" dropFN <>
    functionField "first" firstFN <>
    missingField
  where
    dropFN :: [String] -> Item a -> Compiler String
    dropFN [fn] _ = return . dropFileName $ fn
    dropFN _ _    = error "Called dropFileName with no arguments"

    firstFN :: [String] -> Item a -> Compiler String
    firstFN ss _ = case ss of
        [] -> error "Called first with no arguments"
        _  -> return . worker $ ss
      where
        worker [] = mempty
        worker (h:r) | null h    = worker r
                     | otherwise = h

--------------------------------------------------------------------------------
-- ** Fields
--------------------------------------------------------------------------------

-- | A 'listField' which adds an @item-number@ field to the context.
numberedListField :: String -> Context a -> Compiler [Item a] -> Context b
numberedListField key ctx items =
    let ctx' = ctx <> listNumberField "item-number" items
    in listField key ctx' items

-- | Extend a 'Context' with an item's position in a list.
listNumberField :: String -> Compiler [Item a] -> Context b
listNumberField key items = field key $
    fmap (maybe empty show) . getNumber . itemIdentifier
  where
    getNumber :: Identifier -> Compiler (Maybe Int)
    getNumber ident = do
        idents <- items
        return . fmap (+1) . elemIndex ident $ fmap itemIdentifier idents

tagsField' :: String -> Tags -> Context a
tagsField' =
    tagsFieldWith
        getTags
        simpleRenderLink
        (mconcat . intersperse ", ")
  where
    simpleRenderLink :: String -> Maybe FilePath -> Maybe BH.Html
    simpleRenderLink tag Nothing         =
      Just . (BH.a ! BA.href (toValue . dropFileName $ "/" </> "tags" </> slugify tag </> "index.html")) $ toHtml tag
    simpleRenderLink tag (Just filePath) =
      Just . (BH.a ! BA.href (toValue . dropFileName $ "/" </> filePath)) $ toHtml tag

-- | Context to de-/activate menu entries.
sectionField :: String -> Context a
sectionField s = constField "section" s <>
                 constField ("section_" <> s) s

-- | Make a tag cloud.
tagCloudField' :: String -> Double -> Double -> Tags -> Context a
tagCloudField' key =
    tagCloudFieldWith key makeLink (unwords . filter (not . null))
  where
    makeLink minSize maxSize tag _url count min' max'
        | count <= 2 = mempty
        | otherwise  =
            -- Show the relative size of one 'count' in percent
            let diff     = 1 + fromIntegral max' - fromIntegral min'
                relative = (fromIntegral count - fromIntegral min') / diff
                size     = round $ minSize + relative * (maxSize - minSize) :: Int
            in renderHtml
                 . (BH.a ! BA.style (toValue $ "font-size: " <> show size <> "%")
                         ! BA.href (toValue . dropFileName $ "/" </> "tags" </> slugify tag </> "index.html"))
                 $ toHtml tag


-- | Display a set of terms with relative densities.
tagdensityField :: String -> Tags -> Context a
tagdensityField key tags =
    field key $ \_ -> renderTags mkLink (intercalate " <br>\n" . filter (not . null)) (sortTagsBy frequency tags)
  where
    frequency (t1, i1) (t2, i2) = compare (length i2) (length i1)
    mkLink :: String -> String -> Int -> Int -> Int -> String
    mkLink tag _ count _ max =
        let url = "/" </> "tags" </> slugify tag </> "index.html"
            weight = round (fromIntegral count / fromIntegral max * 100) :: Int
            size :: String
            size = show weight <> "%"
            score :: BH.Html
            score = (BH.span ! BA.class_ "score") (toHtml (" (" <> show count <> " posts)"))
            span :: BH.Html
            span = (BH.span ! BA.style (toValue $ "width: " <> size)
                            ! BA.class_ "density")
                   $ toHtml (show count)
            term :: BH.Html
            term = (BH.span ! BA.class_ "term") $ toHtml tag <> " " <> score
        in if count < 10
           then ""
           else renderHtml
               . (BH.a ! BA.href (toValue . dropFileName $ url)
                       ! BA.class_ "termdensity"
                 )
               $ term <> " " <> span

-- | Absolute url to the resulting item
strippedUrlField :: String -> Context a
strippedUrlField key = field key $
    fmap (maybe empty strippedUrl) . getRoute . itemIdentifier
  where
    strippedUrl = dropFileName . toUrl

-- | Select an image and include the URL.
imageField :: String -> Context a
imageField name =
    (f  name              ("assets/img/site-*" .&&. hasVersion "large")) <>
    (f (name <> "_small") ("assets/img/site-*" .&&. hasVersion "small"))
  where
    f n p = field n $ \item -> do
        files <- getImages p
        case files of
            [] -> empty
            _  -> do
                let m = length files
                let f = toFilePath . itemIdentifier $ item
                let c = hash (f <> f) `mod` m
                let img = files !! c
                return img
    -- | Load images for use with 'imageField'.
    getImages :: Pattern -> Compiler [FilePath]
    getImages p = do
        items <- loadAll p :: Compiler [Item LBS.ByteString]
        urls <- mapM (getRoute . itemIdentifier) $ items
        return $ catMaybes urls

--------------------------------------------------------------------------------
-- * Compilers
--------------------------------------------------------------------------------

-- | Compiler for blog content.
contentCompiler :: Compiler (Item String)
contentCompiler =
    pandocCompilerWith read_opts write_opts
        >>= return . fmap embedKMLImages . fmap bootstrap
        >>= shill
  where
    read_opts = defaultHakyllReaderOptions
    write_opts = defaultHakyllWriterOptions
        { writerHTMLMathMethod = MathJax mathjax
        }
    mathjax = "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"

-- | Sprinkle affiliate links, etc. over a blog.
shill :: Item String -> Compiler (Item String)
shill item = return $ fmap (withUrls amazon) item
  where
    amazon :: String -> String
    amazon url = if "amazon.com/" `isInfixOf` url
        then fromMaybe url $ urlAddQuery ("tag", amazonUSTag) url
        else url

--------------------------------------------------------------------------------
-- * Utility
--------------------------------------------------------------------------------

-- | Add a query parameter to a URL.
urlAddQuery :: (String, String) -> String -> Maybe String
urlAddQuery (key, value) url = do
    u <- URI.mkURI (T.pack url)
    k <- URI.mkQueryKey (T.pack key)
    v <- URI.mkQueryValue (T.pack value)
    let u' = (u { URI.uriQuery = (URI.QueryParam k v) : (URI.uriQuery u)})
    return (T.unpack . URI.render $ u')

-- | Rewrite HTML, classify @<embed>@ tags for @.kml@ files so JavaScript can
-- display them on embedded maps.
--
-- TODO: Use a 'Compiler' to convert KML files into GeoJSON. Then use GeoJSON
-- directly in the JS.
embedKMLImages :: String -> String
embedKMLImages = withTags $ \tag -> case tag of
    TS.TagOpen _ _ -> embed tag
    _              -> tag
  where
    embed :: TS.Tag String -> TS.Tag String
    embed t@(TS.TagOpen "embed" a) = case lookup "src" a of
        Nothing -> t
        Just src -> if ".kml" `isSuffixOf` src
            then TS.TagOpen "embed" (a <> [("class", "embed-kml-map")])
            else t
    embed tag = tag

bootstrap :: String -> String
bootstrap = withTags $ \tag -> case tag of
  TS.TagOpen "table" a -> TS.TagOpen "table" (a <> [("class", "table")])
  _                    -> tag

-- | Convert random 'String' to url-y 'String'.
slugify :: String -> String
slugify s = map (\c -> if p c then c else '-') $ map toLower s
  where
    cs = ['a'..'z'] ++ ['0'..'9'] ++ "-"
    p c = c `elem` cs
