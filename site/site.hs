--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           System.FilePath


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "img/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "pdf/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler


--    match (fromList ["about.rst", "contact.markdown"]) $ do
--        route   $ setExtension "html"
--        compile $ pandocCompiler
--            >>= loadAndApplyTemplate "templates/default.html" defaultContext
--            >>= relativizeUrls

--    match "posts/*" $ do
--        route $ setExtension "html"
--        compile $ pandocCompiler
--            >>= loadAndApplyTemplate "templates/post.html"    postCtx
--            >>= loadAndApplyTemplate "templates/default.html" postCtx
--            >>= relativizeUrls

    match "pages/*" $ do
        route $ matchRoute "pages/*.html"
              $ customRoute directoryIndex
        compile $ do
            getResourceBody
                >>= applyAsTemplate defaultContext
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

--    create ["archive.html"] $ do
--        route idRoute
--        compile $ do
--            posts <- recentFirst =<< loadAll "posts/*"
--            let archiveCtx =
--                    listField "posts" postCtx (return posts) `mappend`
--                    constField "title" "Archives"            `mappend`
--                    defaultContext
--
--            makeItem ""
--                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
--                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
--                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            --posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    --listField "posts" postCtx (return posts) `mappend`
                    --constField "title" "Shorty Goldstein's"  `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/splash.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

directoryIndex :: Identifier -> FilePath
directoryIndex = (</> "index.html") . dropExtension . takeFileName . toFilePath
