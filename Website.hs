{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow ((>>>))
import Control.Monad (forM_)

import Hakyll

main :: IO ()
main = hakyll $ do
    route   "files/*" idRoute
    compile "files/*" copyFileCompiler

    route   "images/*" idRoute
    compile "images/*" copyFileCompiler

    route   "html/lsystem/*" idRoute
    compile "html/lsystem/*" copyFileCompiler

    route   "html/ministg/*" idRoute
    compile "html/ministg/*" copyFileCompiler

    route   "html/pycol/*" idRoute
    compile "html/pycol/*" copyFileCompiler

    route   "docs/*" idRoute
    compile "docs/*" copyFileCompiler

    route   "css/*" idRoute
    compile "css/*" compressCssCompiler

    compile "templates/*" templateCompiler

    let docs = [ "index.markdown"
               , "contact.markdown"
               , "writing.markdown"
               , "software.markdown"
               , "lsystem.markdown"
               , "bjpopray.markdown"
               , "imgtrans.markdown"
               , "notes.markdown"
               , "sprng.markdown"
               ]

    forM_ docs  $ \page -> do
        route   page $ setExtension "html"
        compile page $ pageCompiler
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler
