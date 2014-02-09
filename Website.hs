{-# LANGUAGE OverloadedStrings #-}

-- my website

module Main where

import Hakyll

main :: IO ()
main = hakyll $ do

   mapM_ justCopyFiles
         [ "files/*"
         , "images/*"
         , "html/lsystem/*"
         , "html/jobscript/*"
         , "html/ministg/*"
         , "html/pycol/*"
         , "html/js-turtle/*"
         , "docs/*"
         ]

   routeCompile "css/*" idRoute compressCssCompiler
   match "templates/*" $ compile templateCompiler

   match "*.markdown" $ do
      route $ setExtension "html"
      compile $ pandocCompiler
         >>= loadAndApplyTemplate "templates/default.html" defaultContext
         >>= relativizeUrls

justCopyFiles pattern = routeCompile pattern idRoute copyFileCompiler

routeCompile pattern r c = match pattern $ do
   route r
   compile c
