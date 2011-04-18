{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow ((>>>))
import Control.Monad (forM_)

import Hakyll
import Data.Binary (Binary)
import Data.Typeable (Typeable)

main :: IO ()
main = hakyll $ do

   mapM_ justCopyFiles
         [ "files/*"
         , "images/*"
         , "html/lsystem/*"
         , "html/ministg/*"
         , "html/pycol/*"
         , "docs/*"
         ]

   routeCompile "css/*" idRoute compressCssCompiler
   match "templates/*" $ compile templateCompiler

   let docs = [ "index.markdown"
              , "contact.markdown"
              , "writing.markdown"
              , "software.markdown"
              , "lsystem.markdown"
              , "bjpopray.markdown"
              , "imgtrans.markdown"
              , "notes.markdown"
              , "sprng.markdown"
              , "lfg.markdown"
              ]

   forM_ docs  $ \page -> do
      routeCompile
         page
         (setExtension "html")
         (pageCompiler
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler)

justCopyFiles :: Pattern -> Rules
justCopyFiles pattern = routeCompile pattern idRoute copyFileCompiler

routeCompile :: (Binary a, Typeable a, Writable a) => Pattern -> Routes -> Compiler Resource a -> Rules
routeCompile pattern r c = match pattern $ do
   route r
   compile c

