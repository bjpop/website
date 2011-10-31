{-# LANGUAGE OverloadedStrings #-}

-- my website

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
              , "rng_test.markdown"
              , "linuxPerfEvents.markdown"
              ]

   forM_ docs  $ \page -> do
      routeCompile
         page
         (setExtension "html")
         (pageCompiler
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler)

-- justCopyFiles :: Pattern a -> Rules
justCopyFiles :: Pattern a -> RulesM (Pattern CopyFile)
justCopyFiles pattern = routeCompile pattern idRoute copyFileCompiler

-- routeCompile :: (Binary a, Typeable a, Writable a) => Pattern b -> Routes -> Compiler Resource a -> Rules
routeCompile :: (Binary a, Writable a, Typeable a) =>
                 Pattern b -> Routes -> Compiler Resource a -> RulesM (Pattern a)
routeCompile pattern r c = match pattern $ do
   route r
   compile c




