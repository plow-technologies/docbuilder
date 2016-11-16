module Main where

import Development.Shake ( shakeArgs
                         , shakeOptions
                         , shakeFiles
                         , shakeProgress
                         , progressSimple)

import System.Docbuilder (buildTheDocsRules)

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles    = buildDir
                              , shakeProgress = progressSimple}
                 buildTheDocsRules
  where
    buildDir = "_docBuild"
