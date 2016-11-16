{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

{- |
Module      : System.Docbuilder
Description : System.Docbuilder
Copyright   : Plow Technologies LLC
License     : MIT License

Maintainer  : Scott Murphy

Build documentation and put it in /docs

| -}

module System.Docbuilder ( buildTheDocsRules
                         , NamesThatMustBeDiscovered(..)
                         , GeneratedStaticRules(..)) where

import Development.Shake ( command_
                         , Rules
                         , Action
                         , want
                         , need
                         , (%>)
                         , liftIO)
import           Development.Shake.FilePath ((</>))
import           Control.Monad (filterM)

import qualified Filesystem
--       (isDirectory, getModified, listDirectory, isFile, writeFile) 
import qualified Filesystem.Path.CurrentOS as CurrentOS
--       (FilePath, (</>),  fromText)

import Data.Text (unpack)
import Control.Lens
import Data.Aeson.Lens
import Text.Regex.Lens
import Text.Regex.Quote
import Text.Regex.Posix (Regex)
import Data.Bifunctor (first)
import qualified Data.Yaml as Yaml


--------------------------------------------------
-- Types
--------------------------------------------------

-- | Find all the pieces of the directories and place them here.
data GeneratedStaticRules = GeneratedStaticRules {
                                  generatedWants :: [FilePath],
                                  generatedRules :: [Rules ()] }




-- | All Paths are the base path versions
data NamesThatMustBeDiscovered = NamesThatMustBeDiscovered
  { 
    buildTarget :: FilePath,
    cabalPath   :: FilePath,
      ltsPath   :: FilePath,
    packageName :: String
  } deriving (Eq,Show,Ord)

-- | Errors building documentation
data DocbuilderErrors = LTSNotFound 
                      | CabalNotFound
                      | BuildPlatform
                      | PkgNameNotFound String
  deriving (Show,Eq)


--------------------------------------------------
-- Rules
--------------------------------------------------

-- | Top level of document generation.
buildTheDocsRules :: Rules ()
buildTheDocsRules = do
  GeneratedStaticRules wants rules <- runDynamics
  want wants
  _ <- sequence rules
  return ()




-- | build elements that depend on specific configurations of stack.yaml
--  this mostly involves setting up the correct cabal and lts directories
--  for copying over the documentation. 
runDynamics :: Rules GeneratedStaticRules
runDynamics = do
  eitherNames <- liftIO buildNamesThatMustBeDiscovered
  either reportFailure generateRules eitherNames
 where
   generateRules names = return $ GeneratedStaticRules [haddockInDocsIndex     , haddockInStackWorkIndex names]
                                                       [stackHaddockRule names , docsHaddockRule names]
   reportFailure  e    = fail $ show e



-- | Build the documentation in the .stack-work folder  
stackHaddockRule :: NamesThatMustBeDiscovered -> Rules ()
stackHaddockRule names = haddockInStackWorkIndex names %> \_ -> do
  stackHaddockCommand


-- | Copy the documentation into the destination folder 
-- docsHaddockRule :: Rules ()
docsHaddockRule :: NamesThatMustBeDiscovered -> Rules ()
docsHaddockRule names = haddockInDocsIndex %> \_ -> do
    need [haddockInStackWorkIndex names]    
    copyOtherPackagesCommand      names -- This needs to come before copyHaddock
    copyHaddockCommand            names



--------------------------------------------------
-- Commands and other Action
--------------------------------------------------

-- | Command to call to build docs.
stackHaddockCommand :: Action ()
stackHaddockCommand = command_ [] cmdString opts
  where
    cmdString =  "stack"
    opts      = ["haddock"]

-- | Copy the files over to docs.
copyHaddockCommand :: NamesThatMustBeDiscovered -> Action ()
copyHaddockCommand names = command_ [] "rsync" ["-arv" , haddockInStackWork names  </> "." , haddockInDocs  ]

-- | Copy the rest of the folders.
copyOtherPackagesCommand :: NamesThatMustBeDiscovered -> Action ()
copyOtherPackagesCommand names = command_ [] "rsync" ["-arv" , haddockOtherPackagesInStackWork names </> "." , haddockInDocs]







-------------------------------------------------
-- Declarations for various directories
-------------------------------------------------

-- | Hidden directory for generated documents
haddockInStackWork :: NamesThatMustBeDiscovered -> FilePath
haddockInStackWork names = ".stack-work" </> "dist" </>"x86_64-linux"</>cabalPath</>"doc"</> "html" </> packageName
  where
    (NamesThatMustBeDiscovered { cabalPath
                               , packageName}) = names

-- |index.html for package docs
haddockInStackWorkIndex :: NamesThatMustBeDiscovered -> FilePath
haddockInStackWorkIndex names = haddockInStackWork names </> "index.html"


-- | haddock build packages for everything
haddockOtherPackagesInStackWork :: NamesThatMustBeDiscovered -> FilePath
haddockOtherPackagesInStackWork names = ".stack-work"</>"install"</>"x86_64-linux"</>ltsPath</>"7.10.3"</>"doc"
  where
    (NamesThatMustBeDiscovered {ltsPath}) = names

-- | directory to move things to ./docs
haddockInDocs :: FilePath
haddockInDocs = "docs" 

-- | docs/index.html
haddockInDocsIndex :: FilePath
haddockInDocsIndex = haddockInDocs </> "index.html"

-- | .stack-work install path 
stackWorkInstallPath :: CurrentOS.FilePath -> CurrentOS.FilePath
stackWorkInstallPath wd = (wd CurrentOS.</> ".stack-work" CurrentOS.</> "install")

-- | .stack-work dist path
stackWorkDistPath :: CurrentOS.FilePath -> CurrentOS.FilePath
stackWorkDistPath wd = (wd CurrentOS.</> ".stack-work" CurrentOS.</> "dist")

--------------------------------------------------
-- Dynamic Directory Lookup
--------------------------------------------------

-- | get the directories and filter files out
getDirectories :: CurrentOS.FilePath -> IO [CurrentOS.FilePath]
getDirectories wd = do
  dirs <- Filesystem.listDirectory wd
  filterM Filesystem.isDirectory dirs


-- | Get a full path target
-- | FilePath "<working-dir>/.stack-work/install/x86_64-linux"
getInstallTarget :: IO CurrentOS.FilePath
getInstallTarget = do
 wd         <- Filesystem.getWorkingDirectory
 (dir:_)    <- getDirectories $ stackWorkInstallPath wd
 return dir

-- | FilePath "<working-dir>/.stack-work/install/x86_64-linux"
getDistTarget :: IO CurrentOS.FilePath
getDistTarget = do
 wd         <- Filesystem.getWorkingDirectory
 (dir:_)    <- getDirectories $ stackWorkDistPath wd
 return dir



-- | build the 'NamesThatMustBeDiscovered' record, this will create the dynamic pieces
-- of the build.
buildNamesThatMustBeDiscovered  :: IO (Either DocbuilderErrors NamesThatMustBeDiscovered)
buildNamesThatMustBeDiscovered = do
  
                                installTarget      <-  getInstallTarget
                                distTarget         <-  getDistTarget
                                distDirs           <-  getDirectories distTarget
                                installDirs        <-  getDirectories installTarget
                                eitherPkgName      <-  getPackageInfo
                                
                                let eitherTextInstallDirs  = CurrentOS.encodeString  <$> installDirs
                                    eitherTextDistDirs     = CurrentOS.encodeString  <$> distDirs
                                    eitherLtsString        = eitherTextInstallDirs   ^?  folded .  regex [r|lts.*|]   . matchedString & maybe (Left LTSNotFound )   Right
                                    eitherCabalString      = eitherTextDistDirs      ^?  folded .  regex [r|Cabal.*|] . matchedString & maybe (Left CabalNotFound ) Right
                                    stringEncodedTarget    = CurrentOS.encodeString   $ installTarget

                                return (NamesThatMustBeDiscovered
                                          stringEncodedTarget  <$>
                                          eitherCabalString    <*>
                                          eitherLtsString      <*>
                                          eitherPkgName )




--------------------------------------------------
-- Package Info
--------------------------------------------------
-- | Get the name of the package
getPackageInfo :: IO (Either DocbuilderErrors String)
getPackageInfo = do
  eitherPkg <- Yaml.decodeFileEither "package.yaml" :: IO (Either Yaml.ParseException Yaml.Value)
  let convertedEitherPkg = convertLeft eitherPkg
      decodedPackageName = convertedEitherPkg >>= (\pkg -> convertMaybe $  (pkg ^?  key "name" . _String <&> unpack))
  return decodedPackageName    
  where
   convertLeft  = first (\parseException -> PkgNameNotFound  $ show parseException)
   convertMaybe = maybe (Left $ PkgNameNotFound "problem with key 'name'") Right



