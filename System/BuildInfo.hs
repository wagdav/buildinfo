{-# LANGUAGE TemplateHaskell #-}
module System.BuildInfo (
    buildInfo,
    BuildInfo(..)
) where

-- template Haskell
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift, lift)
import Language.Haskell.TH.Ppr (pprint)

import Data.Ratio ((%), numerator, denominator)
import Data.Time
import System.Directory (getDirectoryContents, getCurrentDirectory)
import System.Process (runInteractiveCommand, waitForProcess)
import System.IO (hGetLine)
import System.FilePath ((</>))
import System.Exit

data BuildInfo = BuildInfo
    { buildDate :: UTCTime
    , scmInfo :: Maybe ScmInfo
    , versionInfo :: Maybe String
    } deriving (Show)

data ScmInfo = Git String
             | Svn String
             | Hg String
    deriving (Show)

instance Lift BuildInfo where
    lift (BuildInfo bDate scmInfo versionInfo) =
         [| BuildInfo bDate scmInfo versionInfo |]

instance Lift UTCTime where
    lift (UTCTime (ModifiedJulianDay day) diffTime) = 
        [| UTCTime (ModifiedJulianDay day) (fromRational (num % denom)) |]
        where
            num = numerator $ toRational diffTime
            denom = denominator $ toRational diffTime

instance Lift ScmInfo where
    lift (Git s) = [| Git s |]
    lift (Svn s) = [| Svn s |]
    lift (Hg s)  = [| Hg s |]

buildInfo :: Q Exp
buildInfo = do
    bi <- runIO genBuildInfo
    [| bi |]

-- useful for debugging
pprintQ q = putStrLn . pprint =<< runQ q

-- template Haskell free definitions
genBuildInfo :: IO BuildInfo
genBuildInfo = do
    version <- genVersionContent
    date <- getCurrentTime
    scmInfo <- genScmInfo
    return BuildInfo { buildDate = date
                     , scmInfo = scmInfo
                     , versionInfo = version }

{- source code management information -}
genScmInfo :: IO (Maybe ScmInfo)
genScmInfo =          git
             `firstM` svn
             `firstM` hg

infixr 0 `firstM`
firstM :: Monad m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
firstM op1 op2 = do
    r1 <- op1
    case r1 of
        Just _ -> return r1
        Nothing -> op2

git :: IO (Maybe ScmInfo)
git = do
    (_, out, _, handle) <- runInteractiveCommand "git describe --always"
    exitCode <- waitForProcess handle
    case exitCode of
        ExitSuccess -> do
            output <- hGetLine out
            return $ Just (Git output)
        _ -> return Nothing

svn :: IO (Maybe ScmInfo)
svn = return Nothing

hg :: IO (Maybe ScmInfo)
hg = return Nothing

{- find VERSION file -}
genVersionContent :: IO (Maybe String)
genVersionContent = do
    versionFilePath <- findVersion =<< getCurrentDirectory
    case versionFilePath of
        Just p -> do content <- readFile p
                     return (Just content)
        Nothing -> return Nothing

findVersion :: FilePath -> IO (Maybe FilePath)
findVersion directory = go directory 5
    where
    go _ 0 = return Nothing
    go dir hops = do
        ct <- getDirectoryContents dir
        if versionFile `elem` ct
            then return $ Just (dir </> versionFile)
            else go (dir </> "..") (hops - 1)

    versionFile = "VERSION"
