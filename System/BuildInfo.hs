{-# LANGUAGE TemplateHaskell #-}
module System.BuildInfo (
    buildInfo,
    BuildInfo(..)
) where

-- template Haskell
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift, lift, location)
import Language.Haskell.TH.Ppr (pprint)

import Data.Ratio ((%), numerator, denominator)
import Data.Time
import System.Directory (getCurrentDirectory)
import System.Process (runInteractiveProcess, waitForProcess)
import System.IO (hGetLine)
import System.FilePath ((</>), takeBaseName)
import System.Exit

data BuildInfo = BuildInfo
    { buildDate :: UTCTime
    , scmInfo :: Maybe ScmInfo
    } deriving (Show)

data ScmInfo = Git String
             | Svn String
             | Hg String
    deriving (Show)

instance Lift BuildInfo where
    lift (BuildInfo bDate scmInfo) = [| BuildInfo bDate scmInfo |]

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
    l <- location
    bi <- runIO $ genBuildInfo (basePath l)
    [| bi |]

    where basePath loc = takeBaseName $ loc_filename loc

-- useful for debugging
pprintQ q = putStrLn . pprint =<< runQ q

-- template Haskell free definitions
genBuildInfo :: FilePath -> IO BuildInfo
genBuildInfo basePath = do
    pwd <- getCurrentDirectory
    date <- getCurrentTime
    scmInfo <- genScmInfo (pwd </> basePath)
    return BuildInfo { buildDate=date
                     , scmInfo = scmInfo }

{- source code management information -}
genScmInfo :: FilePath -> IO (Maybe ScmInfo)
genScmInfo basePath =          git basePath
                      `firstM` svn
                      `firstM` hg

infixr 0 `firstM`
firstM :: Monad m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
firstM op1 op2 = do
    r1 <- op1
    case r1 of
        Just _ -> return r1
        Nothing -> op2

git :: FilePath -> IO (Maybe ScmInfo)
git basePath = do
    (_, out, _, handle) <- invokeGit
    exitCode <- waitForProcess handle
    print basePath
    case exitCode of
        ExitSuccess -> do
            output <- hGetLine out
            return $ Just (Git output)
        _ -> return Nothing

    where
        invokeGit = runInteractiveProcess "git" ["describe", "--always"]
                                          (Just basePath)
                                          Nothing -- environment

svn :: IO (Maybe ScmInfo)
svn = return Nothing

hg :: IO (Maybe ScmInfo)
hg = return Nothing
