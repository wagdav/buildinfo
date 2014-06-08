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
import System.Process (runInteractiveCommand, waitForProcess)
import System.IO (hGetLine)
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
    bi <- runIO genBuildInfo
    [| bi |]

-- useful for debugging
pprintQ q = putStrLn . pprint =<< runQ q

-- template Haskell free definitions
genBuildInfo :: IO BuildInfo
genBuildInfo = do
    date <- getCurrentTime
    scmInfo <- genScmInfo
    return BuildInfo { buildDate=date
                     , scmInfo = scmInfo }

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
