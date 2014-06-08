{-# LANGUAGE TemplateHaskell #-}
module BuildInfo (
    buildInfo,
    BuildInfo(..)
) where

-- template Haskell
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift, lift)
import Language.Haskell.TH.Ppr (pprint)

import Data.Ratio ((%), numerator, denominator)
import Data.Time

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

-- source code management information
genScmInfo :: IO (Maybe ScmInfo)
genScmInfo = do
    scmInfo <- git
    print scmInfo
    return scmInfo

git :: IO (Maybe ScmInfo)
git = return $ Just (Git "blabla")

svn :: IO (Maybe ScmInfo)
svn = return Nothing

hg :: IO (Maybe ScmInfo)
hg = return Nothing

-- from the main code you can write
-- show $(buildInfo)
