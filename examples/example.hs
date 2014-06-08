{-# LANGUAGE TemplateHaskell #-}
import System.BuildInfo

main :: IO ()
main = putStrLn $ "My build information: " ++ show $(buildInfo)
