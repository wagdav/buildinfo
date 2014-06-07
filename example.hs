{-# LANGUAGE TemplateHaskell #-}
import BuildInfo

main :: IO ()
main = putStrLn $ "My build information: " ++ show $(buildInfo)
