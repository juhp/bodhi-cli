module Main (main) where

import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List.Split
--import Data.Time.LocalTime
import SimpleCmdArgs
import Web.Fedora.Bodhi


main :: IO ()
main =
  simpleCmdArgs Nothing "Query Bodhi REST API for json"
    "This tool queries various Bodhi REST API service endpoints outputting JSON" $
    subcommands
    [ Subcommand "override" "Show override" $
      argCmd bodhiOverride <$> strArg "NVR"
    , Subcommand "overrides" "Search overrides" $
      paramsCmd bodhiOverrides <$> some (strArg "KEY=VAL ...")
    , Subcommand "packages" "Search packages" $
      paramsCmd bodhiPackages <$> some (strArg "KEY=VAL ...")
    , Subcommand "release" "Show release" $
      argCmd bodhiRelease <$> strArg "RELEASE"
    , Subcommand "releases" "Search releases" $
      paramsCmd bodhiReleases <$> some (strArg "KEY=VAL ...")
    , Subcommand "update" "Show update" $
      argCmd bodhiUpdate <$> strArg "UPDATE"
    , Subcommand "updates" "Search updates" $
      paramsCmd bodhiUpdates <$> some (strArg "KEY=VAL ...")
    , Subcommand "user" "Show user" $
      argCmd bodhiUser <$> strArg "USER"
    , Subcommand "users" "Search users" $
      paramsCmd bodhiUsers <$> some (strArg "KEY=VAL ...")
    ]
  where
    argCmd cmd arg = do
      mres <- cmd arg
      case mres of
        Nothing -> error "Query failed"
        Just res -> putPretty res

    paramsCmd cmd args = do
      let params = readQuery args
      res <- cmd params
      mapM_ putPretty res

    readQuery [] = []
    readQuery (param:rest) =
      case splitOn "=" param of
        [k,_] | null k -> error $ "Bad key: " ++ param
        [_,v] | null v -> error $ "Missing value: " ++ param
        [k,v] -> makeItem k v : readQuery rest
        _ -> error $ "Bad parameter: " ++ param

    putPretty = BL.putStrLn . encodePretty
