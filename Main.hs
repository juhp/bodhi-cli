module Main (main) where

import Data.Aeson.Types
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List.Split
import qualified Data.Text as T
import qualified Data.Text.IO as T
import SimpleCmdArgs
import Web.Fedora.Bodhi


main :: IO ()
main =
  simpleCmdArgs Nothing "Query Bodhi REST API for json"
    "This tool queries various Bodhi REST API service endpoints outputting JSON" $
    subcommands
    [ Subcommand "override" "Show override" $
      argCmd bodhiOverride <$> keysOpt <*> strArg "NVR"
    , Subcommand "overrides" "Search overrides" $
      paramsCmd bodhiOverrides <$> keysOpt <*> some (strArg "KEY=VAL ...")
    , Subcommand "packages" "Search packages" $
      paramsCmd bodhiPackages <$> keysOpt <*> some (strArg "KEY=VAL ...")
    , Subcommand "release" "Show release" $
      argCmd bodhiRelease <$> keysOpt <*> strArg "RELEASE"
    , Subcommand "releases" "Search releases" $
      paramsCmd bodhiReleases <$> keysOpt <*> some (strArg "KEY=VAL ...")
    , Subcommand "update" "Show update" $
      argCmd bodhiUpdate <$> keysOpt <*> strArg "UPDATE"
    , Subcommand "updates" "Search updates" $
      paramsCmd bodhiUpdates <$> keysOpt <*> some (strArg "KEY=VAL ...")
    , Subcommand "user" "Show user" $
      argCmd bodhiUser <$> keysOpt <*> strArg "USER"
    , Subcommand "users" "Search users" $
      paramsCmd bodhiUsers <$> keysOpt <*> some (strArg "KEY=VAL ...")
    ]
  where
    keysOpt = optional (splitOn "." <$> strOptionWith 'k' "key" "KEY[.KEY..]" "Key value to show")

    argCmd cmd mkeys arg = do
      mres <- cmd arg
      case mres of
        Nothing -> error "Query failed"
        Just obj -> case mkeys of
          Nothing -> (BL.putStrLn . encodePretty) obj
          Just keys -> putKeys keys obj

    paramsCmd cmd mkeys args = do
      let params = readQuery args
      objs <- cmd params
      case mkeys of
        Nothing -> mapM_ (BL.putStrLn . encodePretty) objs
        Just keys -> mapM_ (putKeys keys) objs

    readQuery [] = []
    readQuery (param:rest) =
      case splitOn "=" param of
        [k,_] | null k -> error $ "Bad key: " ++ param
        [_,v] | null v -> error $ "Missing value: " ++ param
        [k,v] -> makeItem k v : readQuery rest
        _ -> error $ "Bad parameter: " ++ param

    putKey k o =
      case parseMaybe (.: (T.pack k)) o of
        Nothing -> return ()
        Just v ->
          case v of
            String t -> T.putStrLn t
            _ -> BL.putStrLn (encodePretty v)

    putKeys [] o = (BL.putStrLn . encodePretty) o
    putKeys [k] o = putKey k o
    putKeys (k:ks) o =
      case parseMaybe (.: (T.pack k)) o of
        Nothing -> return ()
        Just obj -> putKeys ks obj
