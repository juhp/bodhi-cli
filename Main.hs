module Main (main) where

import Data.Aeson.Types
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Lazy as H
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
    [ Subcommand "build" "Show build" $
      argCmd bodhiBuild <$> keysOpt <*> valuesOpt <*> strArg "NVR"
    , Subcommand "builds" "Search overrides by: nvr, packages, releases, updates" $
      paramsCmd bodhiBuilds <$> keysOpt <*> valuesOpt <*> some (strArg "KEY=VAL ...")
    , Subcommand "comment" "Show comment" $
      argCmd bodhiComment <$> keysOpt <*> valuesOpt <*> strArg "ID"
    , Subcommand "comments" "Search comments by: like, search, updates, packages, user, update_owner, ignore_user, since" $
      paramsCmd bodhiComments <$> keysOpt <*> valuesOpt <*> some (strArg "KEY=VAL ...")
    , Subcommand "override" "Show override" $
      argCmd bodhiOverride <$> keysOpt <*> valuesOpt <*> strArg "NVR"
    , Subcommand "overrides" "Search overrides by: like, search, builds, expired, packages, releases, user" $
      paramsCmd bodhiOverrides <$> keysOpt <*> valuesOpt <*> some (strArg "KEY=VAL ...")
    , Subcommand "packages" "Search packages by: like, search, name" $
      paramsCmd bodhiPackages <$> keysOpt <*> valuesOpt <*> some (strArg "KEY=VAL ...")
    , Subcommand "release" "Show release" $
      argCmd bodhiRelease <$> keysOpt <*> valuesOpt <*> strArg "RELEASE"
    , Subcommand "releases" "Search releases by: ids, name, updates, packages, exclude_archived" $
      paramsCmd bodhiReleases <$> keysOpt <*> valuesOpt <*> some (strArg "KEY=VAL ...")
    , Subcommand "update" "Show update" $
      argCmd bodhiUpdate <$> keysOpt <*> valuesOpt <*> strArg "UPDATE"
    , Subcommand "updates" "Search updates by: like, search, alias, approved_since, approved_before, bugs, builds, critpath, locked, modified_since, modified_before, packages, pushed, pushed_since, pushed_before, releases, release, request, severity, status, submitted_since, submitted_before, suggest, type, content_type, user, updateid, gating" $
      paramsCmd bodhiUpdates <$> keysOpt <*> valuesOpt <*> some (strArg "KEY=VAL ...")
    , Subcommand "user" "Show user" $
      argCmd bodhiUser <$> keysOpt <*> valuesOpt <*> strArg "USER"
    , Subcommand "users" "Search users by: like, search, name, groups, updates, packages" $
      paramsCmd bodhiUsers <$> keysOpt <*> valuesOpt <*> some (strArg "KEY=VAL ...")
    ]
  where
    keysOpt = switchWith 'k' "keys" "List keys of object"

    valuesOpt = optional (splitOn "." <$> strOptionWith 'v' "value" "KEY[.KEY..]" "Key value to show")

    argCmd cmd showkeys mkeys arg = do
      mres <- cmd arg
      case mres of
        Nothing -> error "Query failed"
        Just obj -> case mkeys of
          Nothing -> if showkeys then print $ H.keys obj
                     else (BL.putStrLn . encodePretty) obj
          Just keys -> putKeys showkeys keys obj

    paramsCmd cmd showkeys mkeys args = do
      let params = readQuery args
      objs <- cmd params
      case mkeys of
        Nothing -> mapM_ (if showkeys then print . H.keys else BL.putStrLn . encodePretty) objs
        Just keys -> mapM_ (putKeys showkeys keys) objs

    readQuery [] = []
    readQuery (param:rest) =
      case splitOn "=" param of
        [k,_] | null k -> error $ "Bad key: " ++ param
        [_,v] | null v -> error $ "Missing value: " ++ param
        [k,v] -> makeItem k v : readQuery rest
        _ -> error $ "Bad parameter: " ++ param

    putKey showkeys k o =
      case parseMaybe (.: (T.pack k)) o of
        Nothing -> return ()
        Just v ->
          case v of
            String t -> T.putStrLn t
            Object obj | showkeys -> print $ H.keys obj
            _ -> BL.putStrLn (encodePretty v)

    putKeys showkeys [] o = if showkeys then print $ H.keys o
                           else (BL.putStrLn . encodePretty) o
    putKeys showkeys [k] o = putKey showkeys k o
    putKeys showkeys (k:ks) o =
      case parseMaybe (.: (T.pack k)) o of
        Nothing -> return ()
        Just obj -> putKeys showkeys ks obj
