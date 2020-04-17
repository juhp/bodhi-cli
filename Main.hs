module Main (main) where

import Data.Aeson.Types
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Lazy as H
import Data.List.Split
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Yaml (encode)
import SimpleCmdArgs
import Web.Fedora.Bodhi

main :: IO ()
main =
  simpleCmdArgs Nothing "Query Bodhi REST API with YAML output"
    "This tool queries various Bodhi REST API service endpoints outputting YAML from JSON" $
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

    argCmd :: (String -> IO (Maybe Object)) -> Bool -> Maybe [String] -> String -> IO ()
    argCmd cmd showkeys mkeys arg = do
      mobj <- cmd arg
      case mobj of
        Nothing -> error "Query failed"
        Just obj -> putObj showkeys (concat mkeys) obj

--    paramsCmd :: (Query -> IO [Object]) -> Bool -> Maybe [String] -> String -> IO ()
    paramsCmd cmd showkeys mkeys args = do
      let params = readQuery args
      objs <- cmd params
      mapM_ (putObj showkeys (concat mkeys)) objs
      where
        readQuery [] = []
        readQuery (param:rest) =
          case splitOn "=" param of
            [k,_] | null k -> error $ "Bad key: " ++ param
            [_,v] | null v -> error $ "Missing value: " ++ param
            [k,v] -> makeItem k v : readQuery rest
            _ -> error $ "Bad parameter: " ++ param

    -- FIXME option to choose json instead of yaml
    putPretty = B.putStrLn . encode

    putObj :: Bool -> [String] -> Object -> IO ()
    putObj showkeys [] obj =
      if showkeys then putObjKeys obj
      else putPretty $ Object obj
    putObj showkeys keys obj =
      putKeys showkeys keys (Object obj)

    putObjKeys = T.putStrLn . T.intercalate ", " . H.keys

    putKeys :: Bool -> [String] -> Value -> IO ()
    putKeys showkeys [] val =
      case val of
        Object obj | showkeys -> putObjKeys obj
        _ -> putPretty val
    putKeys showkeys [k] val = putKey showkeys k val
    putKeys showkeys (k:ks) val =
      case val of
        Object obj ->
          case parseMaybe (.: (T.pack k)) obj of
            Nothing -> return ()
            Just v -> putKeys showkeys ks v
        Array arr -> mapM_ (putKeys showkeys (k:ks)) arr
        _ -> putPretty val

    putKey showkeys k val =
      case val of
        Object obj ->
          case parseMaybe (.: (T.pack k)) obj of
            Nothing -> return ()
            Just v ->
              case v of
                String t -> T.putStrLn t
                Object o | showkeys -> putObjKeys o
                Array arr -> mapM_ (putKeys showkeys []) arr
                _ -> putPretty v
        Array arr -> mapM_ (putKey showkeys k) arr
        _ -> putPretty val
