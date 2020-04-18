module Main (main) where

import Data.Aeson.Types
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
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
      argCmd bodhiBuild <$> jsonOpt <*> keysOpt <*> valuesOpt <*> strArg "NVR"
    , Subcommand "builds" "Search overrides by: nvr, packages, releases, updates" $
      paramsCmd bodhiBuilds <$> jsonOpt <*> keysOpt <*> valuesOpt <*> some (strArg "KEY=VAL ...")
    , Subcommand "comment" "Show comment" $
      argCmd bodhiComment <$> jsonOpt <*> keysOpt <*> valuesOpt <*> strArg "ID"
    , Subcommand "comments" "Search comments by: like, search, updates, packages, user, update_owner, ignore_user, since" $
      paramsCmd bodhiComments <$> jsonOpt <*> keysOpt <*> valuesOpt <*> some (strArg "KEY=VAL ...")
    , Subcommand "override" "Show override" $
      argCmd bodhiOverride <$> jsonOpt <*> keysOpt <*> valuesOpt <*> strArg "NVR"
    , Subcommand "overrides" "Search overrides by: like, search, builds, expired, packages, releases, user" $
      paramsCmd bodhiOverrides <$> jsonOpt <*> keysOpt <*> valuesOpt <*> some (strArg "KEY=VAL ...")
    , Subcommand "packages" "Search packages by: like, search, name" $
      paramsCmd bodhiPackages <$> jsonOpt <*> keysOpt <*> valuesOpt <*> some (strArg "KEY=VAL ...")
    , Subcommand "release" "Show release" $
      argCmd bodhiRelease <$> jsonOpt <*> keysOpt <*> valuesOpt <*> strArg "RELEASE"
    , Subcommand "releases" "Search releases by: ids, name, updates, packages, exclude_archived" $
      paramsCmd bodhiReleases <$> jsonOpt <*> keysOpt <*> valuesOpt <*> some (strArg "KEY=VAL ...")
    , Subcommand "update" "Show update" $
      argCmd bodhiUpdate <$> jsonOpt <*> keysOpt <*> valuesOpt <*> strArg "UPDATE"
    , Subcommand "updates" "Search updates by: like, search, alias, approved_since, approved_before, bugs, builds, critpath, locked, modified_since, modified_before, packages, pushed, pushed_since, pushed_before, releases, release, request, severity, status, submitted_since, submitted_before, suggest, type, content_type, user, updateid, gating" $
      paramsCmd bodhiUpdates <$> jsonOpt <*> keysOpt <*> valuesOpt <*> some (strArg "KEY=VAL ...")
    , Subcommand "user" "Show user" $
      argCmd bodhiUser <$> jsonOpt <*> keysOpt <*> valuesOpt <*> strArg "USER"
    , Subcommand "users" "Search users by: like, search, name, groups, updates, packages" $
      paramsCmd bodhiUsers <$> jsonOpt <*> keysOpt <*> valuesOpt <*> some (strArg "KEY=VAL ...")
    ]
  where
    jsonOpt = switchWith 'j' "json" "Output json instead of yaml"

    keysOpt = switchWith 'k' "keys" "List keys of object"

    valuesOpt = optional (splitOn "." <$> strOptionWith 'v' "value" "KEY[.KEY..]" "Key value to show")

    argCmd :: (String -> IO (Maybe Object)) -> Bool -> Bool -> Maybe [String] -> String -> IO ()
    argCmd cmd json listkeys mkeys arg = do
      mobj <- cmd arg
      case mobj of
        Nothing -> error "Query failed"
        Just obj -> putKeys json listkeys (concat mkeys) (Object obj)

--    paramsCmd :: (Query -> IO [Object]) -> Bool -> Bool -> Maybe [String] -> String -> IO ()
    paramsCmd cmd json listkeys mkeys args = do
      let params = readQuery args
      objs <- cmd params
      mapM_ (putKeys json listkeys (concat mkeys) . Object) objs
      where
        readQuery [] = []
        readQuery (param:rest) =
          case splitOn "=" param of
            [k,_] | null k -> error $ "Bad key: " ++ param
            [_,v] | null v -> error $ "Missing value: " ++ param
            [k,v] -> makeItem k v : readQuery rest
            _ -> error $ "Bad parameter: " ++ param

    putPretty json = if json
                     then BL.putStrLn . encodePretty
                     else B.putStrLn . encode

    putObjKeys = T.putStrLn . T.intercalate ", " . H.keys

    putKeys :: Bool -> Bool -> [String] -> Value -> IO ()
    putKeys json listkeys [] val =
      case val of
        Object obj | listkeys -> putObjKeys obj
        _ -> putPretty json val
    putKeys json listkeys [k] val = putKey json listkeys k val
    putKeys json listkeys (k:ks) val =
      case val of
        Object obj ->
          case parseMaybe (.: (T.pack k)) obj of
            Nothing -> return ()
            Just v -> putKeys json listkeys ks v
        Array arr -> mapM_ (putKeys json listkeys (k:ks)) arr
        _ -> putPretty json val

    putKey json listkeys k val =
      case val of
        Object obj ->
          case parseMaybe (.: (T.pack k)) obj of
            Nothing -> return ()
            Just v ->
              case v of
                String t -> T.putStrLn t
                Object o | listkeys -> putObjKeys o
                Array arr -> mapM_ (putKeys json listkeys []) arr
                _ -> putPretty json v
        Array arr -> mapM_ (putKey json listkeys k) arr
        _ -> putPretty json val
