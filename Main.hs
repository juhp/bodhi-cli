{-# LANGUAGE CPP #-}

module Main (main) where

#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.Key
import qualified Data.Aeson.KeyMap as M
#else
import qualified Data.HashMap.Lazy as M
#endif
import Data.Aeson.Types hiding (Parser)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import Data.List.Split
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Yaml (encode)
import SimpleCmdArgs
import Fedora.Bodhi

main :: IO ()
main =
  simpleCmdArgs Nothing "Query Bodhi REST API with YAML output"
    "This tool queries various Bodhi REST API service endpoints outputting YAML from JSON" $
    subcommands
    [ Subcommand "build" "Show build" $
      argCmd bodhiBuild <$> jsonOpt <*> keysOpt <*> valuesOpt <*> strArg "NVR"
    , Subcommand "builds" "Search builds by: nvr, packages, releases, updates" $
      paramsCmd bodhiBuilds <$> jsonOpt <*> keysOpt <*> valuesOpt <*> some (strArg "KEY=VAL ...")
    , Subcommand "comment" "Show comment" $
      argCmd bodhiComment <$> jsonOpt <*> keysOpt <*> valuesOpt <*> strArg "ID"
    , Subcommand "comments" "Search comments by: like, search, updates, packages, user, update_owner, ignore_user, since" $
      paramsCmd bodhiComments <$> jsonOpt <*> keysOpt <*> valuesOpt <*> some (strArg "KEY=VAL ...")
    , Subcommand "override" "Show override" $
      argCmdMaybe bodhiOverride <$> jsonOpt <*> keysOpt <*> valuesOpt <*> strArg "NVR"
    , Subcommand "overrides" "Search overrides by: like, search, builds, expired, packages, releases, user" $
      paramsCmd bodhiOverrides <$> jsonOpt <*> keysOpt <*> valuesOpt <*> some (strArg "KEY=VAL ...")
    , Subcommand "packages" "Search packages by: like, search, name" $
      paramsCmd bodhiPackages <$> jsonOpt <*> keysOpt <*> valuesOpt <*> some (strArg "KEY=VAL ...")
    , Subcommand "release" "Show release" $
      argCmd bodhiRelease <$> jsonOpt <*> keysOpt <*> valuesOpt <*> strArg "RELEASE"
    , Subcommand "releases" "Search active releases by: ids, name, updates, packages" $
      paramsCmdReleases
      <$> jsonOpt
      <*> keysOpt
      <*> valuesOpt
      <*> switchWith 'A' "archived" "include archived releases"
      <*> many (strArg "KEY=VAL ...")
    , Subcommand "update" "Show update" $
      argCmdMaybe bodhiUpdate <$> jsonOpt <*> keysOpt <*> valuesOpt <*> strArg "UPDATE"
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

    argCmd :: (String -> IO Object) -> Bool -> Bool -> Maybe [String] -> String
           -> IO ()
    argCmd cmd json listkeys mkeys arg = do
      obj <- cmd arg
      if listkeys
        then mapM_ putKeys . filter (not . null) $ getKeys (concat mkeys) (Object obj)
        else putKeysVal json mkeys (Object obj)

    argCmdMaybe :: (String -> IO (Maybe Object)) -> Bool -> Bool -> Maybe [String] -> String -> IO ()
    argCmdMaybe cmd json listkeys mkeys arg = do
      mobj <- cmd arg
      case mobj of
        Nothing -> error "Query failed"
        Just obj ->
          if listkeys
          then mapM_ putKeys . filter (not . null) $ getKeys (concat mkeys) (Object obj)
          else putKeysVal json mkeys (Object obj)

    paramsCmd :: (Query -> IO [Object]) -> Bool -> Bool -> Maybe [String]
              -> [String] -> IO ()
    paramsCmd cmd json listkeys mkeys args = do
      let params = map readParam args
      objs <- cmd params
      if listkeys then
        mapM_ putKeys . filter (not . null) . L.nub $ concatMap (getKeys (concat mkeys) . Object) objs
        else
        mapM_ (putKeysVal json mkeys . Object) objs

    paramsCmdReleases json listkeys mkeys archived args =
      paramsCmd bodhiReleases json listkeys mkeys $
      (if archived then id else ("exclude_archived=1" :)) args

    readParam :: String -> QueryItem
    readParam param =
      case splitOn "=" param of
        [k,_] | null k -> error $ "Bad key: " ++ param
        [_,v] | null v -> error $ "Missing value: " ++ param
        [k,v] -> makeItem k v
        _ -> error $ "Bad parameter: " ++ param

    putPretty json keyed =
      if json
      then
        (if keyed then BL.putStr else BL.putStrLn) . encodePretty
      else
        (if keyed then B.putStr else B.putStrLn) . encode

    putKeysVal :: Bool -> Maybe [String] -> Value -> IO ()
    putKeysVal json mkeys = putKeysVal' keys
      where
        keys = concat mkeys
        keyed = not (null keys)

        putKeysVal' :: [String] -> Value -> IO ()
        putKeysVal' [] v = putPretty json keyed v
        putKeysVal' (k:ks) v =
          case v of
            Object obj ->
              case parseMaybe (.: fromString k) obj of
                Nothing -> return ()
                Just v' ->
                  if null ks
                  then
                    case v' of
                      Array arr ->
                        mapM_ (putKeysVal' []) arr
                      _ -> putPretty json keyed v'
                  else putKeysVal' ks v'
            Array arr ->
              mapM_ (putKeysVal' (k:ks)) arr
            _ -> putPretty json keyed v

    putKeys = T.putStrLn . T.intercalate ", " . L.sort

    getKeys :: [String] -> Value -> [[T.Text]]
    getKeys [] val =
      case val of
        Object obj -> [map toText (M.keys obj)]
        _ -> []
    getKeys (k:ks) val =
      case val of
        Object obj ->
          case parseMaybe (.: fromString k) obj of
            Nothing -> []
            Just v -> if null ks then
              case v of
                Object o -> [map toText (M.keys o)]
                Array arr -> L.nub $ concatMap (getKeys []) arr
                _ -> []
              else getKeys ks v
        Array arr -> L.nub $ concatMap (getKeys (k:ks)) arr
        _ -> []

#if !MIN_VERSION_aeson(2,0,0)
    fromString = T.pack
    toText = id
#endif
