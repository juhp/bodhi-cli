# bodhi-cli

An experimental Bodhi CLI client outputing JSON.

```
$ bodhi-cli --help
Query Bodhi REST API for json

Usage: bodhi-cli COMMAND
  This tool queries various Bodhi REST API service endpoints outputting JSON

Available options:
  -h,--help                Show this help text

Available commands:
  build                    Show build
  builds                   Search overrides by: nvr, packages, releases, updates
  comment                  Show comment
  comments                 Search comments by: like, search, updates, packages,
                           user, update_owner, ignore_user, since
  override                 Show override
  overrides                Search overrides by: like, search, builds, expired,
                           packages, releases, user
  packages                 Search packages by: like, search, name
  release                  Show release
  releases                 Search releases by: ids, name, updates, packages,
                           exclude_archived
  update                   Show update
  updates                  Search updates by: like, search, alias,
                           approved_since, approved_before, bugs, builds,
                           critpath, locked, modified_since, modified_before,
                           packages, pushed, pushed_since, pushed_before,
                           releases, release, request, severity, status,
                           submitted_since, submitted_before, suggest, type,
                           content_type, user, updateid, gating
  user                     Show user
  users                    Search users by: like, search, name, groups, updates,
                           packages
```

By default json object are pretty-printed.

Particular properties can be accessed by `--value key1.subkey.subsubkey`.

Use `--keys` to list the keys of an json Object.

## Examples

```
$ bodhi-cli release F32 -v candidate_tag
f32-updates-candidate
```

```
$ bodhi-cli updates packages=dnf releases=F33 -v title
dnf-4.2.21-1.fc33
dnf-4.2.19-1.fc33
```
