# ficktoberfest

A simple Haskell tool that silently marks all of the new PRs it receives via notifications with "invalid" label.

## Quick Start

### Cabal

```console
$ cabal v2-build
$ cabal v2-run exe:ficktoberfest <token-file>
```

### Stack

TBD

<!-- TODO(#6): Stack Quick Start section is not documented -->

## Token File

You can create a new token here: [https://github.com/settings/tokens](https://github.com/settings/tokens). Make sure to enable `Full control of private repositories` permission, the tool cannot assign labels without this permission.

The format of Token File is super simple. Just copy-past the token into an empty file:
```
<token>
```
