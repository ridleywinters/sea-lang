#import x"$REPO_ROOT/source/modules/raiment-devenv/build/common.justfile"

#==============================================================================
# default
#==============================================================================

[private]
default:
    @just --list --unsorted

#==============================================================================
# ensure
#==============================================================================

ensure:
    @just ensure-vscode-directory

#==============================================================================
# sync
#==============================================================================

# Syncs all subtrees and pushes to origin
#sync: repo-sync

#==============================================================================
# misc
#==============================================================================

init:
    git status --short
    @git diff-index --quiet HEAD --
    git fetch
    git pull
    echo git lfs push --all https://github.com/ridleywinters/lfs-host.git
    git lfs push --all git@github.com:ridleywinters/lfs-host.git
    -git remote add raiment-devenv git@github.com:ridleywinters/raiment-devenv.git 2> /dev/null
    -git remote add raiment-core git@github.com:ridleywinters/raiment-core.git 2> /dev/null
    -git remote add raiment-ui git@github.com:ridleywinters/raiment-ui.git 2> /dev/null
    -git remote add raiment-shell git@github.com:ridleywinters/raiment-shell.git 2> /dev/null
    -git subtree add --prefix=source/modules/raiment-devenv raiment-devenv main --squash --message="Merge commit"
    -git subtree add --prefix=source/modules/raiment-core raiment-core main --squash --message="Merge commit"
    -git subtree add --prefix=source/modules/raiment-ui raiment-ui main --squash --message="Merge commit"
    -git subtree add --prefix=source/modules/raiment-shell raiment-shell main --squash --message="Merge commit"
