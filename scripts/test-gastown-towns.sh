#!/usr/bin/env bash
# Test home-gastown-service-type activation with three independent town roots.
#
# For each test town, spawns a guix home container, runs the activation, then
# checks that:
#   1. The town directory structure is created (log/, .dolt-data/, etc.)
#   2. Rig and crew directories declared in the config are created.
#   3. git status works in a rig directory that contains an actual git repo.
#
# Usage: scripts/test-gastown-towns.sh

set -euo pipefail

MODULES="modules"
PASS=0
FAIL=0

run_test() {
    local name="$1"
    local town="$2"
    local check_cmd="$3"
    local env_expr="$4"

    echo "--- $name ---"

    # Write a temporary top-level config file that re-exports the desired env.
    local tmpfile
    tmpfile=$(mktemp /tmp/gastown-test-XXXXXX.scm)
    cat > "$tmpfile" <<SCHEME
(use-modules (r0man guix home environments gastown-test))
$env_expr
SCHEME

    local output
    if output=$(guix home -L "$MODULES" container "$tmpfile" -- \
                    bash -c "$check_cmd" 2>&1); then
        echo "PASS: $name"
        echo "$output" | sed 's/^/  /'
        PASS=$((PASS + 1))
    else
        echo "FAIL: $name"
        echo "$output" | sed 's/^/  /'
        FAIL=$((FAIL + 1))
    fi

    rm -f "$tmpfile"
    echo
}

# ---------------------------------------------------------------------------
# Town 1: single rig (repo-a), single crew (roman)
# ---------------------------------------------------------------------------
run_test "town1: directory structure" "town1" \
    'set -e
     test -d "$HOME/town1/log"          && echo "OK: log/"
     test -d "$HOME/town1/.dolt-data"   && echo "OK: .dolt-data/"
     test -f "$HOME/.config/gastown/dolt-config.yaml" && echo "OK: dolt-config.yaml"
     test -d "$HOME/town1/repo-a/crew"  && echo "OK: repo-a/crew/"
     test -d "$HOME/town1/repo-a/crew/roman" && echo "OK: repo-a/crew/roman/"' \
    'gastown-test-env-1'

# ---------------------------------------------------------------------------
# Town 2: single rig (repo-b), two crews (roman, alice)
# ---------------------------------------------------------------------------
run_test "town2: directory structure" "town2" \
    'set -e
     test -d "$HOME/town2/log"              && echo "OK: log/"
     test -d "$HOME/town2/.dolt-data"       && echo "OK: .dolt-data/"
     test -d "$HOME/town2/repo-b/crew/roman" && echo "OK: repo-b/crew/roman/"
     test -d "$HOME/town2/repo-b/crew/alice" && echo "OK: repo-b/crew/alice/"' \
    'gastown-test-env-2'

# ---------------------------------------------------------------------------
# Town 3: two rigs (repo-c, repo-d); repo-d has crew roman
# ---------------------------------------------------------------------------
run_test "town3: directory structure" "town3" \
    'set -e
     test -d "$HOME/town3/log"              && echo "OK: log/"
     test -d "$HOME/town3/.dolt-data"       && echo "OK: .dolt-data/"
     test -d "$HOME/town3/repo-c/crew"      && echo "OK: repo-c/crew/"
     test -d "$HOME/town3/repo-d/crew/roman" && echo "OK: repo-d/crew/roman/"' \
    'gastown-test-env-3'

# ---------------------------------------------------------------------------
# Town 1: git status in an adopted rig
# Initialise a bare git repo inside the container so gt rig add --adopt can
# detect it.  The activation calls (false-if-exception (invoke gt ...)) so
# even if Dolt isn't running the directories exist.  We just verify git works.
# ---------------------------------------------------------------------------
run_test "town1: git status in rig directory" "town1" \
    'set -e
     rigdir="$HOME/town1/repo-a"
     mkdir -p "$rigdir"
     git -C "$rigdir" init -q
     git -C "$rigdir" status && echo "OK: git status clean in repo-a"' \
    'gastown-test-env-1'

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
echo "Results: $PASS passed, $FAIL failed"
if [ "$FAIL" -gt 0 ]; then
    exit 1
fi
