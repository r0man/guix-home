You are a Guix package maintainer tasked with updating the Guix package named: $ARG1 to the latest upstream version.

Follow these comprehensive guidelines from the Guix Reference Manual and ensure the package builds and passes lint checks exactly as in create-guix-package.md.

## Update Process

0. **Plan the creation of the package:**
   - If this projects contains a `.beads/issues.jsonl` file we are using Beads (https://github.com/steveyegge/beads/) we track work in Beads instead of Markdown.
   - Run `bd quickstart` to see how.
   - Create an epic in Beads for this.

1. Determine the latest upstream version
   - Check the project's homepage, release page, tags, or Git history
   - Identify the correct version string format (e.g., 1.2.3, 2025-01-15, git version tag)

2. Update the package definition
   - Increment the `version` field
   - Update the `source` accordingly:
     - For `url-fetch`/release tarballs, update the `uri` template to use the new version
     - For `git-fetch`, update the `commit` and adjust `version` using `git-version` if used
   - Recompute `sha256`:
     - Tarballs: use `guix download <url>` to obtain the base32 SHA256
     - Git sources: compute with `guix hash -rx <path-to-checkout>` for the target commit, or run a build once to have Guix print the expected hash and update it
   - Adjust `file-name` in `origin` if necessary

3. Review build system and phases
   - Ensure the `build-system` is still appropriate; update phases if upstream changed the build process
   - Remove obsolete patches or phase substitutions no longer needed; update `substitute*` patterns if file paths changed
   - If upstream added/removed features or build flags, update `arguments` accordingly

4. Verify and update dependencies
   - Review `inputs`, `native-inputs`, and `propagated-inputs` for changes needed by the new version
   - Prefer existing Guix packages for new dependencies
   - Drop inputs no longer required

5. Metadata checks
   - Confirm `home-page`, `license`, `synopsis`, and `description` are still correct and conform to Guix conventions
   - Ensure synopsis is concise (under ~50 characters, no trailing period) and description is clear and complete

6. Style and formatting
   - Run `guix style` to format the definition and update common patterns

7. Build and lint (must be clean)
   - The package MUST build successfully
   - The package MUST pass all `guix lint` checks without warnings
   - Iterate on the definition until both conditions are met

## Helpful Commands

```bash
# Attempt automatic version/hash update and inspect the diff
guix refresh -u -L $(pwd) $ARG1

# If using a release tarball, compute the new hash
# (replace <url> with the resolved tarball URL for the new version)
guix download <url>

# If using git sources, compute the content hash from a local checkout of the target commit
guix hash -rx /path/to/local/checkout

# Format the package definition
guix style -L $(pwd) $ARG1

# Build the package (add current directory to the load path)
guix build -L $(pwd) $ARG1

# Run lint checks (must be clean)
guix lint -L $(pwd) $ARG1
```

## Expected Output

Provide:
1. A diff or updated `.scm` package definition reflecting the new version, source, hash, and any required adjustments (phases, inputs, metadata)
2. A brief explanation of upstream changes affecting the build (if any) and how you adapted the definition
3. Confirmation that the package builds successfully and that `guix lint` reports no warnings
4. Any notable considerations (e.g., removed patches, updated dependencies, changes in license or homepage)

The final result should be a cleanly updated Guix package for $ARG1 that builds and passes `guix lint` with zero warnings, consistent with the quality bar in create-guix-package.md.
