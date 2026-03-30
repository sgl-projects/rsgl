---
name: sync-sglref
description: Sync rsgl with the next unsynced commit from the sglref reference implementation. Use when the user wants to pull changes from sglref into rsgl.
disable-model-invocation: true
user-invocable: true
effort: max
context: fork
---

# Sync rsgl with sglref

Sync the rsgl package with the next unsynced commit from the sglref reference implementation.

## Context

- sglref (at `/Users/jochapjo/sgl_projects/sglref/`) is the reference SGL implementation. It is READ-ONLY.
- rsgl (at `/Users/jochapjo/sgl_projects/rsgl/`) is the CRAN-ready package derived from sglref.
- rsgl tracks sglref closely. Most files are 1:1 copies with the package name changed (`sglref` → `rsgl`).
- The file `sglref-sync.txt` in the rsgl root contains the short hash of the last sglref commit that rsgl is synced to.

## Flow

Sync **one commit at a time**. After opening the PR, stop and wait for the user to review and merge it before syncing the next commit. Do not batch multiple commits or start the next sync unprompted. Once the user confirms the PR has been merged, perform cleanup (Step 7) before proceeding.

## Instructions

Use `EnterWorktree` to create a git worktree, then perform all of the steps below within it. When finished, use `ExitWorktree` to return.

### Step 1: Find the next commit to sync

1. Read `sglref-sync.txt` to get the last synced commit hash.
2. Pull the latest in sglref: `git -C /Users/jochapjo/sgl_projects/sglref pull`
3. List new commits in chronological order: `git -C /Users/jochapjo/sgl_projects/sglref log <hash>..HEAD --oneline --reverse`
4. If there are no new commits, stop — nothing to sync.
5. Take the **first** (oldest) commit from the list. This is the one to sync.

### Step 2: Classify changed files

Get the diff for this single commit: `git -C /Users/jochapjo/sgl_projects/sglref diff <parent>..<hash> --stat`

Classify each changed file:

| Category | sglref files | Action |
|---|---|---|
| Direct-copy | `R/*.R`, `src/*.cpp`, `src/*.h`, `src/*.c` (non-generated), `tests/testthat/*` | Apply the change to the rsgl counterpart. Adjust any `sglref` → `rsgl` references that appear in package-level identifiers (e.g., `_sglref_` → `_rsgl_`, `useDynLib(sglref` → `useDynLib(rsgl`). |
| Generated | `parser.tab.c`, `parser.tab.h`, `scanner.c` | Copy the file as-is from sglref into rsgl's `src/` directory. These are generated from `parser.y`/`scanner.l` which rsgl does not carry. |
| sglref-only | `parser.y`, `scanner.l`, `src/test/*`, `makefile` | Skip — these files do not exist in rsgl. |
| Package metadata | `DESCRIPTION`, `NAMESPACE` | Review case-by-case. Structural changes (new imports, new S3 method registrations, new exports, dependency changes) must be adapted to rsgl. Do NOT blindly overwrite — rsgl has its own package name, title, description, URLs, etc. |
| Documentation | `man/*`, `README.Rmd` | Review case-by-case. Content changes should be ported; package name references must stay as `rsgl`. |
| Snapshots | `tests/testthat/_snaps/*` | Copy as-is from sglref. |
| CI | `.github/workflows/*` | Review case-by-case. |

### Step 3: Apply changes

For each changed file, read the sglref diff (`git -C /Users/jochapjo/sgl_projects/sglref diff <parent>..<hash> -- <file>`) and apply the equivalent change to the rsgl counterpart. Use your judgement on whether to copy the file wholesale or edit surgically — prefer surgical edits when only a few lines changed, and prefer full copy (with naming adjustments) when the file changed substantially.

### Step 4: Update the sync marker

Write this sglref commit's short hash to `sglref-sync.txt`.

### Step 5: Validate

Run `R CMD build . && R CMD check --as-cran rsgl_*.tar.gz` from the rsgl directory. If checks fail, fix the issues.

### Step 6: Commit and open a PR

Commit all changes with the original sglref commit message.

Open a PR against `main` with the original sglref commit message as the title. The PR body should describe what changed.

### Step 7: Cleanup

Once the user confirms the PR has been merged:

1. Pull latest main: `git pull`
2. Remove any worktrees created during the sync: `git worktree remove <path>` (use `--force` if needed).
3. Delete the local branch used for the PR: `git branch -D <branch>`.
