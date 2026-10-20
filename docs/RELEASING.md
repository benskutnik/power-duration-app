# Releasing

How changes move from a working branch to the live app.

The live app runs on Posit Connect Cloud, which redeploys automatically whenever `main` changes on GitHub. Merging into `main` is the release, so all work happens on a branch first.

## While working

1. In GitHub Desktop, create a branch from `main` (Branch → New Branch), named for the work, such as `v1-2-changes`.
2. Make each change in `app.R`, run the app with `shiny::runApp()`, and check it.
3. Commit each working step with a summary that names it, then Push origin. Pushing a branch backs it up without changing the live app.
4. Record any new or changed rule in `docs/DECISIONS.md`, and any change users will notice under `## Unreleased` in `CHANGELOG.md`.

## Before merging

1. Run the automated checks from the project folder:
   ```r
   source("tests/test_app.R")
   ```
   Every check should pass. If a rule changed on purpose, update its check.
2. Open the app in a web browser and walk through it as a user: enter a test, save it, download and re-upload the history, and look at each tab. Narrow the window to phone width once.
3. In `CHANGELOG.md`, rename `## Unreleased` to the new version and date, such as `## 1.2.0 (2026-11-15)`.
4. In `README.md`, update the version in the Status section.
5. Regenerate the deployment manifest, which records the R and package versions Connect Cloud installs:
   ```r
   rsconnect::writeManifest(appFiles = "app.R")
   ```
   Limiting it to `app.R` keeps `docs/`, `tests/`, and the example file out of the deployed app.
6. Commit these files together as `Prepare release X.Y.Z`, then Push origin.

## Releasing

1. In GitHub Desktop, switch to `main` (Current Branch → main).
2. Choose Branch → Merge into Current Branch, select the working branch, and create a merge commit.
3. Push origin. Connect Cloud starts a new deployment within a minute or so.
4. Watch the deployment in the Connect Cloud dashboard. If it fails, the deployment log explains why, and users stay on the previous version.

## After releasing

1. Open the live app, refresh the page, and repeat a short version of the browser walkthrough. Check it on a phone too.
2. Optional: on GitHub, draft a release with the tag `vX.Y.Z` on `main`, and paste in that version's changelog section.
3. Post the update in Discord, as a reply to the original app post.
4. Re-upload the changed files to the Claude project.

## Undoing a release

If the live app breaks after a release, revert the merge: in GitHub Desktop, open the History tab on `main`, right-click the merge commit, choose Revert Changes in Commit, and Push origin. Connect Cloud redeploys the previous version.