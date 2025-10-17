# GitHub Setup Instructions

This document provides instructions for pushing this repository to GitHub.

## Prerequisites

- GitHub account
- Git CLI configured with your credentials

## Steps to Publish

### 1. Create GitHub Repository

Go to https://github.com/new and create a new repository:

- Repository name: `org-link-basic-memory`
- Description: `Emacs package for Basic Memory integration with org-mode`
- Visibility: Public (or Private, your choice)
- **Do NOT** initialize with README, license, or .gitignore (we already have these)

### 2. Add Remote and Push

After creating the repository on GitHub, run these commands:

```bash
cd ~/src/private/org-link-basic-memory

# Add the remote (replace YOUR-USERNAME with your GitHub username)
jj git remote add origin git@github.com:YOUR-USERNAME/org-link-basic-memory.git

# Push to GitHub
jj git push --all
```

Or if you prefer HTTPS:

```bash
jj git remote add origin https://github.com/YOUR-USERNAME/org-link-basic-memory.git
jj git push --all
```

### 3. Verify on GitHub

Visit your repository at `https://github.com/YOUR-USERNAME/org-link-basic-memory` to confirm all files are present.

### 4. Create Release Tag (Optional)

To create the v0.1.0 release:

```bash
# Create tag with jujutsu
jj bookmark create v0.1.0

# Push tag to GitHub
jj git push --all
```

Then create a GitHub release from the tag via the web interface.

### 5. Update README

If you used a different GitHub username, update the installation URLs in README.md:

```bash
# Replace christian-romney with your username
sed -i '' 's/christian-romney/YOUR-USERNAME/g' README.md
jj describe -m "Update GitHub username in README"
jj git push
```

## Doom Emacs Installation (After Publishing)

Once published, users can install with:

```elisp
;; In ~/.doom.d/packages.el
(package! org-link-basic-memory
  :recipe (:host github
           :repo "YOUR-USERNAME/org-link-basic-memory"))
```

## Testing the Package

To test that the package works correctly:

1. Load it in Emacs:
   ```elisp
   (load-file "~/src/private/org-link-basic-memory/org-link-basic-memory.el")
   ```

2. Open the example file:
   ```
   M-x find-file ~/src/private/org-link-basic-memory/examples/test-memory-links.org
   ```

3. Try following the test link in the example file

## Maintenance

### Creating New Releases

1. Update CHANGELOG.md with new version
2. Update version in org-link-basic-memory.el header
3. Commit changes
4. Create and push tag:
   ```bash
   jj bookmark create v0.2.0
   jj git push --all
   ```

### Syncing with Git

Remember that jujutsu is colocated with git. To see git status:

```bash
jj git fetch  # Fetch from remote
jj git push   # Push to remote
git status    # Check git status directly
```

## Support

For issues:
- GitHub Issues: `https://github.com/YOUR-USERNAME/org-link-basic-memory/issues`
- Discussions: Enable in repository settings if desired
