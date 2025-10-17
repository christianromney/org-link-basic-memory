# org-link-basic-memory

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Emacs package that adds `memory://` link support to org-mode, integrating with the [Basic Memory](https://basicmemory.com) personal knowledge management system.

## Overview

Basic Memory uses `memory://` URLs to reference notes via permalinks that remain stable even when files are moved in the directory structure. This package allows you to:

- **Follow** `memory://` links in org-mode documents to open corresponding markdown files
- **Store** links from Basic Memory markdown files for use in org documents
- Seamlessly integrate your Basic Memory knowledge base with org-mode workflows

## Installation

### Requirements

- Emacs 25.1 or later
- org-mode 9.0 or later
- [basic-memory](https://basicmemory.com) CLI installed and configured
- A configured Basic Memory project

### Doom Emacs

Add to `~/.doom.d/packages.el`:

```elisp
(package! org-link-basic-memory
  :recipe (:host github
           :repo "christianromney/org-link-basic-memory"))
```

Then add to `~/.doom.d/config.el`:

```elisp
(use-package! org-link-basic-memory
  :after org)
```

Run `doom sync` and restart Emacs.

### Spacemacs

Add to `dotspacemacs-additional-packages`:

```elisp
(org-link-basic-memory :location (recipe
                                   :fetcher github
                                   :repo "christianromney/org-link-basic-memory"))
```

Then add to `dotspacemacs/user-config`:

```elisp
(use-package org-link-basic-memory
  :after org)
```

### use-package (vanilla Emacs)

```elisp
(use-package org-link-basic-memory
  :ensure t
  :after org
  :config
  ;; Optional: customize settings here
  )
```

### straight.el

```elisp
(straight-use-package
 '(org-link-basic-memory :type git
                          :host github
                          :repo "christianromney/org-link-basic-memory"))
```

### Manual Installation

Clone the repository:

```bash
git clone https://github.com/christianromney/org-link-basic-memory.git
```

Add to your init file:

```elisp
(add-to-list 'load-path "/path/to/org-link-basic-memory")
(require 'org-link-basic-memory)
```

## Usage

### Following Links

Click any `memory://` link in an org file, or place the cursor on it and press `C-c C-o`:

```org
* My Notes

Check out the [[memory://homepage-platform/distributed-tracing][Distributed Tracing]] documentation.
```

The package will:
1. Query the Basic Memory CLI to find the file by permalink
2. Resolve the full file path
3. Open the markdown file in Emacs

### Storing Links

1. Visit a Basic Memory markdown file (must have a `permalink` field in YAML frontmatter)
2. Run `M-x org-store-link` (typically bound to `C-c l`)
3. Switch to an org file
4. Run `M-x org-insert-link` (typically bound to `C-c C-l`)
5. Select the stored link from the list

The link will be created using the format `memory://[permalink]`.

#### Example Markdown File

```markdown
---
title: Distributed Tracing
type: note
permalink: homepage-platform/distributed-tracing
---

# Distributed Tracing

Content here...
```

When you store a link from this file, it creates: `[[memory://homepage-platform/distributed-tracing][Distributed Tracing]]`

## How It Works

### Link Resolution Process

When you follow `memory://homepage-platform/distributed-tracing`:

1. Extract permalink: `homepage-platform/distributed-tracing`
2. Query Basic Memory: `basic-memory tool search-notes --permalink 'homepage-platform/distributed-tracing'`
3. Parse JSON response to get `file_path` (e.g., `tracing.md`)
4. Get project path from: `basic-memory project info`
5. Combine paths: `/path/to/project/tracing.md`
6. Open file using org-mode's file handler

### Architecture

The package uses three helper functions:

- **`org-link-basic-memory--get-project-path`**: Extracts current project path from `basic-memory project info`
- **`org-link-basic-memory--search-by-permalink`**: Queries for notes using `basic-memory tool search-notes`
- **`org-link-basic-memory--extract-permalink`**: Parses YAML frontmatter to extract permalink field

## Configuration

The package works out of the box with no configuration required, but you can customize behavior if needed:

```elisp
;; Example: Add a hook when following memory links
(defun my-memory-link-hook ()
  "Custom behavior when opening memory links."
  (message "Opened Basic Memory note!"))

(add-hook 'org-follow-link-hook #'my-memory-link-hook)
```

## Troubleshooting

### "Could not determine Basic Memory project path"

Ensure the `basic-memory` CLI is installed and in your PATH:

```bash
which basic-memory
basic-memory project info
```

### "Could not find note with permalink"

Verify the permalink exists in your Basic Memory project:

```bash
basic-memory tool search-notes --permalink 'your-permalink-here'
```

### Links not storing from markdown files

Ensure your markdown files have valid YAML frontmatter with a `permalink` field:

```yaml
---
title: My Note
permalink: folder/note-name
---
```

## Examples

See the `examples/` directory for sample org files demonstrating link usage.

## Limitations

- Only works with the currently active Basic Memory project
- Requires the `basic-memory` CLI to be available in PATH
- Only supports permalink-based links (not title or pattern matching)
- Link storage only works when visiting markdown files with permalinks in frontmatter

## Future Enhancements

Potential improvements:

- Support for title-based links (`memory://Note Title`)
- Pattern-based link completion
- Multi-project support
- Caching of project path and search results
- Export support for different formats (HTML, LaTeX, etc.)
- Async link resolution for better performance

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- [Basic Memory](https://basicmemory.com) - The knowledge management system this integrates with
- [Org mode](https://orgmode.org) - The amazing Emacs organization system
- The Emacs community

## Documentation References

### Basic Memory
- [Knowledge Format](https://docs.basicmemory.com/guides/knowledge-format/) - Memory URL format and structure
- [CLI Reference](https://docs.basicmemory.com/guides/cli-reference/) - Command-line tool documentation
- [Basic Memory Home](https://basicmemory.com) - Official website

### Org Mode
- [Adding Hyperlink Types](https://orgmode.org/manual/Adding-Hyperlink-Types.html) - Official guide for custom link types
- [Org Mode Manual](https://orgmode.org/manual/) - Complete org-mode documentation

## Related Projects

- [basic-memory](https://github.com/cryeprecision/basic-memory) - The Basic Memory MCP server and CLI
- [org-roam](https://www.orgroam.com/) - Alternative note-taking system for org-mode

## Support

For issues and questions:
- File an issue on [GitHub](https://github.com/christianromney/org-link-basic-memory/issues)
- Check the Basic Memory [documentation](https://docs.basicmemory.com)
