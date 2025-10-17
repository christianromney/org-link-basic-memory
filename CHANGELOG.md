# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0] - 2025-01-17

### Added
- Initial release of org-link-basic-memory
- Support for `memory://` link type in org-mode
- Link following via permalink resolution using basic-memory CLI
- Link storage from Basic Memory markdown files
- YAML frontmatter parsing to extract permalinks
- Automatic project path detection from basic-memory CLI
- Comprehensive error handling and user feedback
- Autoload cookies for easy package loading
- Documentation and examples

### Features
- Follow `memory://` links to open corresponding markdown files
- Store links from Basic Memory files with permalinks in frontmatter
- Seamless integration with org-mode's link system
- Works with the currently active Basic Memory project

[0.1.0]: https://github.com/christianromney/org-link-basic-memory/releases/tag/v0.1.0
