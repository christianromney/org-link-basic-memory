;;; org-link-basic-memory.el --- Basic Memory link support for Org mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Christian Romney

;; Author: Christian Romney
;; URL: https://github.com/christianromney/org-link-basic-memory
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (org "9.0"))
;; Keywords: outlines, hypermedia, basic-memory, org

;;; Commentary:

;; This package adds support for memory:// links in Org mode, integrating
;; with the Basic Memory personal knowledge management system.
;;
;; Basic Memory uses memory:// URLs to reference notes via permalinks that
;; remain stable even when files are moved in the directory structure.
;;
;; Usage:
;;
;; 1. Install via your package manager (see README.md)
;;
;; 2. Visit a Basic Memory markdown file (with permalink in frontmatter)
;;
;; 3. Run `org-store-link` (typically bound to C-c l) to capture the link
;;
;; 4. In an org file, run `org-insert-link` (typically C-c C-l) to insert
;;
;; 5. Click the link or run `org-open-at-point` (C-c C-o) to open the file
;;
;; Link Format:
;;   memory://homepage-platform/distributed-tracing
;;   memory://project/note-name
;;
;; The permalink is extracted from the YAML frontmatter of Basic Memory files.
;;
;; For more information, see: https://basicmemory.com

;;; Code:

(require 'ol)
(require 'json)

;;; Helper Functions

(defun org-link-basic-memory--get-project-path ()
  "Get the current Basic Memory project path from the CLI.
Returns the absolute path to the project directory, or nil on error."
  (condition-case err
      (let* ((output (shell-command-to-string "basic-memory project info 2>/dev/null"))
             ;; Match the path which appears after "Path:" on the next line
             ;; The path may be surrounded by box-drawing characters (│)
             (path-line (when (string-match "Path:[^\n]*\n[^│\n]*│\\s-*\\([^\n│]+\\)" output)
                          (match-string 1 output))))
        (when (and path-line (not (string-empty-p path-line)))
          (string-trim path-line)))
    (error
     (message "Error getting Basic Memory project path: %s" (error-message-string err))
     nil)))

(defun org-link-basic-memory--search-by-permalink (permalink)
  "Search for a note by PERMALINK using the Basic Memory CLI.
Returns the file path relative to the project root, or nil if not found."
  (condition-case err
      (let* ((cmd (format "basic-memory tool search-notes --permalink '%s' 2>/dev/null"
                          (shell-quote-argument permalink)))
             (output (shell-command-to-string cmd))
             (json-object-type 'hash-table)
             (json-array-type 'list)
             (json-key-type 'string)
             (data (json-read-from-string output))
             (results (gethash "results" data)))
        (when (and results (> (length results) 0))
          (let ((result (car results)))
            (gethash "file_path" result))))
    (error
     (message "Error searching for permalink '%s': %s"
              permalink (error-message-string err))
     nil)))

(defun org-link-basic-memory--extract-permalink ()
  "Extract the permalink from the YAML frontmatter of the current buffer.
Returns the permalink string, or nil if not found."
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "^---\n")
      (let ((frontmatter-start (point))
            (frontmatter-end nil))
        ;; Find the closing ---
        (forward-line 1)
        (when (re-search-forward "^---\n" nil t)
          (setq frontmatter-end (point))
          ;; Search for permalink in the frontmatter
          (goto-char frontmatter-start)
          (when (re-search-forward "^permalink:\\s-*\\(.+\\)$" frontmatter-end t)
            (string-trim (match-string 1))))))))

;;; Link Type Functions

;;;###autoload
(defun org-link-basic-memory-follow (path _)
  "Follow a memory:// link by resolving PATH to a file and opening it.
PATH should be a permalink like 'homepage-platform/distributed-tracing'."
  (let ((project-path (org-link-basic-memory--get-project-path)))
    (if (not project-path)
        (error "Could not determine Basic Memory project path. Is basic-memory CLI available?")
      (let ((relative-path (org-link-basic-memory--search-by-permalink path)))
        (if (not relative-path)
            (error "Could not find note with permalink '%s' in Basic Memory" path)
          (let ((full-path (expand-file-name relative-path project-path)))
            (if (file-exists-p full-path)
                (org-link-open-as-file full-path nil)
              (error "File not found: %s (resolved from memory://%s)"
                     full-path path))))))))

;;;###autoload
(defun org-link-basic-memory-store ()
  "Store a memory:// link from a Basic Memory markdown file.
Only activates when visiting a file with a permalink in its frontmatter."
  (when (and (buffer-file-name)
             (string-match-p "\\.md\\'" (buffer-file-name)))
    (let ((permalink (org-link-basic-memory--extract-permalink)))
      (when permalink
        (org-link-store-props
         :type "memory"
         :link (concat "memory://" permalink)
         :description (or (when (looking-at "^title:\\s-*\\(.+\\)$")
                            (match-string 1))
                          permalink))
        t))))

;;; Registration

;;;###autoload
(with-eval-after-load 'org
  (org-link-set-parameters "memory"
                           :follow #'org-link-basic-memory-follow
                           :store #'org-link-basic-memory-store))

(provide 'org-link-basic-memory)

;;; org-link-basic-memory.el ends here
