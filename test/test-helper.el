;;; test-helper.el --- Test helpers for org-link-basic-memory -*- lexical-binding: t; -*-

;;; Commentary:

;; Helper functions for setting up and tearing down isolated test environments
;; for org-link-basic-memory tests. Creates temporary basic-memory projects
;; to avoid polluting user's actual projects.

;;; Code:

(require 'cl-lib)

(defvar org-link-basic-memory-test--original-project nil
  "Store the original basic-memory project to restore after tests.")

(defvar org-link-basic-memory-test--temp-project-name "org-link-test-temp"
  "Name for the temporary test project.")

(defvar org-link-basic-memory-test--temp-dir nil
  "Temporary directory for test project.")

(defun org-link-basic-memory-test--get-current-project ()
  "Get the currently active basic-memory project name."
  (let ((output (shell-command-to-string "basic-memory project info 2>/dev/null")))
    (when (string-match "Project:\\s-*\\([^\n]+\\)" output)
      (string-trim (match-string 1 output)))))

(defun org-link-basic-memory-test--setup-temp-project ()
  "Create and switch to a temporary basic-memory project for testing.
Returns the temp directory path."
  ;; Save current project
  (setq org-link-basic-memory-test--original-project
        (org-link-basic-memory-test--get-current-project))

  ;; Create temp directory
  (setq org-link-basic-memory-test--temp-dir
        (make-temp-file "org-link-basic-memory-test-" t))

  ;; Add as basic-memory project
  (let ((cmd (format "basic-memory project add %s \"%s\" 2>&1"
                     org-link-basic-memory-test--temp-project-name
                     org-link-basic-memory-test--temp-dir)))
    (shell-command-to-string cmd))

  ;; Set as default and sync
  (shell-command-to-string
   (format "basic-memory project default %s 2>&1"
           org-link-basic-memory-test--temp-project-name))

  (shell-command-to-string "basic-memory sync 2>&1")

  org-link-basic-memory-test--temp-dir)

(defun org-link-basic-memory-test--teardown-temp-project ()
  "Remove temporary test project and restore original project."
  (when org-link-basic-memory-test--temp-dir
    ;; Switch back to original project if it exists
    (when org-link-basic-memory-test--original-project
      (shell-command-to-string
       (format "basic-memory project default %s 2>&1"
               org-link-basic-memory-test--original-project)))

    ;; Remove test project from config
    (shell-command-to-string
     (format "basic-memory project remove %s 2>&1"
             org-link-basic-memory-test--temp-project-name))

    ;; Clean up temp directory
    (when (file-exists-p org-link-basic-memory-test--temp-dir)
      (delete-directory org-link-basic-memory-test--temp-dir t))

    (setq org-link-basic-memory-test--temp-dir nil)
    (setq org-link-basic-memory-test--original-project nil)))

(defun org-link-basic-memory-test--create-test-file (relative-path permalink title content)
  "Create a test markdown file in the temp project.
RELATIVE-PATH is the path relative to temp directory.
PERMALINK is the permalink to use in frontmatter.
TITLE is the note title.
CONTENT is the body content."
  (let* ((full-path (expand-file-name relative-path org-link-basic-memory-test--temp-dir))
         (dir (file-name-directory full-path)))
    ;; Ensure directory exists
    (unless (file-exists-p dir)
      (make-directory dir t))

    ;; Write file with frontmatter
    (with-temp-file full-path
      (insert (format "---\ntitle: %s\ntype: note\npermalink: %s\n---\n\n%s\n"
                      title permalink content)))

    ;; Sync to basic-memory database
    (shell-command-to-string "basic-memory sync 2>&1")

    ;; Wait for watch service to index the file (needs significant time)
    (sleep-for 5)

    full-path))

(defun org-link-basic-memory-test--move-file (old-path new-relative-path)
  "Move a file from OLD-PATH to NEW-RELATIVE-PATH and sync.
NEW-RELATIVE-PATH is relative to temp directory."
  (let* ((new-path (expand-file-name new-relative-path org-link-basic-memory-test--temp-dir))
         (new-dir (file-name-directory new-path)))
    ;; Ensure new directory exists
    (unless (file-exists-p new-dir)
      (make-directory new-dir t))

    ;; Move file
    (rename-file old-path new-path)

    ;; Sync to update basic-memory database
    (shell-command-to-string "basic-memory sync 2>&1")

    ;; Wait for watch service to index the move (needs significant time)
    (sleep-for 5)

    new-path))

(defun org-link-basic-memory-test--search-permalink (permalink)
  "Search for a note by PERMALINK in the test project.
Returns the relative file path or nil."
  (let* ((cmd (format "basic-memory tool search-notes --permalink '%s' 2>/dev/null"
                      (shell-quote-argument permalink)))
         (output (shell-command-to-string cmd))
         (json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string))
    (condition-case nil
        (let* ((data (json-read-from-string output))
               (results (gethash "results" data)))
          (when (and results (> (length results) 0))
            (gethash "file_path" (car results))))
      (error nil))))

(provide 'test-helper)

;;; test-helper.el ends here
