;;; org-link-basic-memory-test.el --- Tests for org-link-basic-memory -*- lexical-binding: t; -*-

;;; Commentary:

;; Test suite for org-link-basic-memory package.
;; Tests are run in an isolated temporary basic-memory project to avoid
;; polluting the user's actual projects.

;;; Code:

(require 'ert)
(require 'json)

;; Load the package under test
(let ((load-path (cons (expand-file-name "..") load-path)))
  (require 'org-link-basic-memory))

;; Load test helpers
(load (expand-file-name "test-helper.el"
                        (file-name-directory (or load-file-name buffer-file-name))))

;;; Helper Functions

(defun org-link-basic-memory-test--get-project-path-for-test ()
  "Get the test project path from CLI."
  (let ((output (shell-command-to-string "basic-memory project info 2>/dev/null")))
    (when (string-match "Path:[^\n]*\n[^│\n]*│\\s-*\\([^\n│]+\\)" output)
      (string-trim (match-string 1 output)))))

;;; Tests

(ert-deftest org-link-basic-memory-test-permalink-stability-after-move ()
  "Test that permalink resolution works after a file is moved on disk.

This test validates the core claim that memory:// links remain stable
when files are relocated in the directory structure, as long as the
permalink in the frontmatter remains unchanged."
  (unwind-protect
      (progn
        ;; Setup: Create temp project and test file
        (org-link-basic-memory-test--setup-temp-project)

        (let* ((original-path "original/test-note.md")
               (moved-path "moved/subfolder/test-note.md")
               (permalink "test/stable-permalink")
               (title "Test Note for Permalink Stability")
               (content "This note tests permalink stability across file moves.")
               (original-full-path nil)
               (moved-full-path nil)
               (search-result-before nil)
               (search-result-after nil))

          ;; Step 1: Create test file in original location
          (setq original-full-path
                (org-link-basic-memory-test--create-test-file
                 original-path permalink title content))

          ;; Verify file was created
          (should (file-exists-p original-full-path))

          ;; Step 2: Verify permalink resolves to original location
          (setq search-result-before
                (org-link-basic-memory-test--search-permalink permalink))
          (should search-result-before)
          (should (string= search-result-before original-path))

          ;; Step 3: Move file to new location
          (setq moved-full-path
                (org-link-basic-memory-test--move-file original-full-path moved-path))

          ;; Verify file was moved
          (should (file-exists-p moved-full-path))
          (should-not (file-exists-p original-full-path))

          ;; Step 4: Verify permalink now resolves to NEW location
          (setq search-result-after
                (org-link-basic-memory-test--search-permalink permalink))
          (should search-result-after)
          (should (string= search-result-after moved-path))

          ;; Step 5: Verify the permalink in the file hasn't changed
          (with-temp-buffer
            (insert-file-contents moved-full-path)
            (goto-char (point-min))
            (should (re-search-forward "^permalink:\\s-*\\(.+\\)$" nil t))
            (should (string= (string-trim (match-string 1)) permalink)))))

    ;; Cleanup: Always remove temp project
    (org-link-basic-memory-test--teardown-temp-project)))

(ert-deftest org-link-basic-memory-test-follow-link-after-move ()
  "Test that org-link-basic-memory-follow works after file is moved.

This test validates the end-to-end behavior: that clicking a memory://
link will open the file even after it has been moved to a different
location on disk."
  (unwind-protect
      (progn
        ;; Setup: Create temp project and test file
        (org-link-basic-memory-test--setup-temp-project)

        (let* ((original-path "docs/guide.md")
               (moved-path "relocated/new-location/guide.md")
               (permalink "docs/user-guide")
               (title "User Guide")
               (content "Guide content here.")
               (original-full-path nil)
               (moved-full-path nil)
               (resolved-path-before nil)
               (resolved-path-after nil))

          ;; Create test file
          (setq original-full-path
                (org-link-basic-memory-test--create-test-file
                 original-path permalink title content))

          ;; Verify initial resolution works
          (let ((project-path (org-link-basic-memory-test--get-project-path-for-test))
                (relative-path (org-link-basic-memory-test--search-permalink permalink)))
            (should project-path)
            (should relative-path)
            (setq resolved-path-before (expand-file-name relative-path project-path))
            (should (file-exists-p resolved-path-before)))

          ;; Move the file
          (setq moved-full-path
                (org-link-basic-memory-test--move-file original-full-path moved-path))

          ;; Verify resolution works after move
          (let ((project-path (org-link-basic-memory-test--get-project-path-for-test))
                (relative-path (org-link-basic-memory-test--search-permalink permalink)))
            (should project-path)
            (should relative-path)
            (setq resolved-path-after (expand-file-name relative-path project-path))
            (should (file-exists-p resolved-path-after))
            (should (string= resolved-path-after moved-full-path)))

          ;; Verify paths are different (file actually moved)
          (should-not (string= resolved-path-before resolved-path-after))))

    ;; Cleanup
    (org-link-basic-memory-test--teardown-temp-project)))

(provide 'org-link-basic-memory-test)

;;; org-link-basic-memory-test.el ends here
