;;; helm-hunks.el --- A git hunk source for helm -*- lexical-binding: t -*-

;; Copyright (C) 2012-2016 Free Software Foundation, Inc.

;; Author: @torgeir
;; Version: 1.0.0
;; Keywords: helm git hunks

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Todo:

;; TODO helm hunks, stage hunk
;; TODO helm hunks, kill hunk

;;; Code:

(require 'cl-macs)

(defconst helm-hunks--diff-re
  "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@ ?\\(.*\\)?$"
  "Regex to match the git diff hunk lines, e.g `@@ -{del-line},{del-len} +{add-line},{add-len} @@'")

(defvar helm-hunks--cmd-file-names
  "git --no-pager diff --name-only"
  "Git command to return names of the changed files")

(defvar helm-hunks--cmd-diffs
  "git --no-pager diff --no-color --no-ext-diff -U0"
  "Git command to show minimal diffs")

(defvar helm-hunks--cmd-git-root
  "git rev-parse --show-toplevel"
  "Git command to find the root folder of the current repo")

(defvar helm-hunks--msg-no-changes
  "No changes."
  "Message shown in the helm buffer when there are no changed hunks")

(defun helm-hunks--get-file-names ()
  "List file names of changed files"
  (let* ((result (shell-command-to-string helm-hunks--cmd-file-names))
         (raw-file-names (split-string result "\r?\n")))
    (delete "" raw-file-names)))

(defun helm-hunks--get-diffs ()
  "List raw diffs"
  (let* ((result (shell-command-to-string helm-hunks--cmd-diffs))
         (raw-diff-lines (split-string result "diff --git a/")))
    (delete "" raw-diff-lines)))

(defun helm-hunks--get-git-root ()
  (let* ((result (shell-command-to-string helm-hunks--cmd-git-root)))
    (replace-regexp-in-string "\r?\n" "" result)))

(defun helm-hunks--is-hunk-line (line)
  "Predicate to tell if `line' is a diff line"
  (string-match-p "^@@" line))

(defun helm-hunks--extract-hunk-lines (diff-lines)
  "Filters hunk lines from `diff-lines'"
  (remove-if-not 'helm-hunks--is-hunk-line (split-string diff-lines "\r?\n")))

(defun helm-hunks--get-hunk-lines-per-file ()
  "List file names and their changed hunks"
  (mapcar 'helm-hunks--extract-hunk-lines (helm-hunks--get-diffs)))

(defun helm-hunks--parse-hunk (hunk-line)
  "Parse `hunk-line' in to hunk with `line', `content' and the `type' of change"
  (when (string-match helm-hunks--diff-re hunk-line)
    (let* ((del-len (string-to-number (or (match-string 2 hunk-line) "1")))
           (add-line (string-to-number (match-string 3 hunk-line)))
           (add-len (string-to-number (or (match-string 4 hunk-line) "1")))
           (content (match-string 5 hunk-line))
           (type (cond ((zerop del-len) 'added)
                       ((zerop add-len) 'deleted)
                       (t 'modified)))
           (line (if (eq type 'deleted)
                     (1+ add-line)
                   add-line)))
      (list `(content . ,content)
            `(type . ,type)
            `(line . ,line)))))

(defun helm-hunks--assoc-file-name (file-name hunks)
  "Associates `file' name with each hunk of the list."
  (mapcar (lambda (hunk)
            (cons `(file . ,file-name) hunk))
          hunks))

(defun helm-hunks--get-hunks-by-file (file-names hunk-lines-per-file)
  "Join the changed file names with their corresponding hunks in a list"
  (cl-loop for file-name in file-names
           for hunk-lines in hunk-lines-per-file
           collect (let* ((parsed-hunks (mapcar 'helm-hunks--parse-hunk hunk-lines))
                          (parsed-hunks-with-file (helm-hunks--assoc-file-name file-name parsed-hunks)))
                     (cons file-name parsed-hunks-with-file))))

(defun helm-hunks--format-candidate-display (file hunk)
  "Formats a hunk for display as a line in helm"
  (unless (equal file helm-hunks--msg-no-changes)
    (let* ((line (cdr (assoc 'line hunk)))
           (content (cdr (assoc 'content hunk))))
      (if (not (equal "" content))
          (format "%s:%s - %s" file line content)
        (format "%s:%s" file line)))))

(defun helm-hunks--find-hunk-with-fn (find-file-fn real)
  "Jump to the changed line in the file using the provided `find-file-fn' function"
  (let* ((file (cdr (assoc 'file real)))
         (line (cdr (assoc 'line real)))
         (file-path (concat (helm-hunks--get-git-root) "/" file)))
    (funcall find-file-fn file-path)
    (goto-line line)))

(defun helm-hunks--action-find-hunk (real)
  "Jump to the changed line in the file using `find-file'"
  (helm-hunks--find-hunk-with-fn #'find-file real))

(defun helm-hunks--action-find-hunk-other-frame (real)
  "Jump to the changed line in the file using `find-file-other-frame'"
  (helm-hunks--find-hunk-with-fn #'find-file-other-frame real))

(defun helm-hunks--action-find-hunk-other-window (real)
  "Jump to the changed line in the file using `find-file-other-window'"
  (helm-hunks--find-hunk-with-fn #'find-file-other-window real))

(defun helm-hunks--find-hunk ()
  "Interactive defun to jump to the changed line in the file"
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action #'helm-hunks--action-find-hunk)))

(defun helm-hunks--find-hunk-other-frame ()
  "Interactive defun to jump to the changed line in the file in another frame"
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action #'helm-hunks--action-find-hunk-other-frame)))

(defun helm-hunks--find-hunk-other-window ()
  "Interactive defun to jump to the changed line in the file in another window"
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action #'helm-hunks--action-find-hunk-other-window)))

(defun helm-hunks--changes ()
  "List changes, on the form (display . real) suitable as candidates for the helm-hunks source"
  (reverse
   (let* ((hunks-by-file (helm-hunks--get-hunks-by-file (helm-hunks--get-file-names) (helm-hunks--get-hunk-lines-per-file)))
          (changes nil))
     (dolist (hunk-by-file hunks-by-file changes)
       (let* ((file (car hunk-by-file))
              (hunks (cdr hunk-by-file)))
         (dolist (hunk hunks)
           (push `(,(helm-hunks--format-candidate-display file hunk) . ,hunk)
                 changes)))))))

(defun helm-hunks--candidates ()
  "Candidates for the helm-hunks source, on the form (display . real)"
  (let ((candidates (helm-hunks--changes)))
    (if (seq-empty-p candidates)
        `((,helm-hunks--msg-no-changes))
      candidates)))

(defun helm-hunks--action-find-hunk (real)
  "Action to trigger on RET, for the helm-hunks source"
  (unless (equal real helm-hunks--msg-no-changes)
    (helm-hunks--find-hunk-with-fn #'find-file real)))

(defun helm-hunks--persistent-action (real)
  "Persistent action to trigger on follow, for the helm-hunks source"
  (unless (equal real helm-hunks--msg-no-changes)
    (helm-hunks--find-hunk-with-fn #'find-file real)))

(defvar helm-hunks--source
  (helm-build-async-source "Show hunks in project"
    :candidates-process 'helm-hunks--candidates
    :action '(("Go to hunk" . helm-hunks--action-find-hunk))
    :persistent-action 'helm-hunks--persistent-action
    :multiline t
    :nomark t
    :follow 1)
  "Helm-hunks source to list changed hunks in the project")

(defvar helm-hunks--keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    ;; TODO
    ;; (define-key map (kbd "C-r") 'helm-hunks--revert-hunk)
    ;; TODO
    ;; (define-key map (kbd "C-s") 'helm-hunks--stage-hunk)
    (define-key map (kbd "C-c C-o") 'helm-hunks--find-hunk-other-frame)
    (define-key map (kbd "C-c o") 'helm-hunks--find-hunk-other-window)
    map)
  "Keymap for `helm-hunks'")

;;;###autoload
(defun helm-hunks ()
  "Helm-hunks entry point"
  (interactive)
  (helm :sources '(helm-hunks--source)
        :keymap helm-hunks--keymap))
