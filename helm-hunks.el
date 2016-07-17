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

;;; Commentary:

;; A helm interface for browsing unstaged git hunks.
;;
;; Enable `helm-follow-mode' and trigger `helm-hunks' to jump around
;; unstaged hunks like never before.
;;
;; Credits/inspiration: git-gutter+ - https://github.com/nonsequitur/git-gutter-plus/

;;; Todo:

;; TODO kill visited hunk from helm-hunks

;;; Code:

(require 'cl-macs)

(defconst helm-hunks--diff-re
  "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
  "Regex to match the git diff hunk lines, e.g `@@ -{del-line},{del-len} +{add-line},{add-len} @@'")

(defvar helm-hunks--cmd-file-names
  "git --no-pager diff --name-only"
  "Git command to return names of the changed files")

(defvar helm-hunks--cmd-diffs
  "git --no-pager diff --no-color --no-ext-diff --unified=0"
  "Git command to show minimal diffs")

(defvar helm-hunks--cmd-git-root
  "git rev-parse --show-toplevel"
  "Git command to find the root folder of the current repo")

(defvar helm-hunks--msg-no-changes
  "No changes."
  "Message shown in the helm buffer when there are no changed hunks")

(defvar helm-hunks--is-preview
  nil
  "Is preview mode enabled, to show diff lines preview inside helm while navigating")

(defvar helm-hunks-refresh-hook
  nil
  "Hooks triggered whenever helm-hunks trigger git changes, so you can refresh your
favorite git-gutter on git changes")

;; Refresh git-gutter+ on git changes
(when (fboundp 'git-gutter+-refresh)
  (add-hook 'helm-hunks-refresh-hook 'git-gutter+-refresh))

(defun helm-hunks--get-file-names ()
  "List file names of changed files"
  (let* ((result (shell-command-to-string helm-hunks--cmd-file-names))
         (raw-file-names (split-string result "\r?\n")))
    (delete "" raw-file-names)))

(defun helm-hunks--get-diffs ()
  "List raw diffs per changed file"
  (let* ((result (shell-command-to-string helm-hunks--cmd-diffs))
         (split-diff-lines (split-string result "^diff --git a/"))
         (split-diff-lines-without-empties (delete "" split-diff-lines)))
    (mapcar (lambda (line)
              (concat "diff --git a/" line)
              line)
            split-diff-lines-without-empties)))

(defun helm-hunks--extract-hunk-lines (diff)
  "Split on ^@@ to group lists of each hunk's header and content lines in a list"
  (mapcar (lambda (hunk)
            (concat "@@" hunk))
          (delete "" (split-string diff "^@@"))))

(defun helm-hunks--get-git-root ()
  "Get the root folder of the current git repository"
  (let* ((result (shell-command-to-string helm-hunks--cmd-git-root)))
    (file-name-as-directory
     (replace-regexp-in-string "\r?\n" "" result))))

(defun helm-hunks--parse-hunk (diff-header-str hunk-str)
  "Parse `raw-hunk' into hunk with `line', `content' and the `type' of change"
  (let* ((hunk-lines (split-string hunk-str "\n"))
         (hunk-header-line (car hunk-lines))
         (content-lines (rest hunk-lines)))
    (when (string-match helm-hunks--diff-re hunk-header-line)
      (let* ((del-len (string-to-number (or (match-string 2 hunk-header-line) "1")))
             (add-line (string-to-number (match-string 3 hunk-header-line)))
             (add-len (string-to-number (or (match-string 4 hunk-header-line) "1")))
             (content (string-join content-lines "\n"))
             (type (cond ((zerop del-len) 'added)
                         ((zerop add-len) 'deleted)
                         (t 'modified)))
             (line (if (eq type 'deleted)
                       (1+ add-line)
                     add-line)))
        (list (cons 'diff-header diff-header-str)
              (cons 'hunk-header hunk-header-line)
              (cons 'content content)
              (cons 'raw-content (concat diff-header-str "\n" hunk-str))
              (cons 'type type)
              (cons 'line line))))))

(defun helm-hunks--assoc-file-name (file-name hunks)
  "Associates `file' name with each hunk of the list."
  (mapcar (lambda (hunk)
            (cons (cons 'file file-name)
                  hunk))
          hunks))

(defun helm-hunks--get-hunks-by-file (file-names diffs-per-file)
  "Join the changed file names with their corresponding hunks in a list"
  (cl-loop for file-name in file-names
           for diff-str in diffs-per-file
           collect (let* ((split-hunk (split-string diff-str "\r?\n"))
                          (diff-header-lines (subseq split-hunk 0 4))
                          (diff-header-str (string-join diff-header-lines "\n"))
                          (rest-str (string-join (nthcdr 4 split-hunk) "\n"))
                          (hunks-lines (helm-hunks--extract-hunk-lines rest-str))
                          (parsed-hunks (mapcar (lambda (hunk-lines)
                                                  (helm-hunks--parse-hunk diff-header-str hunk-lines))
                                                hunks-lines))
                          (parsed-hunks-with-file (helm-hunks--assoc-file-name file-name parsed-hunks)))
                     (cons file-name parsed-hunks-with-file))))

(defun helm-hunks--run-hooks-for-buffer-of-hunk (hunk)
  "Runs refresh hooks with the buffer visiting the `hunk's file"
  (let* ((path-of-hunk (cdr (assoc 'file hunk)))
         (buffer-name (file-name-nondirectory path-of-hunk)))
    (when buffer-name
      (with-current-buffer buffer-name
        (run-hooks 'helm-hunks-refresh-hook)))))

(defun helm-hunks--fontify-as-diff (content)
  "Fontify content as a diff shown in `diff-mode'"
  (with-temp-buffer
    (insert content)
    (diff-mode)
    (font-lock-default-function 'diff-mode)
    (font-lock-default-fontify-buffer)
    (buffer-string)))

(defun helm-hunks--format-candidate-for-display (file hunk)
  "Formats a hunk for display as a line in helm"
  (unless (equal file helm-hunks--msg-no-changes)
    (let* ((line (cdr (assoc 'line hunk)))
           (type (cdr (assoc 'type hunk)))
           (content (cdr (assoc 'content hunk)))
           (is-content-empty (equal "" content)))
      (if (and helm-hunks--is-preview
               (not is-content-empty))
          (helm-hunks--format-candidate-multiline file line type content)
        (helm-hunks--format-candidate-line file line type)))))

(defun helm-hunks--format-candidate-multiline (file line type content)
  (let ((fontified-content (helm-hunks--fontify-as-diff content)))
    (format "%s:%s (%s)\n%s" file line type fontified-content)))

(defun helm-hunks--format-candidate-line (file line type)
  (format "%s:%s (%s)" file line type))

(defun helm-hunks--find-hunk-with-fn (find-file-fn real)
  "Jump to the changed line in the file using the provided `find-file-fn' function"
  (let* ((file (cdr (assoc 'file real)))
         (line (cdr (assoc 'line real)))
         (file-path (concat (helm-hunks--get-git-root) file)))
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

(defun helm-hunks--toggle-preview-interactive ()
  "Toggle diff lines preview mode inside helm, while helm is open"
  (interactive)
  (let* ((is-preview (not helm-hunks--is-preview))
         (hunk (helm-get-selection))
         (file (cdr (assoc 'file hunk)))
         (line (cdr (assoc 'line hunk)))
         (type (cdr (assoc 'type hunk)))
         (candidate (helm-hunks--format-candidate-line file line type)))
    (setq helm-hunks--is-preview is-preview)
    (with-helm-alive-p
      (helm-force-update candidate))))

(defun helm-hunks--find-hunk-other-frame-interactive ()
  "Interactive defun to jump to the changed line in the file in another frame"
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action #'helm-hunks--action-find-hunk-other-frame)))

(defun helm-hunks--find-hunk-other-window-interactive ()
  "Interactive defun to jump to the changed line in the file in another window"
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action #'helm-hunks--action-find-hunk-other-window)))

(defun helm-hunks--stage-hunk-interactive ()
  "Interactive defun to stage the currently selected helm candidate's hunk (`real' value)"
  (interactive)
  (with-helm-alive-p
    (let ((real (helm-get-selection)))
      (when real
        (helm-hunks--stage-hunk real)
        (helm-refresh)
        (helm-hunks--run-hooks-for-buffer-of-hunk real)))))

(defun helm-hunks--stage-hunk (real)
  "Stage a hunk. Will `cd' to the git root to make git diff paths align with paths on disk as we're not nescessarily in the git root when helm-hunks is run, and diffs are gathered."
  (let ((raw-hunk-diff (cdr (assoc 'raw-content real))))
    (with-temp-buffer
      (insert raw-hunk-diff)
      (unless (zerop
               (shell-command-on-region
                (point-min)
                (point-max)
                (format "cd %s && git apply --unidiff-zero --cached -"
                        (shell-quote-argument (helm-hunks--get-git-root)))
                t t nil))
        (buffer-string)))))

(defun helm-hunks--changes ()
  "List changes, on the form `(display . real)' suitable as candidates for the helm-hunks source"
  (reverse
   (let* ((hunks-by-file (helm-hunks--get-hunks-by-file (helm-hunks--get-file-names)
                                                        (helm-hunks--get-diffs)))
          (changes nil))
     (dolist (hunk-by-file hunks-by-file changes)
       (let* ((file (car hunk-by-file))
              (hunks (cdr hunk-by-file)))
         (dolist (hunk hunks)
           (push `(,(helm-hunks--format-candidate-for-display file hunk) . ,hunk)
                 changes)))))))

(defun helm-hunks--candidates ()
  "Candidates for the helm-hunks source, on the form (display . real)"
  (let ((candidates (helm-hunks--changes)))
    (if (seq-empty-p candidates)
        `((,helm-hunks--msg-no-changes . nil))
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
    (define-key map (kbd "C-s") 'helm-hunks--stage-hunk-interactive)
    (define-key map (kbd "C-c C-o") 'helm-hunks--find-hunk-other-frame-interactive)
    (define-key map (kbd "C-c o") 'helm-hunks--find-hunk-other-window-interactive)
    (define-key map (kbd "C-c p") 'helm-hunks--toggle-preview-interactive)
    map)
  "Keymap for `helm-hunks'")

;;;###autoload
(defun helm-hunks ()
  "Helm-hunks entry point"
  (interactive)
  (helm :sources '(helm-hunks--source)
        :keymap helm-hunks--keymap))
