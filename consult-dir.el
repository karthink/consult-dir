;;; consult-dir.el --- Consult based directory picker -*- lexical-binding: t -*-

;; Author: Karthik Chikmagalur
;; Maintainer: Karthik Chikmagalur <karthik.chikmagalur@gmail.com>
;; Created: 2021
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (consult "0.9"))
;; Homepage: https://github.com/karthink/consult-dir
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Consult-dir implements commands to easily switch between "active"
;; directories. The directory candidates are collected from user bookmarks,
;; projectile project roots (if available), project.el project roots and recentf
;; file locations. The `default-directory' variable not changed in the process.
;;
;; Call `consult-dir' from the minibuffer to choose a directory with completion
;; and insert it into the minibuffer prompt, shadowing or deleting any existing
;; directory. The file name input is retained. This lets the user switch to
;; distant directories very quickly when finding files, for instance.
;;
;; Call `consult-dir' from a regular buffer to choose a directory with
;; completion and then interactively find a file in that directory. The command
;; run with this directory is configurable via `consult-dir-default-command' and
;; defaults to `find-file'.
;;
;; Call `consult-dir-jump-file' from the minibuffer to asynchronously find a
;; file anywhere under the directory that is currently in the prompt. This can
;; be used with `consult-dir' to quickly switch directories and find files at an
;; arbitrary depth under them. `consult-dir-jump-file' uses `consult-find' under
;; the hood.
;; 
;; To use this package, bind `consult-dir' and `consult-dir-jump-file' under the
;; `minibuffer-local-completion-map' or equivalent, and `consult-dir' to the global map.
;;
;; (define-key minibuffer-local-completion-map (kbd "C-x C-d") #'consult-dir)
;; (define-key minibuffer-local-completion-map (kbd "C-x C-j") #'consult-dir-jump-file)
;; (define-key global-map (kbd "C-x C-d") #'consult-dir)
;;
;; Directory sources configuration:
;; - To make recent directories available, turn on `recentf-mode'.
;;
;; - To make projectile projects available, turn on projectile-mode and
;; configure `consult-project-root-function'. Note that Projectile is NOT
;; required to install this package.
;;
;; - To make project.el projects available, configure
;; `consult-project-root-function'.
;;
;; To change directory sources or their ordering, customize
;; `consult-dir-directory-sources'.
   
;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))
(require 'bookmark)
(require 'project)
(require 'recentf)
(require 'seq)
(require 'consult)

;;; Declare variables for byte compiler

(defvar projectile-known-projects)
(defvar projectile-mode)

(defcustom consult-dir-shadow-filenames t
  "Shadow file names instead of replacing them when using `consult-dir-directory'."
  :type 'boolean)

(defcustom consult-dir-default-command #'find-file
  "Default command to run after selecting a directory using `consult-dir'."
  :type 'function)

(defcustom consult-dir-directory-sources
  '(consult-dir--source-bookmark
    consult-dir--source-default
    consult-dir--source-project         ;projectile if available, project.el otherwise
    consult-dir--source-recentf)
  "Sources used by `consult-dir--pick'."
  :type '(repeat symbol))

(defun consult-dir--default-dirs ()
  "Return the default directory and project root if available."
  (let ((fulldir (expand-file-name default-directory))
                       (dir (abbreviate-file-name default-directory))
                       (root (consult--project-root)))
                   (cond ((and root (equal fulldir root)) (list dir))
                         (root (list dir (abbreviate-file-name root)))
                         (t (list dir)))))

(defun consult-dir--bookmark-dirs ()
  "Return bookmarks that are directories."
  (bookmark-maybe-load-default-file)
  (let ((file-narrow ?f))
    (thread-last bookmark-alist
      (cl-remove-if-not (lambda (cand)
                          (let ((bm (bookmark-get-bookmark-record cand)))
                            (when-let ((file (alist-get 'filename bm)))
                              (file-directory-p file)))))
      (mapcar (lambda (cand) (let ((bm (bookmark-get-bookmark-record cand)))
                          (propertize (car cand) 'consult--type file-narrow)))))))

(defvar consult-dir--project-list-hash nil
  "Hash to store the list of projects.

Used to avoid duplicating source entries in
`consult-dir-directory'.")

(defun consult-dir--project-list ()
  "Make hash table to store the list of projects.

The table is stored in `consult-dir--project-list-hash'."
  (or consult-dir--project-list-hash
      (setq consult-dir--project-list-hash
            (consult--string-hash (delq nil (if (bound-and-true-p projectile-mode)
                                                (progn (projectile-load-known-projects)
                                                       projectile-known-projects)
                                              (project--ensure-read-project-list)
                                              (mapcar #'car project--list)))))))

(defun consult-dir--project-dirs ()
  "Return list of project directories."
  (hash-table-keys (consult-dir--project-list)))

(defun consult-dir--recentf-dirs ()
  "Return list of recentf dirs.

Entries that are also in the list of projects are removed."
  (let* ((current-dirs (consult-dir--default-dirs))
           (proj-hash (consult-dir--project-list))
           (in-other-source-p (lambda (dir) (not (or (gethash dir proj-hash)
                                                (member dir current-dirs))))))
    (thread-last recentf-list
      (mapcar #'file-name-directory)
      (delete-dups)
      (mapcar #'abbreviate-file-name)
      (seq-filter in-other-source-p))))

(defvar consult-dir--source-recentf
  `(:name "Recentf dirs"
    :narrow ?r
    :category file
    :face consult-file
    :history file-name-history
    :enabled ,(lambda () recentf-mode)
    :items ,#'consult-dir--recentf-dirs)
  "Recentf directory source for `consult-dir--pick'.")

(defvar consult-dir--source-bookmark
  `(:name "Bookmarks"
    :narrow ?b
    :category bookmark
    :face consult-file
    :history bookmark-history
    :items ,#'consult-dir--bookmark-dirs)
  "Bookmark directory source for `consult-dir--pick'.")

(defvar consult-dir--source-default
  `(:name "This directory/project"
    :narrow ?.
    :category file
    :face consult-file
    :history file-name-history
    :items ,#'consult-dir--default-dirs)
  "Current project directory for `consult-dir--pick'.")

(defvar consult-dir--source-project
  `(:name "Projects"
    :narrow ?p
    :category file
    :face consult-file
    :history file-name-history
    :enabled ,(lambda () consult-project-root-function)
    :items ,(lambda () (let ((current-dirs (consult-dir--default-dirs)))
                    (seq-filter (lambda (proj) (not (member proj current-dirs)))
                                (consult-dir--project-dirs)))))
  "Project directory source for `consult-dir--pick'.")

(defun consult-dir--pick (&optional prompt)
  "Return a directory chosen from bookmarks and projects.

Optional argument PROMPT is the prompt."
  (let ((match (consult--multi consult-dir-directory-sources
                               :prompt (or prompt "Switch directory: ")
                               :sort nil)))
    (pcase (plist-get (cdr match) :category)
      ('bookmark (bookmark-get-filename (car match)))
      ('file (car match)))))

;;;###autoload
(defun consult-dir-file-jump ()
  "Jump to file from the directory in the minibuffer prompt."
  (interactive)
  (let* ((shadow-pt (overlay-end rfn-eshadow-overlay))
         (mc (substring-no-properties
              (minibuffer-contents)
              (if shadow-pt
                  (- shadow-pt (minibuffer-prompt-end)) 0)))
         (dir (file-name-directory mc))
         (search (file-name-nondirectory mc)))
    (run-at-time 0 nil
                 (lambda () (consult-find
                        dir
                        (concat search
                                (unless (string-empty-p search)
                                  (plist-get (consult--async-split-style)
                                             :initial))))))
    (abort-recursive-edit)))
  
;;;###autoload
(defun consult-dir ()
    "Choose a directory and act on it.

The action taken on the directory is the value of
`consult-dir-default-command'. The default is to call
`find-file' starting at this directory.

When called from the minibuffer, insert the directory into the
minibuffer prompt instead. Existing minibuffer contents will be
shadowed or deleted depending on the value of
`consult-dir-shadow-filenames'.

The list of sources for directory paths is
`consult-dir-directory-sources', which can be customized."
    (interactive)
    (if (minibufferp)
      (let* ((enable-recursive-minibuffers t)
             (file-name (file-name-nondirectory (minibuffer-contents)))
             (new-dir (consult-dir--pick))
             (new-full-name (concat (file-name-as-directory new-dir)
                                    file-name)))
        (when new-dir
          (if consult-dir-shadow-filenames
              (insert (concat "/" new-full-name))
            (delete-minibuffer-contents)
            (insert new-full-name))))
      (let* ((new-dir (consult-dir--pick "In directory: "))
             (default-directory new-dir))
        (call-interactively consult-dir-default-command))))

(provide 'consult-dir)
;;; consult-dir.el ends here
