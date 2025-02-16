;;; consult-dir.el --- Insert paths into the minibuffer prompt  -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

;; Author: Karthik Chikmagalur
;; Maintainer: Karthik Chikmagalur <karthik.chikmagalur@gmail.com>
;; Created: 2021
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (consult "1.0"))
;; Keywords: convenience
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
;; configure `consult-dir-project-list-function'. Note that Projectile is NOT
;; required to install this package.
;;
;; - To make project.el projects available, configure
;; `consult-dir-project-list-function'.
;;
;; To change directory sources or their ordering, customize
;; `consult-dir-sources'.

;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'cl-lib)
(require 'bookmark)
(require 'recentf)
(require 'seq)
(require 'consult)

(declare-function projectile-load-known-projects "projectile")
(declare-function project--read-project-list "project")
(declare-function tramp-parse-sconfig "tramp")

;;; Declare variables for byte compiler

(defvar projectile-known-projects)
(defvar projectile-mode)
(defvar tramp-default-method)

(defgroup consult-dir nil
  "Consulting `completing-read'."
  :group 'convenience
  :group 'minibuffer
  :group 'consult
  :prefix "consult-dir-")

(defcustom consult-dir-shadow-filenames t
  "Shadow file names instead of replacing them when using `consult-dir'."
  :group 'consult-dir
  :type 'boolean)

(defcustom consult-dir-default-command #'find-file
  "Command to run after selecting a directory using `consult-dir'.

The default is to invoke `find-file' from the chosen
directory. Setting it to `consult-dir-dired' will instead open
the chosen directory in dired.

Any arbitrary function (of no variables) can be specified
instead. It is run with `default-directory' set to the directory
chosen using `consult-dir'."
  :group 'consult-dir
  :type '(choice (function-item :tag "Find file" find-file)
                 (function-item :tag "Open directory" consult-dir-dired)
                 (function :tag "Custom function")))

(defcustom consult-dir-tramp-default-remote-path "~"
  "Default path to use for remote directories when using `consult-dir'."
  :group 'consult-dir
  :type 'string)

(defcustom consult-dir-tramp-local-hosts '("/sudo:root@localhost:/")
  "A list of local hosts to include."
  :group 'consult-dir
  :type '(repeat string))

(defcustom consult-dir-project-list-function #'consult-dir-project-dirs
  "Function that returns the list of project directories.

These are used as candidates for switching when using `consult-dir'.

The options are

1. project.el project directories (the default)
2. projectile project directories
3. Any user-defined function. This function should take no
arguments and return a list of directories."
  :type '(radio
          (function-item :tag "Project.el projects" consult-dir-project-dirs)
          (function-item :tag "Projectile projects" consult-dir-projectile-dirs)
          (function :tag "User-defined function")))

(defcustom consult-dir-sources
  '(consult-dir--source-bookmark
    consult-dir--source-default
    consult-dir--source-project         ;projectile if available, project.el otherwise
    consult-dir--source-recentf
    consult-dir--source-tramp-local)
  "Directory sources for `consult-dir'.

There are several built-in sources available, including
bookmarked directories, recently visited file locations, project
directories and more, see customization options.

You can add your own directory sources to the list. The entry
must be a variable in the plist format specified by Consult, see
the documentation of `consult--multi' for details."
  :type
  '(repeat (choice
            (const :tag "Bookmarked directories"
                   consult-dir--source-bookmark)
            (const :tag "Current directory and project root"
                   consult-dir--source-default)
            (const :tag "All project directories"
                   consult-dir--source-project)
            (const :tag "Recent directories"
                   consult-dir--source-recentf)
            (const :tag "Local TRAMP methods"
                   consult-dir--source-tramp-local)
            (const :tag "Known ssh hosts"
                   consult-dir--source-tramp-ssh)
            (symbol :tag "Custom directory source"))))

(defcustom consult-dir-jump-file-command 'consult-find
  "Consult command used by `consult-dir-jump-file'.

Available options are `consult-find' and `consult-fd' (both
provided by Consult), or your own variation thereof.  It should
possess the same signature as `consult-find'."
  :type '(choice
          (const :tag "Use `find'" consult-find)
          (const :tag "Use `fd'" consult-fd)
          (function :tag "Custom consult function")))

(defcustom consult-dir-sort-candidates nil
  "If non nil, enable sorting of completion candidates."
  :type 'boolean)

(defun consult-dir-dired ()
    (interactive)
    (dired default-directory))

(defun consult-dir--tramp-parse-config (config)
  "Given a CONFIG, parse the hosts from it and return the results as a list."
  (let ((hosts))
    (when (and (file-exists-p config)
               (require 'tramp nil t))
      (dolist (cand (tramp-parse-sconfig config))
        (let ((user (if (car cand) (concat (car cand) "@")))
              (host (cadr cand)))
          (when host
            (push (concat "/" tramp-default-method
                          ":" user host
                          ":" consult-dir-tramp-default-remote-path)
                  hosts))))
      hosts)))

(defun consult-dir--tramp-ssh-hosts ()
  "Get a list of hosts from `consult-tramp-ssh-config'."
  (consult-dir--tramp-parse-config "~/.ssh/config"))

(defcustom consult-dir-bookmark-handlers
  '(nil)
  "Symbols used to filter bookmarks"
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
      (cl-remove-if-not (lambda (cand) (member (bookmark-get-handler cand) consult-dir-bookmark-handlers)))
      (cl-remove-if-not (lambda (cand)
                          (let ((bm (bookmark-get-bookmark-record cand)))
                            (when-let ((file (alist-get 'filename bm)))
                              (if (file-remote-p file)
                                  (string-suffix-p "/" file)
                                (file-directory-p file))))))
      (mapcar (lambda (cand) (propertize (car cand) 'consult--type file-narrow))))))

(defun consult-dir-project-dirs ()
  "Return a list of project directories managed by project.el."
  (unless (and (boundp 'project--list) (listp project--list))
    (and (require 'project nil t)
	 (fboundp #'project--read-project-list)
         (project--read-project-list)))
  (and (boundp 'project--list) (consp project--list)
       (mapcar #'car project--list)))

(defun consult-dir-projectile-dirs ()
  "Return a list of the project directories managed by Projectile."
  (if (not (bound-and-true-p projectile-known-projects))
      (if (require 'projectile nil t)
          (projectile-load-known-projects)
        (error (message "Projectile projects could not be loaded.")))
    projectile-known-projects))

(defvar consult-dir--project-list-hash nil
  "Hash to store the list of projects.

Used to avoid duplicating source entries in
`consult-dir'.")

(defun consult-dir--project-list-make (&optional refresh)
  "Make hash table to store the list of projects.

The table is stored in `consult-dir--project-list-hash'. When
REFRESH is non-nil force the hash to be rebuilt."
  (when consult-dir-project-list-function
    (let* ((proj-list (funcall consult-dir-project-list-function))
           (proj-sx (sxhash proj-list)))
      (unless (or refresh
                  (equal proj-sx (car consult-dir--project-list-hash)))
        (setq consult-dir--project-list-hash
              (cons proj-sx (consult--string-hash (delq nil proj-list)))))
      (cdr consult-dir--project-list-hash))))

(defun consult-dir--project-dirs ()
  "Return list of project directories."
  (when-let ((projects (consult-dir--project-list-make)))
    (hash-table-keys projects)))

(defun consult-dir--recentf-dirs ()
  "Return list of recentf dirs.

Entries that are also in the list of projects are removed."
  (let* ((current-dirs (consult-dir--default-dirs))
         (proj-list-hash (consult-dir--project-list-make))
         (in-other-source-p (lambda (dir) (not (or (and proj-list-hash (gethash dir proj-list-hash))
                                              (member dir current-dirs)))))
         (file-directory-safe (lambda (f) (or (and (if (file-remote-p f)
                                                       (string-suffix-p "/" f)
                                                     (file-directory-p f))
                                                   (file-name-as-directory f))
                                              (file-name-directory f)))))
    (thread-last recentf-list
      (mapcar file-directory-safe)
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
    :narrow ?m
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
    :enabled ,(lambda () consult-dir-project-list-function)
    :items ,(lambda () (let ((current-dirs (consult-dir--default-dirs)))
                    (seq-filter (lambda (proj) (not (member proj current-dirs)))
                                (consult-dir--project-dirs)))))
  "Project directory source for `consult-dir--pick'.")

(defvar consult-dir--source-tramp-local
  `(:name     "Local"
    :narrow   ?l
    :category file
    :face     consult-file
    :history  file-name-history
    :items    ,consult-dir-tramp-local-hosts)
  "Local host candidate source for `consult-dir'.")

(defvar consult-dir--source-tramp-ssh
  `(:name     "SSH Config"
    :narrow   ?s
    :category file
    :face     consult-file
    :history  file-name-history
    :items    ,#'consult-dir--tramp-ssh-hosts)
  "SSH Config candiadate source for `consult-dir'.")

(defun consult-dir--pick (&optional prompt)
  "Return a directory chosen from bookmarks and projects.

Optional argument PROMPT is the prompt."
  (let ((match (consult--multi consult-dir-sources
                               :prompt (or prompt "Switch directory: ")
                               :sort consult-dir-sort-candidates)))
    (pcase (plist-get (cdr match) :category)
      ('bookmark (bookmark-get-filename (car match)))
      ('file (car match)))))

;;;###autoload
(defun consult-dir-jump-file ()
  "Jump to file from the directory in the minibuffer prompt."
  (interactive)
  (let* ((mc (substitute-in-file-name (minibuffer-contents)))
         (dir (file-name-directory mc))
         (search (file-name-nondirectory mc)))
    (run-at-time 0 nil
                 (lambda () (funcall consult-dir-jump-file-command
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
`consult-dir-sources', which can be customized."
    (interactive)
    (if (minibufferp)
      (let* ((enable-recursive-minibuffers t)
             (file-name (file-name-nondirectory (minibuffer-contents)))
             (new-dir (consult-dir--pick))
             (new-full-name (concat (file-name-as-directory new-dir)
                                    file-name)))
        (when new-dir
          (if consult-dir-shadow-filenames
              (insert "/" new-full-name)
            (delete-minibuffer-contents)
            (insert new-full-name))))
      (let* ((new-dir (consult-dir--pick "In directory: "))
             (default-directory new-dir))
        (call-interactively consult-dir-default-command))))

(provide 'consult-dir)
;;; consult-dir.el ends here
