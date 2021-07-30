;; consult-file-jump.el --- consult file utils -*- lexical-binding: t -*-

;; Author: Karthik Chikmagalur
;; Maintainer: Karthik Chikmagalur <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
;; Homepage: https://github.com/karthink/consult-switch

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

;;; Commentary:

;; Consult-Switch implements commands to easily switch between "active"
;; directories when finding files from the minibuffer. (The default-directory is
;; not changed in the process.)

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))
(require 'bookmark)
(require 'project)
(require 'consult)

(defcustom consult-switch-shadow-filenames t
  "Shadow file names instead of replacing them when using
`consult-switch-directory'."
  :type 'boolean)

(defun consult--directory-bookmarks ()
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

(defcustom consult-switch-directory-sources
  '(consult-switch-bookmark-dirs
    consult-switch-current-dirs
    consult-switch-project-dirs
    consult-switch-recentf-dirs)
  "Sources used by `consult--switch-directory'."
  :type '(repeat symbol))

(defvar consult-switch-recentf-dirs
  `(:name "(r)ecentf dirs"
    :narrow ?r
    :category file
    :face consult-file
    :items ,(thread-last recentf-list
              (mapcar #'file-name-directory)
              (delete-dups)
              (mapcar #'abbreviate-file-name)))
  "Recentf directory source for `consult--switch-directory'.")

(defvar consult-switch-bookmark-dirs
  `(:name "(b)ookmarks"
          :narrow ?b
          :category bookmark
          :face consult-file
          :history bookmark-history
          ;; :action ,#'bookmark-locate
          :items ,#'consult--directory-bookmarks
          :default t)
  "Bookmark directory source for `consult--switch-directory'.")

(defvar consult-switch-current-dirs
  `(:name "(.)this directory/project"
         :narrow ?.
         :category file
         :face consult-file
         ;; :action ,(lambda (this-dir &optional norecord)
         ;;            (insert (abbreviate-file-name default-directory)))
         :items ,(let ((dir (abbreviate-file-name default-directory))
                       (proj (project-current)))
                   (cond ((and proj (equal dir (project-root proj))) (list dir))
                         (proj (list dir (project-root proj)))
                         (t (list dir)))))
  "Current project directory for `consult--switch-directory'.")

(defvar consult-switch-project-dirs
  `(:name "(p)rojects"
         :narrow ?p
         :category file
         :face consult-file
         :history nil
         ;; :action ,(lambda (project-dir &optional norecord)
         ;;            (insert project-dir))
         :items ,(lambda () (mapcar 'car project--list)))
  "Project directory source for `consult--switch-directory'.")

(defun consult--switch-directory (&optional prompt dir)
  "Return a directory chosen from bookmarks and projects."
  (or dir
      (let ((match (or dir
                       (consult--multi consult-switch-directory-sources
                        :prompt (or prompt "Switch directory: ")
                        :sort nil))))
        (pcase (plist-get (cdr match) :category)
          ('bookmark (bookmark-get-filename (car match)))
          ('file (car match))))))

;;;###autoload
(defun find-file-in-directory (dir &optional wildcards)
  (interactive (list (consult--switch-directory "In directory: ")))
  (let ((default-directory dir))
    (call-interactively #'find-file)))
  
;;;###autoload
(defun consult-switch-find-from-directory (&optional arg)
  "Jump to file from the current minibuffer directory."
  (interactive "P")
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
(defun consult-switch-directory ()
    "Choose a directory from bookmarks and projects with
completion and insert it at point."
    (interactive)
    (let* ((file-name (file-name-nondirectory (minibuffer-contents)))
           (new-dir (consult--switch-directory))
           (new-full-name (concat (file-name-as-directory new-dir)
                                  file-name)))
      (when new-dir
        (if consult-switch-shadow-filenames
            (insert (concat "/" new-full-name))
          (delete-minibuffer-contents)
          (insert new-full-name)))))

(provide 'consult-switch)
