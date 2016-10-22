;;; cpinfo.el --- Copy file, recording information  -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Kyle Meyer

;; Author: Kyle Meyer <kyle@kyleam.com>
;; URL: https://gitlab.com/kyleam/cpinfo
;; Keywords: files
;; Version: 0.1.0

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

;; This library provides a command, `cpinfo-copy', that copies a file
;; to a new directory and records information about the source of the
;; file.  The information is stored in a file, specified by
;; `cpinfo-pathfile', which is kept in the same directory as the
;; destination file.  By default, the information consists of the Git
;; hash for the repository in which the source file resides.  The
;; variable `cpinfo-info-functions' can be used to customize what
;; information is saved.


;;; Code:

(require 'cl-lib)

(defgroup cpinfo nil
  "Copy file, recording source information."
  :group 'files)

(defcustom cpinfo-root nil
  "Root directory to use when reading destination directory."
  :type '(choice (const :tag "Use `default-directory'" nil)
                 (directory :tag "Root directory")))

(defcustom cpinfo-pathfile "cpinfo-paths.el"
  "Base name of file used to store source information."
  :type 'string)

(defcustom cpinfo-info-functions '(cpinfo-git-hash)
  "Functions used to retrieve information about source file.
These functions will be called with the full path of the source
file."
  :type '(choice function))

(defcustom cpinfo-skip-writeprotected t
  "If non-nil, skip write-protected files in destination directory."
  :type 'boolean)

(defun cpinfo-read-paths-file (directory)
  (let ((path-file (expand-file-name cpinfo-pathfile directory)))
    (when (file-exists-p path-file)
      (with-temp-buffer
        (insert-file-contents path-file)
        (read (current-buffer))))))

(defun cpinfo-write-paths-file (directory value)
  (let ((fpaths (mapcar
                 (lambda (v)
                   (let ((fname (car v)))
                     (cons (file-name-nondirectory fname) fname)))
                 value)))
    (with-temp-file (expand-file-name cpinfo-pathfile directory)
      (let ((print-length nil))
        (print (mapcar
                (lambda (k) (assoc (cdr (assoc k fpaths)) value))
                (sort (delete-dups
                       ;; Map from base name rather than full path to
                       ;; catch any changes in the source directory.
                       (mapcar #'car fpaths))
                      #'string-lessp))
               (current-buffer)))
      (goto-char (point-min))
      (while (re-search-forward ") " nil t)
        (unless (nth 3 (syntax-ppss))
          (replace-match ")\n "))))))

(defvar cpinfo-last-destination nil)

(defun cpinfo-read-destination ()
  (setq cpinfo-last-destination
        (expand-file-name
         (read-directory-name
          (concat "Directory"
                  (and cpinfo-last-destination
                       (format " (default: %s)"
                               (abbreviate-file-name
                                cpinfo-last-destination)))
                  ": ")
          cpinfo-root
          cpinfo-last-destination
          t))))

(defvar cpinfo-destination-directory nil)

;;;###autoload
(defun cpinfo-set-destination (&optional unset)
  "Set destination.
This will be used instead of prompting.  If UNSET is non-nil,
unset destination instead."
  (interactive "P")
  (setq cpinfo-destination-directory (and (not unset)
                                          (cpinfo-read-destination))))

(defun cpinfo-destination ()
  (or cpinfo-destination-directory
      (cpinfo-read-destination)))

(defun cpinfo-git-hash (source-file)
  (let ((default-directory (file-name-directory source-file)))
    (with-temp-buffer
      (when (= 0 (call-process
                  "git" nil t nil "rev-parse" "HEAD"))
        (buffer-substring (point-min) (1- (point-max)))))))

(declare-function dired-get-marked-files 'dired)

;;;###autoload
(defun cpinfo-copy (files directory)
  "Copy FILES to DIRECTORY.
Record source information in `cpinfo-pathfile'."
  (interactive
   (list (if (derived-mode-p 'dired-mode)
             (dired-get-marked-files nil current-prefix-arg)
           (list (read-file-name "File to copy: " nil nil t)))
         (cpinfo-destination)))
  (let ((dirpaths (cpinfo-read-paths-file directory))
        (ncopied 0))
    (dolist (f files)
      (let* ((new-file (expand-file-name (file-name-nondirectory f) directory))
             (protected-p (not (file-writable-p new-file))))
        (if (and protected-p cpinfo-skip-writeprotected)
            (message "Skipping write-protected file: %s" new-file))
        (when protected-p
          (delete-file new-file))
        (copy-file f new-file t)
        (cl-incf ncopied)
        (push
         (cons f (delq nil (mapcar (lambda (func) (funcall func f))
                                   cpinfo-info-functions)))
         dirpaths)))
    (when (> ncopied 0)
      (message "Copied %s file(s) to %s" ncopied directory)
      (cpinfo-write-paths-file directory dirpaths))))

;;;###autoload
(defun cpinfo-update-paths-file (directory)
  "Delete `cpinfo-pathfile' entries that do not exist in DIRECTORY."
  (interactive (list (cpinfo-destination)))
  (cpinfo-write-paths-file
   directory
   (cl-remove-if-not
    (lambda (x) (file-exists-p
                 (expand-file-name (file-name-nondirectory (car x))
                                   directory)))
    (cpinfo-read-paths-file directory))))

;;;###autoload
(defun cpinfo-recopy (directory)
  "Recopy files listed in DIRECTORY's `cpinfo-pathfile'."
  (interactive (list (cpinfo-destination)))
  (cpinfo-copy
   (cl-remove-if-not
    (lambda (f)
      (and (file-exists-p f)
           (file-writable-p (expand-file-name
                             (file-name-nondirectory f)
                             directory))))
    (mapcar #'car (cpinfo-read-paths-file directory)))
   directory))

(provide 'cpinfo)
;;; cpinfo.el ends here
