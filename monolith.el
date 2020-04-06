;;; monolith.el --- a web page saver for Emacs using monolith.

;; Copyright (C) 2020 zbelial

;; Author:     zbelial
;; Maintainer: zbelial
;; Created on: February 03, 2020
;; Keywords:   html, webpage, monolith
;; Package-Requires: ((s "1.7.0") (f "0.20.0"))

;; This file is not (yet) part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Description:
;;
;; Requirement:
;; 
;; GNU Emacs 25 or higher.
;; 
;; Installation:
;;
;; Insert the following line to your .emacs:
;;
;;  (require 'monolith)
;; 
;; Keybindings:
;;
;;
;; TODO
;; 1. save original URL
;;
;;
;; User Commands:
;; 
;; * `monolith-save-page'  - Save a url as a single html file to disk.
;; * `monolith-save-and-insert-into-org' - Save a url to disk and insert a heading to a org buffer.
;; * `monolith-insert-existing-file-into-org' - Save a url to disk and insert a heading to a org buffer.

;;; Code:

(require 's)
(require 'f)

(defgroup monolith nil
  "Save complete webpage as a single html."
  :prefix "monolith-"
  :group 'tools)

(defcustom monolith-executable (executable-find "monolith")
  "'monolith' executable."
  :type 'string
  :group 'monolith)

(defcustom monolith-directory "~/.monolith"
  "Directory under which all web pages are, and will be, saved."
  :type  'string
  :group 'monolith)

(defcustom monolith-org-link-type 'relative
  "."
  :type '(choice (const :tag "Relative" relative)
                 (const :tag "Absolute" absolute))
  :group 'monolith)

(defvar original-url "")

(defun monolith-executable ()
  "Return the 'monolith' executable.
Raises an error if it can not be found."
  (unless monolith-executable
    (error "No 'monolith' executable found"))
  (shell-quote-argument monolith-executable))

(defun monolith--get-first-url ()
  "Return URL in clipboard, or first URL in the `kill-ring', or nil if none."
  (cl-loop for item in (append (list (gui-get-selection 'CLIPBOARD))
                               kill-ring)
           when (and item (string-match (rx bol "http" (optional "s") "://") item))
           return item))

(defun monolith--output-file-name ()
  "Read the output file name."
  (setq output (read-from-minibuffer (concat "Output file name (in " monolith-directory "): " )))
  (if (= (length output) 0)
      (error "Output file name is empty"))
  (if (not (s-suffix? ".html" output))
      (setq output (concat output ".html")))
  (concat (s-chop-suffix "/" monolith-directory) "/" output)
  )

(defun monolith-save-page ()
  "Save url to a file and return the filename if saved successfully."
  (interactive)
  (if (not (file-exists-p monolith-directory))
      (make-directory monolith-directory))
  
  (if (not (file-directory-p monolith-directory))
      (error (format "%s is not a directory" monolith-directory)))
  
  (setq original-url "")
  (setq default-url (monolith--get-first-url))
  (setq url (read-from-minibuffer "Enter the url: " default-url))
  (if (= (length url) 0)
      (error "url is empty"))
  (setq original-url url)

  (setq output (monolith--output-file-name))

  (setq cmd (concat (monolith-executable) " -o " (shell-quote-argument (expand-file-name output)) " '" (url-encode-url url) "'"))
  (shell-command cmd)

  (if (not (file-exists-p output))
      (error (format "cmd %S failed" cmd))
    output)
  )

(defun monolith--relative-file (url-filename)
  (let* ((common-prefix (f-common-parent (list buffer-file-name url-filename)))
         (buffer-filename-suffix (s-chop-prefix common-prefix buffer-file-name))
         (url-filename-suffix (s-chop-prefix common-prefix url-filename))
         (parts-count (length (f-split buffer-filename-suffix)))
         (result url-filename-suffix)
         )
    (dotimes (__unused (- parts-count 1) result)
      (setq result (concat "../" result)))
    )
  )

(defun monolith--cleanup-title (title)
  "Return TITLE with spurious whitespace removed."
  (->> title
       (s-replace "\n" " ")
       (s-trim)
       (s-collapse-whitespace)))


(defun monolith--url-title (url-filename)
  "Return title of HTML page, or nil if it has none. Uses the `dom' library."
  (let* ((dom (with-temp-buffer
                (insert-file url-filename)
                (libxml-parse-html-region (point-min) (point-max))))
         (title (cl-caddr (car (dom-by-tag dom 'title)))))
    (when title
      (monolith--cleanup-title title))))


(defun monolith--get-url-title (url-filename)
  "Return title of the url(using saved file)."
  (let ((title (monolith--url-title url-filename))
        )
    (if title
        title
      (f-filename url-filename)))
  )

(defun monolith--insert-into-org (link desc)
  "Insert a new heading and a link to the current org buffer."
  (beginning-of-line)
  ;; (org-insert-heading-respect-content)
  (org-insert-heading-after-current)
  (insert desc)
  (newline-and-indent)
  (org-set-property "URL" original-url)
  (org-set-property "DATE" (format-time-string "<%Y-%m-%d %H:%M:%S  %A>"))
  (insert (concat "[[file:" link "][" desc "]]"))
  )

(defun monolith-insert-existing-file-into-org ()
  "Insert a new heading and a link of an existing saved file to the current org buffer."
  (interactive)

  (unless (eq major-mode 'org-mode)
    (error "Current buffer is non-org-mode buffer"))

  (setq filename (expand-file-name (read-file-name "Existing file: " monolith-directory)))
  (if (not (f-file? filename))
      (error (format "%s is not a regular file" filename)))

  (setq original-url "")
  (let* ((desc (monolith--get-url-title filename))
         (linked-file-name "")
         )
    (if (eq monolith-org-link-type 'relative)
        (setq linked-file-name (monolith--relative-file filename))
      (setq linked-file-name filename)
      )
    (monolith--insert-into-org linked-file-name desc))
  )

(defun monolith-save-and-insert-into-org ()
  "Save url to a file and insert into current (orgmode) buffer as a link if saved successfully."
  (interactive)

  (unless (eq major-mode 'org-mode)
    (error "Current buffer is non-org-mode buffer"))

  (setq original-url "")
  (let* ((url-output (monolith-save-page))
         (desc (monolith--get-url-title url-output))
         (linked-file-name "")
         )
    (if (eq monolith-org-link-type 'relative)
        (setq linked-file-name (monolith--relative-file (expand-file-name url-output)))
      (setq linked-file-name url-output)
      )
    (monolith--insert-into-org linked-file-name desc))
  
  )

(provide 'monolith)
;;; monolith.el ends here
