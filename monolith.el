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

(defcustom monolith-tmp-directory "/tmp/monolith"
  "Directory under which all web pages are saved temporarily before saved to `monolith-directory'."
  :type  'string
  :group 'monolith)

(defcustom monolith-org-link-type 'relative
  "."
  :type '(choice (const :tag "Relative" relative)
                 (const :tag "Absolute" absolute))
  :group 'monolith)

(defvar monolith--original-url "")

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
  (let* ((dir (expand-file-name (read-directory-name "Save into directory: " monolith-directory)))
         (output (read-from-minibuffer (concat "Output file name (in " dir "): " )))
         )
    (if (= (length output) 0)
        (error "Output file name is empty"))
    (if (not (s-suffix? ".html" output))
        (setq output (concat output ".html")))
    (concat (s-chop-suffix "/" dir) "/" output)
    ))

(defun monolith--tmp-output-file-name ()
  "Read the output file name."
  (concat (s-chop-suffix "/" monolith-tmp-directory) "/monolith.html")
  )


(defun monolith-save-page (&optional url)
  "Save url to a file and return the filename if saved successfully."
  (interactive)
  (if (not (file-exists-p monolith-directory))
      (make-directory monolith-directory))
  
  (if (not (file-directory-p monolith-directory))
      (error (format "%s is not a directory" monolith-directory)))
  
  (if (not (file-exists-p monolith-tmp-directory))
      (make-directory monolith-tmp-directory))
  
  (if (not (file-directory-p monolith-tmp-directory))
      (error (format "%s is not a directory" monolith-tmp-directory)))
  
  (let* ((default-url (or url (monolith--get-first-url)))
         (url (read-from-minibuffer "Enter the url: " default-url))
         (tmp-output (monolith--tmp-output-file-name))
         output
         output-dir
         title
         )
    (if (= (length url) 0)
        (error "url is empty"))
    (setq monolith--original-url url)

    (setq cmd (concat (monolith-executable) " -o " (shell-quote-argument (expand-file-name tmp-output)) " '" (url-encode-url url) "'"))
    (shell-command cmd)

    (if (not (file-exists-p tmp-output))
        (error (format "cmd %S failed" cmd))
      )

    (setq output "")
    (setq output-dir (expand-file-name (read-directory-name "Save into directory: " monolith-directory)))
    (setq title (monolith--url-title tmp-output))
    (if (not title)
        (setq title (read-from-minibuffer (concat "Output file name (in " output-dir "): " )))
      (setq title (read-from-minibuffer (concat "Output file name (in " output-dir "): " ) title))
      )  
    (setq output (concat (s-chop-suffix "/" output-dir) "/" title ".html"))
    (rename-file tmp-output output)

    (cons title output)
    )
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
  (org-set-property "URL" monolith--original-url)
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

  (setq monolith--original-url "")
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

  (setq monolith--original-url "")
  (let* ((result (monolith-save-page))
         (desc (car result))
         (filename (cdr result))
         (linked-file-name "")
         )
    (if (eq monolith-org-link-type 'relative)
        (setq linked-file-name (monolith--relative-file (expand-file-name filename)))
      (setq linked-file-name filename)
      )
    (monolith--insert-into-org linked-file-name desc))
  
  )

(provide 'monolith)
;;; monolith.el ends here
