;;; org-download.el --- Image drag-and-drop for Emacs org-mode

;; Copyright (C) 2013  Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/org-download
;; Version: 0.1

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This extension enables dragging an image from a browser to
;; an org-mode buffer in Emacs.
;; The image will be downloaded to an appropriate folder and a link
;; to it will be inserted at point.
;;
;;; Code:

(require 'async)
(eval-when-compile
  (require 'cl))

(defgroup org-download nil
  "Image drag-and-drop for org-mode."
  :group 'org
  :prefix "org-download-")

(defcustom org-download-image-dir nil
  "If not nil, `org-download-image' will store images here."
  :type 'string
  :group 'org-download)

(defvar org-download--backend-cmd nil
  "Backend command for downloading.

Do not set this directly.  Customize `org-download-backend' instead.")

(defcustom org-download-backend 'wget
  "Set this to 'wget or 'curl."
  :set (lambda (symbol value)
         (case value
           (wget (setq org-download--backend-cmd "wget \"%s\" -O \"%s\""))
           (curl (setq org-download--backend-cmd "curl \"%s\" -o \"%s\""))
           (t (error "Unsupported key: %s" value)))
         (set-default symbol value))
  :group 'org-download)

(defcustom org-download-timestamp "_%Y-%m-%d_%H:%M:%S"
  "This will be substituted into `format-time-string' and appended to the file name.
Set this to \"\" if you don't want time stamps."
  :type 'string
  :group 'org-download)

(defun org-download-get-heading (lvl)
  "Return the heading of the current entry's LVL level parent."
  (save-excursion
    (let ((cur-lvl (org-current-level)))
      (unless (= cur-lvl 1)
        (org-up-heading-all (- (1- (org-current-level)) lvl)))
      (substring-no-properties
       (org-get-heading)))))

(defun org-download--dir ()
  "Return the directory where the images will be saved to."
  (let ((dir (expand-file-name
              (or org-download-image-dir
                  (format
                   "./%s"
                   (org-download-get-heading 0))))))
    (unless (file-exists-p dir)
      (make-directory dir t))
    dir))

(defun org-download--fullname (link)
  "Return the file name where LINK will be saved to.

It's affected by `org-download-timestamp' and `org-download-image-dir'
custom variables."
  (let ((filename (car (last (split-string link "/"))))
        (dir (org-download--dir)))
    (format "%s/%s%s.%s"
            dir
            (file-name-sans-extension filename)
            (format-time-string org-download-timestamp)
            (file-name-extension filename))))

(defun org-download--image (link to)
  "Save LINK to TO using wget."
  (async-start
   `(lambda() (shell-command
          ,(format org-download--backend-cmd link to)))
   (lexical-let ((cur-buf (current-buffer)))
     (lambda(x)
       (with-current-buffer cur-buf
         (org-display-inline-images))))))

(defun org-download-image (link)
  "Save image at address LINK to current directory's sub-directory DIR.
DIR is the name of the current level 0 heading."
  (interactive (list (current-kill 0)))
  (let ((filename (org-download--fullname link)))
    (if (null (image-type-from-file-name filename))
        (message "not an image URL")
      (unless (file-exists-p filename)
        (org-download--image link filename))
      (if (looking-back "^[ \t]+")
          (delete-region (match-beginning 0) (match-end 0))
        (newline))
      (insert (format "#+DOWNLOADED: %s @ %s\n [[%s]]"
                      link
                      (format-time-string "%Y-%m-%d %H:%M:%S")
                      filename))
      (org-display-inline-images))))

(defun org-download-dnd (uri action)
  (org-download-image uri))

(defun org-download-enable ()
  "Enable org-download."
  (unless (eq (cdr (assoc "^\\(https?\\|ftp\\|file\\|nfs\\)://" dnd-protocol-alist))
              'org-download-dnd)
    (setq dnd-protocol-alist
          `(("^\\(https?\\|ftp\\|file\\|nfs\\)://" . org-download-dnd) ,@dnd-protocol-alist))))

(defun org-download-disable ()
  "Disable org-download."
  (rassq-delete-all 'org-download-dnd dnd-protocol-alist))

(org-download-enable)
(provide 'org-download)
;;; org-download.el ends here
