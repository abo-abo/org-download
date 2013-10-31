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
;; The folder is constructed in two stages:
;; * first part of the folder name is:
;;   either "." (current folder)
;;   or `org-download-image-dir' (if it's not nil).
;; * second part is:
;;   either "" (nothing) when `org-download-heading-lvl' is nil
;;   or the name of current heading with level `org-download-heading-lvl'
;;   Level count starts with 0, i.e. * is 0, ** is 1, *** is 2 etc.
;;   `org-download-heading-lvl' becomes buffer-local when set, so each
;;   file can customize this value.
;;
;;; Code:


(eval-when-compile
  (require 'cl))
(require 'url-parse)
(require 'url-http)

(defgroup org-download nil
  "Image drag-and-drop for org-mode."
  :group 'org
  :prefix "org-download-")

(defcustom org-download-method 'directory
  "The way images should be stored."
  :type '(choice
          (const :tag "Directory" directory)
          (const :tag "Attachment" attach))
  :group 'org-download)

(defcustom org-download-image-dir nil
  "If set, images will be stored in this directory instead of \".\".
See `org-download--dir-1' for more info."
  :type '(choice
          (const :tag "Default" nil)
          (string :tag "Directory"))
  :group 'org-download)
(make-variable-buffer-local 'org-download-image-dir)

(defvar org-download-heading-lvl 0
  "Heading level to be used in `org-download--dir-2'.")
(make-variable-buffer-local 'org-download-heading-lvl)

(defcustom org-download-backend t
  "Method to use for downloading."
  :type '(choice
          (const :tag "wget" "wget \"%s\" -O \"%s\"")
          (const :tag "curl" "curl \"%s\" -o \"%s\"")
          (const :tag "url-retrieve" t))
  :group 'org-download)

(defcustom org-download-timestamp "_%Y-%m-%d_%H:%M:%S"
  "This `format-time-string'-style string will be appended to the file name.
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

(defun org-download--dir-1 ()
  "Return the first part of the directory path for `org-download--dir'.
It's `org-download-image-dir', unless it's nil.  Then it's \".\"."
  (or org-download-image-dir "."))

(defun org-download--dir-2 ()
  "Return the second part of the directory path for `org-download--dir'.
Unless `org-download-heading-lvl' is nil, it's the name of the current
`org-download-heading-lvl'-leveled heading.  Otherwise it's \"\"."
  (and org-download-heading-lvl
       (org-download-get-heading
        org-download-heading-lvl)))

(defun org-download--dir ()
  "Return the directory path for image storage.

The path is composed from `org-download--dir-1' and `org-download--dir-2'.
The directory is created if it didn't exist before."
  (let* ((part1 (org-download--dir-1))
         (part2 (org-download--dir-2))
         (dir (if part2
                  (format "%s/%s" part1 part2)
                part1)))
    (unless (file-exists-p dir)
      (make-directory dir t))
    dir))

(defun org-download--fullname (link)
  "Return the file name where LINK will be saved to.

It's affected by `org-download-timestamp' and `org-download--dir'."
  (let ((filename
         (file-name-nondirectory
          (car (url-path-and-query
                (url-generic-parse-url link)))))
        (dir (org-download--dir)))
    (format "%s/%s%s.%s"
            dir
            (file-name-sans-extension filename)
            (format-time-string org-download-timestamp)
            (file-name-extension filename))))

(defun org-download--image (link filename)
  "Save LINK to FILENAME asynchronously and show inline images in current buffer."
  (cond ((string-match "^file://\\(.*\\)" link)
         (org-download--image/command
          "cp \"%s\" \"%s\""
          (url-unhex-string
           (match-string 1 link))
          filename))
        ((eq org-download-backend t)
         (org-download--image/url-retrieve link filename))
        (t (org-download--image/command org-download-backend link filename))))

(defun org-download--image/command (command link filename)
  "Using COMMAND, save LINK to FILENAME.
COMMAND is a format-style string with two slots for LINK and FILENAME."
  (require 'async)
  (async-start
   `(lambda() (shell-command
          ,(format command link
                   (expand-file-name filename))))
   (lexical-let ((cur-buf (current-buffer)))
     (lambda(x)
       (with-current-buffer cur-buf
         (org-display-inline-images))))))

(defun org-download--image/url-retrieve (link filename)
  "Save LINK to FILENAME using `url-retrieve'."
  (url-retrieve
   link
   (lambda (status filename buffer)
     ;; Write current buffer to FILENAME
     ;; and update inline images in BUFFER
     (let ((err (plist-get status :error)))
       (if err (error
                "\"%s\" %s" link
                (downcase (nth 2 (assq (nth 2 err) url-http-codes))))))
     (delete-region
      (point-min)
      (progn
        (re-search-forward "\n\n" nil 'move)
        (point)))
     (let ((coding-system-for-write 'no-conversion))
       (write-region nil nil filename nil nil nil 'confirm))
     (with-current-buffer buffer
       (org-display-inline-images)))
   (list
    (expand-file-name filename)
    (current-buffer))
   nil t))

(defun org-download-yank ()
  "Call `org-download-image' with current kill."
  (interactive)
  (org-download-image (current-kill 0)))

(defun org-download-image (link)
  "Save image at address LINK to `org-download--dir'."
  (interactive "sUrl: ")
  (let ((filename
         (if (eq org-download-method 'attach)
             (let (org-download-image-dir
                   org-download-heading-lvl)
               (org-download--fullname link))
           (org-download--fullname link))))
    (if (null (image-type-from-file-name filename))
        (message "not an image URL")
      (org-download--image link filename)
      (when (eq org-download-method 'attach)
        (require 'org-attach)
        (org-attach-attach filename nil 'mv)
        (let* ((attach-dir (org-attach-dir t)))
          (setq filename
                (expand-file-name
                 (file-name-nondirectory filename) attach-dir))))
      (if (looking-back "^[ \t]+")
          (delete-region (match-beginning 0) (match-end 0))
        (newline))
      (insert (format "#+DOWNLOADED: %s @ %s\n [[%s]]"
                      link
                      (format-time-string "%Y-%m-%d %H:%M:%S")
                      filename))
      (org-display-inline-images))))

(defun org-download--at-comment-p ()
  "Check if current line begins with #+DOWLOADED:."
  (save-excursion
    (move-beginning-of-line nil)
    (looking-at "#\\+DOWNLOADED:")))

(defun org-download-delete ()
  "Delete inline image link on current line, and the file that it points to."
  (interactive)
  (cond ((org-download--at-comment-p)
         (delete-region (line-beginning-position)
                        (line-end-position))
         (org-download--delete (line-beginning-position)
                               nil
                               1))
        ((region-active-p)
         (org-download--delete (region-beginning)
                               (region-end))
         (delete-region (region-beginning)
                        (region-end)))

        (t (org-download--delete (line-beginning-position)
                              (line-end-position)))))

(defun org-download--delete (beg end &optional times)
  "Delete inline image links and the files they point to between BEG and END.

When TIMES isn't nil, delete only TIMES links."
  (unless times
    (setq times most-positive-fixnum))
  (save-excursion
    (goto-char beg)
    (while (and (>= (decf times) 0)
                (re-search-forward "\\[\\[\\([^]]*\\)\\]\\]" end t))
      (let ((str (match-string-no-properties 1)))
        (delete-region (match-beginning 0)
                       (match-end 0))
        (when (file-exists-p str)
          (delete-file str))))))

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
