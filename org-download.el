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
  "Set this to `wget' or `curl' or `url-retrieve'"
  :set (lambda (symbol value)
         (case value
           (wget (setq org-download--backend-cmd "wget \"%s\" -O \"%s\""))
           (curl (setq org-download--backend-cmd "curl \"%s\" -o \"%s\""))
           (url-retrieve t)
           (t (error "Unsupported key: %s" value)))
         (set-default symbol value))
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

(defun org-download--image (link filename)
  "Save LINK to FILENAME asynchronously and show inline images in current buffer."
  (if (eq org-download-backend 'url-retrieve)
      (url-retrieve
       link
       (lambda (status filename buffer)
         "Write current buffer to FILENAME and update inline images in BUFFER"
         (let ((err (plist-get status :error)))
           (if err
               (signal :error (cdr err))))
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
        filename
        (current-buffer))
       nil t)
    (require 'async)
    (async-start
     `(lambda() (shell-command
                 ,(format org-download--backend-cmd link filename)))
     (lexical-let ((cur-buf (current-buffer)))
       (lambda(x)
         (with-current-buffer cur-buf
           (org-display-inline-images)))))))

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
