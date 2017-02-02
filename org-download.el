;;; org-download.el --- Image drag-and-drop for Emacs org-mode

;; Copyright (C) 2014 Free Software Foundation, Inc.

;; Author: Oleh Krehel
;; URL: https://github.com/abo-abo/org-download
;; Version: 0.1.0
;; Package-Requires: ((async "1.2"))
;; Keywords: images, screenshots, download

;; This file is not part of GNU Emacs.

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
;;
;; This extension facilitates moving images from point A to point B.
;;
;; Point A (the source) can be:
;; 1. An image inside your browser that you can drag to Emacs.
;; 2. An image on your file system that you can drag to Emacs.
;; 3. A local or remote image address in kill-ring.
;;    Use the `org-download-yank' command for this.
;;    Remember that you can use "0 w" in `dired' to get an address.
;; 4. An screenshot taken using `gnome-screenshot' or `scrot' or `gm'.
;;    Use the `org-download-screenshot' command for this.
;;    Customize the backend with  `org-download-screenshot-method'.
;;
;; Point B (the target) is an Emacs `org-mode' buffer where the inline
;; link will be inserted.  Several customization options will determine
;; where exactly on the file system the file will be stored.
;;
;; They are:
;; `org-download-method':
;; a. 'attach => use `org-mode' attachment machinery
;; b. 'directory => construct the directory in two stages:
;;    1. first part of the folder name is:
;;       * either "." (current folder)
;;       * or `org-download-image-dir' (if it's not nil).
;;         `org-download-image-dir' becomes buffer-local when set,
;;         so each file can customize this value, e.g with:
;;         # -*- mode: Org; org-download-image-dir: "~/Pictures/foo"; -*-
;;    2. second part is:
;;       * `org-download-heading-lvl' is nil => ""
;;       * `org-download-heading-lvl' is n => the name of current
;;         heading with level n. Level count starts with 0,
;;         i.e. * is 0, ** is 1, *** is 2 etc.
;;         `org-download-heading-lvl' becomes buffer-local when set,
;;         so each file can customize this value, e.g with:
;;         # -*- mode: Org; org-download-heading-lvl: nil; -*-
;;
;; `org-download-timestamp':
;; optionally add a timestamp to the file name.
;;
;; Customize `org-download-backend' to choose between `url-retrieve'
;; (the default) or `wget' or `curl'.
;;
;;; Code:


(eval-when-compile
  (require 'cl))
(require 'async)
(require 'url-parse)
(require 'url-http)
(require 'org)

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

(defcustom org-download-heading-lvl 0
  "Heading level to be used in `org-download--dir-2'."
  :group 'org-download)
(make-variable-buffer-local 'org-download-heading-lvl)

(defcustom org-download-backend t
  "Method to use for downloading."
  :type '(choice
          (const :tag "wget" "wget \"%s\" -O \"%s\"")
          (const :tag "curl" "curl \"%s\" -o \"%s\"")
          (const :tag "url-retrieve" t))
  :group 'org-download)

(defcustom org-download-timestamp "_%Y-%m-%d_%H-%M-%S"
  "This `format-time-string'-style string will be appended to the file name.
Set this to \"\" if you don't want time stamps."
  :type 'string
  :group 'org-download)

(defcustom org-download-img-regex-list
  '("<img +src=\"" "<img +\\(class=\"[^\"]+\"\\)? *src=\"")
  "This regex is used to unalias links that look like images.
The html to which the links points will be searched for these
regexes, one by one, until one succeeds.  The found image address
will be used."
  :group 'org-download)

(defcustom org-download-screenshot-method "gnome-screenshot -a -f %s"
  "The tool to capture screenshots."
  :type '(choice
          (const :tag "gnome-screenshot" "gnome-screenshot -a -f %s")
          (const :tag "scrot" "scrot -s %s")
          (const :tag "gm" "gm import %s")
          ;; screenshot script in osx, -i standars for iterative,
          ;; press space key to toggle bettwen selection and
          ;; window/application mode.
          (const :tag "screencapture" "screencapture -i %s"))
  :group 'org-download)

(defcustom org-download-image-html-width 0
  "When non-zero add #+attr_html: :width tag to the image."
  :type 'integer
  :group 'org-download)

(defcustom org-download-image-latex-width 0
  "When non-zero add #+attr_latex: :width tag to the image."
  :type 'integer
  :group 'org-download)

(defun org-download-get-heading (lvl)
  "Return the heading of the current entry's LVL level parent."
  (save-excursion
    (let ((cur-lvl (org-current-level)))
      (if cur-lvl
          (progn
            (unless (= cur-lvl 1)
              (org-up-heading-all (- (1- (org-current-level)) lvl)))
            (nth 4 (org-heading-components)))
        ""))))

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

(defun org-download--fullname (link &optional ext)
  "Return the file name where LINK will be saved to.

It's affected by `org-download--dir'.
EXT can hold the file extension, in case LINK doesn't provide it."
  (let ((filename
         (file-name-nondirectory
          (car (url-path-and-query
                (url-generic-parse-url link)))))
        (dir (org-download--dir)))
    (when (string-match ".*?\\.\\(?:png\\|jpg\\)\\(.*\\)$" filename)
      (setq filename (replace-match "" nil nil filename 1)))
    (abbreviate-file-name
     (expand-file-name
      (org-download--fullname-format filename ext)
      dir))))

(defun org-download--fullname-format (filename &optional ext)
  "It's affected by `org-download-timestamp'.
EXT can hold the file extension, in case FILENAME doesn't provide it."
  (format "%s%s.%s"
          (file-name-sans-extension filename)
          (format-time-string org-download-timestamp)
          (or ext (file-name-extension filename))))

(defun org-download--image (link filename)
  "Save LINK to FILENAME asynchronously and show inline images in current buffer."
  (when (string= "file" (url-type (url-generic-parse-url link)))
    (setq link (url-unhex-string (url-filename (url-generic-parse-url link)))))
  (cond ((and (not (file-remote-p link))
              (file-exists-p link))
         (copy-file link (expand-file-name filename)))
        ((eq org-download-backend t)
         (org-download--image/url-retrieve link filename))
        (t
         (org-download--image/command org-download-backend link filename))))

(defun org-download--image/command (command link filename)
  "Using COMMAND, save LINK to FILENAME.
COMMAND is a format-style string with two slots for LINK and FILENAME."
  (async-start
   `(lambda () (shell-command
                ,(format command link
                         (expand-file-name filename))))
   (lexical-let ((cur-buf (current-buffer)))
     (lambda (x)
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

(defun org-download-screenshot ()
  "Capture screenshot and insert the resulting file.
The screenshot tool is determined by `org-download-screenshot-method'."
  (interactive)
  (let ((link "/tmp/screenshot.png"))
    (shell-command (format org-download-screenshot-method link))
    (org-download-image link)))

(declare-function org-attach-dir "org-attach")
(declare-function org-attach-attach "org-attach")
(declare-function org-attach-sync "org-attach")

(defun org-download-annotate-default (link)
  "Annotate LINK with the time of download."
  (format "#+DOWNLOADED: %s @ %s"
          link
          (format-time-string "%Y-%m-%d %H:%M:%S")))

(defvar org-download-annotate-function
  #'org-download-annotate-default
  "Function that takes LINK and returns a string.
It's inserted before the image link and is used to annotate it.")

(defvar org-download-link-format
  "[[file:%s]]"
  "Format of the file link to insert.")

(defun org-download-image (link)
  "Save image at address LINK to `org-download--dir'."
  (interactive "sUrl: ")
  (let (ext)
    (unless (image-type-from-file-name link)
      (with-current-buffer (url-retrieve-synchronously link t)
        (cond ((let ((regexes org-download-img-regex-list)
                     lnk)
                 (while (and (not lnk) regexes)
                   (goto-char (point-min))
                   (when (re-search-forward (pop regexes) nil t)
                     (backward-char)
                     (setq lnk (read (current-buffer)))))
                 (when lnk
                   (setq link lnk))))
              ((progn
                 (goto-char (point-min))
                 (when (re-search-forward "^Content-Type: image/\\(.*\\)$")
                   (setq ext (match-string 1)))))
              (t
               (error "link %s does not point to an image; unaliasing failed" link)))))
    (let ((filename
           (if (eq org-download-method 'attach)
               (let ((org-download-image-dir (progn (require 'org-attach)
                                                    (org-attach-dir t)))
                     org-download-heading-lvl)
                 (org-download--fullname link ext))
             (org-download--fullname link ext))))
      (when (image-type-from-file-name filename)
        (org-download--image link filename)
        (when (eq org-download-method 'attach)
          (org-attach-attach filename nil 'none))
        (org-download-insert-link link filename)))))

(defun org-download-insert-link (link filename)
  (if (looking-back "^[ \t]+" (line-beginning-position))
      (delete-region (match-beginning 0) (match-end 0))
    (newline))
  (insert
   (concat
    (funcall org-download-annotate-function link)
    "\n"
    (if (= org-download-image-html-width 0)
        ""
      (format "#+attr_html: :width %dpx\n" org-download-image-html-width))
    (if (= org-download-image-latex-width 0)
        ""
      (format "#+attr_latex: :width %dcm\n" org-download-image-latex-width))
    (format org-download-link-format (file-relative-name filename (file-name-directory (buffer-name))))))
  (org-display-inline-images))

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
                                 (line-end-position))))
  (when (eq org-download-method 'attach)
    (org-attach-sync)))

(defcustom org-download-edit-cmd "gimp %s"
  "Command for editing an image link.")

(defun org-download-edit ()
  "Open the image at point for editing."
  (interactive)
  (let ((context (org-element-context)))
    (if (not (eq (car-safe context) 'link))
        (user-error "not on a link")
      (start-process-shell-command
       "org-download-edit"
       "org-download-edit"
       (format org-download-edit-cmd
               (shell-quote-wildcard-pattern
                (url-unhex-string (plist-get (cadr context) :path))))))))

(defun org-download--delete (beg end &optional times)
  "Delete inline image links and the files they point to between BEG and END.

When TIMES isn't nil, delete only TIMES links."
  (unless times
    (setq times most-positive-fixnum))
  (save-excursion
    (goto-char beg)
    (while (and (>= (decf times) 0)
                (re-search-forward "\\[\\[file:\\([^]]*\\)\\]\\]" end t))
      (let ((str (match-string-no-properties 1)))
        (delete-region beg
                       (match-end 0))
        (when (file-exists-p str)
          (delete-file str))))))

(defun org-download-dnd-fallback (uri action)
  (let ((dnd-protocol-alist
         (rassq-delete-all
          'org-download-dnd
          (copy-alist dnd-protocol-alist))))
    (dnd-handle-one-url nil action uri)))

(defun org-download-dnd (uri action)
  "When in `org-mode' and URI points to image, download it.
Otherwise, pass URI and ACTION back to dnd dispatch."
  (cond ((eq major-mode 'org-mode)
         (condition-case nil
             (org-download-image uri)
           (error
            (org-download-dnd-fallback uri action))))
        ((eq major-mode 'dired-mode)
         (org-download-dired uri))
        ;; redirect to someone else
        (t
         (org-download-dnd-fallback uri action))))

(defun org-download-dired (uri)
  "Download URI to current directory."
  (raise-frame)
  (let ((filename (file-name-nondirectory
                   (car (url-path-and-query
                         (url-generic-parse-url uri))))))
    (message "Downloading %s to %s ..."
             filename
             (expand-file-name filename))
    (url-retrieve
     uri
     (lambda (status filename)
       (let ((err (plist-get status :error)))
         (if err (error
                  "\"%s\" %s" uri
                  (downcase (nth 2 (assq (nth 2 err) url-http-codes))))))
       (let ((coding-system-for-write 'no-conversion))
         (write-region nil nil filename nil nil nil 'confirm)))
     (list
      (expand-file-name filename))
     t t)))

(defun org-download-dnd-base64 (uri action)
  (when (eq major-mode 'org-mode)
    (when (string-match "^data:image/png;base64," uri)
      (let* ((me (match-end 0))
             (filename (org-download--fullname
                        (substring-no-properties uri me (+ me 10))
                        "png")))
        (with-temp-buffer
          (insert (base64-decode-string (substring uri me)))
          (write-file filename))
        (org-download-insert-link filename filename)))))

;;;###autoload
(defun org-download-enable ()
  "Enable org-download."
  (unless (eq (cdr (assoc "^\\(https?\\|ftp\\|file\\|nfs\\):" dnd-protocol-alist))
              'org-download-dnd)
    (setq dnd-protocol-alist
          `(("^\\(https?\\|ftp\\|file\\|nfs\\):" . org-download-dnd)
            ("^data:" . org-download-dnd-base64)
            ,@dnd-protocol-alist))))

(defun org-download-disable ()
  "Disable org-download."
  (rassq-delete-all 'org-download-dnd dnd-protocol-alist))

(org-download-enable)

(provide 'org-download)

;;; org-download.el ends here
