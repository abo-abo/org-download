# org-download

[![MELPA](https://melpa.org/packages/org-download-badge.svg)](https://melpa.org/#/org-download)
[![MELPA Stable](https://stable.melpa.org/packages/org-download-badge.svg)](https://stable.melpa.org/#/org-download)

This extension facilitates moving images from point **A** to point **B**.

Point **A** (*the source*) can be:

1. An image inside your browser that you can drag to Emacs.
2. An image on your file system that you can drag to Emacs.
3. A local or remote image address in kill-ring.
   Use the `org-download-yank` command for this.
   Remember that you can use "0 w" in `dired` to get an address.
4. A screenshot taken using `gnome-screenshot`, `scrot`, `gm`, `xclip`
   (on Linux), `screencapture` (on OS X) or , `imagemagick/convert`
   (on Windows).  Use the `org-download-screenshot` command for this.
   Customize the backend with `org-download-screenshot-method`.

Point **B** (*the target*) is an Emacs `org-mode` buffer where the inline
link will be inserted.  Several customization options will determine
where exactly on the file system the file will be stored.

They are:
`org-download-method`:

1. 'attach => use `org-mode` attachment machinery
2. 'directory => construct the directory in two stages:
   1. first part of the folder name is:
      * either "." (current folder)
      * or `org-download-image-dir` (if it's not nil).

        `org-download-image-dir` becomes buffer-local when set,
        so each file can customize this value, e.g with:

                -*- mode: Org; org-download-image-dir: "~/Pictures/foo"; -*-

        To set it for all files at once, use this:

                (setq-default org-download-image-dir "~/Pictures/foo")


   2. second part is:
      * `org-download-heading-lvl` is nil => ""
      * `org-download-heading-lvl` is n => the name of current
        heading with level n.

        Level count starts with 0,
        i.e. * is 0, ** is 1, *** is 2 etc.
        `org-download-heading-lvl` becomes buffer-local when set,
        so each file can customize this value, e.g with:

                -*- mode: Org; org-download-heading-lvl: nil; -*-

`org-download-timestamp`:
optionally add a timestamp to the file name.

Customize `org-download-backend` to choose between `url-retrieve`
(the default) or `wget` or `curl`.

if you don't want to show the `#+DOWNLOAD` anonotate, use this:
```
(setq org-download-annotate-function (lambda (_link) ""))
```

## Set up

```elisp
(require 'org-download)

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)
```

## Pasting from the clipboard
If you have the image stored in the clipboard, use `org-download-clipboard`.

here is an example. shortcut `C-M-y` will have the image stored from the clipboard in the path `images/$currentfilename/%Y%m%d-%H%M%S_$imagename.png` 

```
(defun zz/org-download-paste-clipboard (&optional use-default-filename)
  (interactive "P")
  (require 'org-download)
  (setq org-download-image-dir (concat "images/" (file-name-sans-extension (buffer-name))))
  (let ((file
         (if (not use-default-filename)
             (read-string (format "Filename [%s]: " org-download-screenshot-basename)
                          nil nil org-download-screenshot-basename)
           nil)))
(org-download-clipboard file)))

(after! org
  (setq org-download-method 'directory)
  (setq org-download-image-dir  "images/")
  (setq org-download-heading-lvl nil)
  (setq org-download-timestamp "%Y%m%d-%H%M%S_")
  (setq org-image-actual-width nil)
  (setq org-src-window-setup 'split-window-right)
  (setq org-download-annotate-function (lambda (_link) ""))
(map! :map org-mode-map
        "C-M-y" #'zz/org-download-paste-clipboard ))
```