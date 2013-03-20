(when
    (load
     (expand-file-name "~/.emacs.d/package.el"))
  (package-initialize))

(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

(require 'package)
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(starter-kit starter-kit-ruby starter-kit-js starter-kit-lisp starter-kit-bindings yaml-mode rvm rspec-mode rinari anything ruby-mode full-ack rvm)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))    (package-install p)))

(menu-bar-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; autosave on switching buffer
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-frame (before other-frame-now activate)
  (when buffer-file-name (save-buffer)))

;; Require YAML-Mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; ENTER key in Emacs does 'newline-and-indent'
(add-hook 'yaml-mode-hook
      '(lambda ()
         (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; Load Rinari
(require 'rinari )
(add-hook 'ruby-mode-hook 'rinari-minor-mode)

(blink-cursor-mode 1)


(require 'rspec-mode)

;; config for full-ack
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

;; add support for rhtml
(add-to-list 'load-path "~/Code/rhtml")
(require 'rhtml-mode)

;; indent the whole god-damn buffer
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; setting the size of the emacs GUI window properly
;; This is important because I have a netbook


(setq default-frame-alist
      '((top . 200) (left . 400)
        (width . 70) (height . 30)
))

(setq initial-frame-alist '((top . 10) (left . 30)))