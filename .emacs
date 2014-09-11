(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

(require 'package)
(setq package-archives
          '(("gnu"         . "http://elpa.gnu.org/packages/")
            ("original"    . "http://tromey.com/elpa/")
            ("org"         . "http://orgmode.org/elpa/")
            ("marmalade"   . "http://marmalade-repo.org/packages/")
            ("melpa"       . "http://melpa.milkbox.net/packages/")))
    (package-initialize)
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

;; line numbers globally
(global-linum-mode 1)
(menu-bar-mode 0)
(load-theme 'manoj-dark)
;; Load a nice font
;; Snippet taken from http://emacswiki.org/EmacsChannelFaq#toc5
;; Set font size to 11pt
(let ((11pt (round (* 11.1 10))))
  (set-face-attribute 'default (not 'this-frame-only)
                      :height 11pt))

;; Set font to Dejavu sans mono
(set-face-attribute 'default (not 'this-frame-only)
                    :font "Droid Sans Mono Slashed")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("2b5aa66b7d5be41b18cc67f3286ae664134b95ccc4a86c9339c886dfd736132d" "543976df2de12eb2ac235c79c7bc1dac6c58f4a34ae6f72237d6e70d8384f37a" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
