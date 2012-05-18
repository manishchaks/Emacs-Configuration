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
(defvar my-packages '(starter-kit starter-kit-ruby starter-kit-js starter-kit-lisp starter-kit-bindings yaml-mode rvm rspec-mode rinari anything ruby-mode full-ack)
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

;; ========= Set colours ==========

;; Set cursor and mouse-pointer colours
(set-cursor-color "white")
(set-mouse-color "goldenrod")

;; Set region background colour
;;(set-foreground-color "white")

;; Set emacs background colour
;;(set-background-color "black")


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

;; =============  Load a nice font ===============
;; ===== It is safe to comment out the lines below if you dont need
;; the fancy font. ========

;; Snippet taken from http://emacswiki.org/EmacsChannelFaq#toc5
;; Set font size to 13pt
(let ((13pt (round (* 13.1 10))))
  (set-face-attribute 'default (not 'this-frame-only)
                      :height 13pt))

;; Set font to Dejavu sans mono
(set-face-attribute 'default (not 'this-frame-only)
                    :font "Droid Sans Mono Slashed")
;; === End nice font section ==============
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(markdown-command "/opt/local/bin/markdown-2.6"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(global-hl-line-mode 1)
(blink-cursor-mode 1)