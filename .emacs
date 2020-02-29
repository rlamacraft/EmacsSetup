(load "~/.emacs.d/my-loadpackages.el")

;;; rebind M-3 to #
;;; on Mac OS, this is best achieved through rebinding the character
;;; this may be desirable when virtualising linux on a mac, so keeping here
;;; (global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

;;; disabled splash screen i.e. show scratch pad on startup
(setq inhibit-startup-screen t)

;;; disable the toolbar
(tool-bar-mode 0)
(menu-bar-mode 0)

;;; set font
(set-default-font "Fira Code-13")

;;; relocate auto-saved files
(setq backup-directory-alist '(("." . "~/.emacs_saves")))

;;; enable ido-mode
(ido-mode 1)

;;; enable line numbers for all buffers
(global-linum-mode t)
(set-face-foreground 'linum "#6F7986")
(setq linum-format "%d ")

(require 'linum-relative)
(setq linum-relative-format "%3s ")

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

; (server-start)

;;; theme
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(package-selected-packages
   (quote
    (yasnippet linum-relative neotree request xah-math-input smex psgml psci psc-ide markdown-mode magit idris-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-highlight-datatype-face ((t (:foreground "brightcyan"))))
 '(agda2-highlight-function-face ((t (:foreground "brightblue"))))
 '(agda2-highlight-postulate-face ((t (:foreground "brightblue"))))
 '(agda2-highlight-record-face ((t (:foreground "brightblue"))))
 '(agda2-highlight-symbol-face ((t (:foreground "brightblack"))))
 '(font-lock-builtin-face ((t (:foreground "brightcyan"))))
 '(font-lock-comment-face ((t (:foreground "color-246"))))
 '(font-lock-function-name-face ((t (:foreground "brightblue")))))

;;; remove background colour
(defun set-background-for-terminal (&optional frame)
  (or frame (setq frame (selected-frame)))
  "unsets the background color in terminal mode"
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)))
(add-hook 'after-make-frame-functions 'set-background-for-terminal)
(add-hook 'window-setup-hook 'set-background-for-terminal)

;;; remove status bar colour
(set-face-attribute 'mode-line nil :background "unspecified-bg")

;;; neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;;;yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"
	))
(yas-global-mode 1)
