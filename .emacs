(load "~/.emacs.d/my-loadpackages.el")

;;; treat right alt as mac standard key to enable alt-3 for #
(setq ns-right-alternate-modifier 'none)

(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))
;;; (global-set-key (kbd "M-4") #'(lambda () (interactive) (end-of-buffer)))
(global-set-key (kbd "M-+") '(lambda () (interactive) (end-of-buffer)))

;;; disabled splash screen i.e. show scratch pad on startup
(setq inhibit-startup-screen t)

;;; disable the toolbar
(tool-bar-mode 0)
(menu-bar-mode 0)

;;; set font
(set-default-font "Fira Code-13")

;;; theme
;(custom-set-variables
; ;; custom-set-variables was added by Custom.
; ;; If you edit it by hand, you could mess it up, so be careful.
; ;; Your init file should contain only one such instance.
; ;; If there is more than one, they won't work right.
; '(ansi-color-faces-vector
;   [default default default italic underline success warning error])
; '(custom-enabled-themes (quote (misterioso)))
; '(package-selected-packages
;   (quote
;    (idris-mode xah-math-input smex psgml psci psc-ide markdown-mode magit))))
;(custom-set-faces
; ;; custom-set-faces was added by Custom.
; ;; If you edit it by hand, you could mess it up, so be careful.
; ;; Your init file should contain only one such instance.
; ;; If there is more than one, they won't work right.
; )

;;; remove background colour
;(defun set-background-for-terminal (&optional frame)
;  (or frame (setq frame (selected-frame)))
;  "unsets the background color in terminal mode"
;  (unless (display-graphic-p frame)
;    (set-face-background 'default "unspecified-bg" frame)))
;(add-hook 'after-make-frame-functions 'set-background-for-terminal)
;(add-hook 'window-setup-hook 'set-background-for-terminal)

;;; remove status bar colour
(set-face-attribute 'mode-line nil :background "unspecified-bg")

;;; relocate auto-saved files
(setq backup-directory-alist '(("." . "~/.emacs_saves")))

;;; enable ido-mode
(ido-mode 1)

;;; enable line numbers for all buffers
(global-linum-mode t)
(set-face-foreground 'linum "#6F7986")
(setq linum-format "%d ")

(define-key key-translation-map (kbd "<f5>") (kbd "#"))


(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

(server-start)
