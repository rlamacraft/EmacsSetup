(load "~/.emacs.d/my-loadpackages.el")

;;; treat right alt as mac standard key to enable alt-3 for #
(setq ns-right-alternate-modifier (quote none))

;;; disabled splash screen i.e. show scratch pad on startup
(setq inhibit-startup-screen t)

;;; disable the toolbar
(tool-bar-mode 0)

;;; set font
(set-default-font "Fira Code-13")

;;; theme
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (misterioso))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; relocate auto-saved files
(setq backup-directory-alist '(("." . "~/.emacs_saves")))

;;; enable ido-mode
(ido-mode 1)

;;; enable line numbers for all buffers
(global-linum-mode t)
(set-face-foreground 'linum "#6F7986")
