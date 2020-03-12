(load "~/.emacs.d/my-loadpackages.el")

;;; rebind M-3 to #
;;; on Mac OS, this is best achieved through rebinding the character
;;; this may be desirable when virtualising linux on a mac, so keeping here
;;; (global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

;;; disabled splash screen i.e. show scratch pad on startup
(setq inhibit-startup-screen t)

;;; clear scratch buffer
(setq initial-scratch-message nil)

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

;;; customized
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(org-sticky-header-full-path (quote full))
 '(package-selected-packages
   (quote
    (org-sticky-header engine-mode haskell-mode yasnippet linum-relative neotree request xah-math-input smex psgml psci psc-ide markdown-mode magit idris-mode))))
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

;;; when selecting text, write over
(delete-selection-mode t)

;;; instead of asking 'yes/no?', just ask 'y/n?'
(fset 'yes-or-no-p 'y-or-n-p)

;;; when opening from finder, keep the same window
(setq ns-pop-up-frames nil)

(defun sensible-defaults/comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if
there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

;;; toggle comments with M-;
(global-set-key (kbd "M-;")
		'sensible-defaults/comment-or-uncomment-region-or-line)

;;; different symbol for org-mode's collapsed headings
(setq org-ellipsis "⤵")

;;; org-mode 
(add-hook 'org-mode-hook '(lambda ()
			    ;;; line wrap
			    (visual-line-mode)
			    (org-indent-mode)
	                    ;;; sticky header
	                    (org-sticky-header-mode)
	  ))

;;; always enable relative line numbers
(linum-relative-toggle)

;;; haskell-mode
(set-face-attribute 'haskell-constructor-face nil :foreground "#ffff00")

;;; cleanup block python comments
; each line is prepended with a # and then 2 spaces
(defun clean (start end)
  (interactive "r")
  (setq comment (replace-regexp-in-string "\n?#\\W\\W" "" (buffer-substring-no-properties start end)))
  (setq words (split-string comment " "))
  (setq len (number-to-string (length words)))
  (setq line-length 2)
  (setq output '())
  (while (> (length words) 0)
    (setq word (car words))
         (if (> (+ line-length (length word)) 80)
	     (progn
	       (setq output (append output (list "\n# ")))
	       (setq line-length 2)))
	 (setq output (append output (list word)))
	 (setq line-length (+ 1 (+ line-length (length word))))
	 (setq words (cdr words)))
  (delete-region start end)
  (insert (mapconcat (function (lambda (x) x)) (cons "# " output) " ")))

;;; engine-mode
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "firefox")

(defengine ddg
  "https://duckduckgo.com/?q=%s")

;;; selection region style
(set-face-attribute 'region nil :background nil :foreground "red")
