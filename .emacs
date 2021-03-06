(load "~/.emacs.d/my-loadpackages.el")

;;; rebind M-3 to #
;;; on Mac OS, this is best achieved through rebinding the character
;;; this may be desirable when virtualising linux on a mac, so keeping here
;;; (global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

;;; disabled splash screen i.e. show scratch pad on startup
(setq inhibit-startup-screen t)

;;; clear scratch buffer
(setq initial-scratch-message nil)

;;; disable annoying bell
(setq ring-bell-function #'ignore)

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
 '(font-lock-function-name-face ((t (:foreground "brightblue"))))
 '(header-line ((t (:background "unspecified-by" :foreground "color-252"))))
 '(org-hide ((t (:foreground "unspecified-by"))))
 '(org-target ((t (:foreground "magenta" :underline t))))
 '(outline-1 ((t (:foreground "color-252" :weight normal))))
 '(outline-2 ((t (:foreground "color-252" :weight normal))))
 '(outline-3 ((t (:foreground "color-252" :weight normal))))
 '(outline-4 ((t (:foreground "color-252" :weight normal))))
 '(outline-5 ((t (:foreground "color-252" :weight normal))))
 '(outline-6 ((t (:foreground "color-252" :weight normal))))
 '(outline-7 ((t (:foreground "color-252" :weight normal))))
 '(outline-8 ((t (:foreground "color-252" :weight normal)))))

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

;;; org-mode 
(add-hook 'org-mode-hook '(lambda ()
			    ;;; line wrap
			    (visual-line-mode)
			    (org-indent-mode)
	                    ;;; sticky header
;	                    (org-sticky-header-mode)
			    ;;; disable linum mode
			    (linum-mode -1)
			    ))

;;; org-mode collapsing
(setq org-ellipsis "⤵")
(setq org-cycle-separator-lines -1)

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
(set-face-attribute 'region nil :background "#000")
(set-face-attribute 'region nil :foreground "#fff")

;;; functon for inserting the current date
(fset 'today
   "\C-u\C-[!date +\"%Y-%m-%d\"\C-m\C-[OB\C-?")

;;; org-sticky-mode customisation
(setq org-sticky-header-heading-star "")
(setq org-sticky-header-prefix nil)
(setq org-sticky-header-outline-path-separator " > ")

;;; org-mode key bindings
(global-set-key (kbd "C-c l") 'org-store-link)


;;; org-mode: hide angle brackets in "<<target>>" notation
;;; source: https://emacs.stackexchange.com/questions/19230/how-to-hide-targets
(defcustom org-hidden-links-additional-re "\\(<<\\)[[:print:]]+\\(>>\\)"
  "Regular expression that matches strings where the invisible-property is set to org-link."
  :type '(choice (const :tag "Off" nil) regexp)
  :group 'org-link)
(make-variable-buffer-local 'org-hidden-links-additional-re)

(defun org-activate-hidden-links-additional (limit)
  "Put invisible-property org-link on strings matching"
  (if org-hidden-links-additional-re
      (re-search-forward org-hidden-links-additional-re limit t)
    (goto-char limit)
    nil))

(defun org-hidden-links-hook-function ()
  "Add rule for `org-activate-hidden-links-additional' to `org-font-lock-extra-keywords'.
You can include this function in `org-font-lock-set-keywords-hook'."
  (add-to-list 'org-font-lock-extra-keywords
                              '(org-activate-hidden-links-additional
                                (1 '(face org-target invisible org-link))
                (2 '(face org-target invisible org-link)))))

(add-hook 'org-font-lock-set-keywords-hook #'org-hidden-links-hook-function)

;; Insert Heading After Current key binding
(global-set-key (kbd "C-c M-RET") 'org-insert-heading-after-current)

;; org-mode indentation styling
(setq org-indent-indentation-per-level 1)
(setq org-hide-leading-stars nil)
