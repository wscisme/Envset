;; --------------------------------------
;; This file is based on Emacs Prelude at https://github.com/bbatsov/prelude
;; To be put at ~/.emacs.d/personal/
;; --------------------------------------

;; ---- Mac specifics ----
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(set-frame-parameter nil 'fullscreen 'fullboth)

;; ---- Disable uncomfortable features ----
(disable-theme 'zenburn)
;; (setq prelude-theme 'solarized-dark)
(setq prelude-whitespace nil)
(setq prelude-flyspell nil)
;; (setq electric-indent-mode nil)
;; (setq prelude-auto-save nil)
(global-unset-key (kbd "C--"))
(global-flycheck-mode -1)
(remove-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(global-company-mode -1)  ; due to the lag over remote
(projectile-mode -1)      ; due to the lag over remote

;; ---- Adjusted features ----
;; (setq scroll-margin 2)
(setq scroll-conservatively 8)
(set-face-attribute 'default nil :height 140)

;; ---- Additional features ----
(when (not (display-graphic-p))
  (menu-bar-mode -1)
  (beacon-mode -1)
  (global-hl-line-mode -1))
(when  (display-graphic-p)
  (scroll-bar-mode -1)
  (setq confirm-kill-emacs 'yes-or-no-p))

(setq display-time-day-and-date 't)
(setq display-time-24hr-format 't)
(display-time)
(set-default 'truncate-partial-width-windows 185)

;; ---- Additional auto-mode ----
(add-to-list 'auto-mode-alist '(".bash_aliases" . shell-script-mode))
(setq auto-mode-alist (cons (cons "\\.h\\'" 'c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons (cons "\\.icc\\'" 'c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons (cons "\\.xdc\\'" 'tcl-mode) auto-mode-alist))

;; ---- Additional functions ----
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
   If no region is selected and current line is not blank and we are not at the end of the line,
   then comment current line.
   Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (not (region-active-p))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

;; ---- Additional global key bindings ----
(global-set-key (kbd "C-\\")  'undo-tree-redo)
(global-set-key (kbd "C-s-p") 'move-text-up)
(global-set-key (kbd "C-s-n") 'move-text-down)
(global-set-key (kbd "C-s-f") 'forward-whitespace)
(global-set-key (kbd "C-s-b") (lambda() (interactive) (forward-whitespace -1)))
(global-set-key (kbd "s-SPC") 'just-one-space)
(global-set-key (kbd "M-s r") 'rgrep)
(global-set-key (kbd "M-s d d") 'ediff)
(global-set-key (kbd "M-s d b") 'ediff-buffers)
(global-set-key (kbd "M-s d r") 'ediff-revision)
(global-set-key (kbd "M-s d l") 'ediff-regions-linewise)
(global-set-key (kbd "M-g t c") 'tramp-cleanup-connection)
(global-set-key (kbd "M-g t a") 'tramp-cleanup-all-connections)
(global-set-key (kbd "M-g i w") 'toggle-truncate-lines)
(global-set-key (kbd "M-n") (lambda() (interactive) (next-line 7)))
(global-set-key (kbd "M-p") (lambda() (interactive) (previous-line 7)))

;; ---- Additional Key-chord bindings ----
(key-chord-define-global "OO" 'other-window)
(key-chord-define-global "LL" (lambda() (interactive)
                                (split-window-right)
                                (other-window 1)
                                (ido-switch-buffer)))
(key-chord-define-global "XX" (lambda() (interactive)
                                (kill-buffer (current-buffer))
                                (delete-window)))
(key-chord-define-global ";;" 'comment-dwim-line)
(key-chord-define-global "xx" 'nil)

;; ---- Multiple-cursors configs ----
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-next-like-this-word)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-M-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "C-, C-,") 'mc/freeze-fake-cursors-dwim)
(global-set-key (kbd "C-, SPC") 'mc/mark-pop)
(define-key mc/keymap (kbd "<return>") nil)
(define-key mc/keymap (kbd "C-, n") 'mc/insert-numbers)
(define-key mc/keymap (kbd "C-, a") 'mc/insert-letters)
(define-key mc/keymap (kbd "C-, s") 'mc/skip-to-next-like-this)
(define-key mc/keymap (kbd "C-, m") 'mc/unmark-next-like-define)
(define-key mc/keymap (kbd "C-, M-C-f") 'mc/mark-next-sexps)
(define-key mc/keymap (kbd "C-, M-C-b") 'mc/mark-previous-sexps)
(define-key mc/keymap (kbd "C-, <") 'mc/mark-all-above)
(define-key mc/keymap (kbd "C-, >") 'mc/mark-all-below)
(define-key mc/keymap (kbd "C-, C-d") 'mc/remove-current-cursor)
(define-key mc/keymap (kbd "C-, C-k") 'mc/remove-cursors-at-eol)
(define-key mc/keymap (kbd "C-, d")   'mc/remove-duplicated-cursors)
(define-key mc/keymap (kbd "C-, C-o") 'mc/remove-cursors-on-blank-lines)
(define-key mc/keymap (kbd "C-, ,")   'mc/move-to-column)
(define-key mc/keymap (kbd "C-, =")   'mc/compare-chars)

;; ---- God-mode configs ----
(require 'god-mode)
(global-set-key (kbd "C-;") 'god-local-mode)
(global-set-key (kbd "M-s a") (lambda() (interactive)
                                (isearch-forward-symbol-at-point)
                                (god-mode-isearch-activate)))
(define-key god-local-mode-map (kbd ".") 'repeat)
(define-key god-local-mode-map (kbd "i") 'god-local-mode)
(define-key god-local-mode-map (kbd "M-s .") (lambda() (interactive)
                                               (isearch-forward-symbol-at-point)
                                               (god-mode-isearch-activate)))
(define-key god-local-mode-map [escape] 'keyboard-quit)
(require 'god-mode-isearch)
(define-key isearch-mode-map (kbd "M-a") 'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "M-a") 'god-mode-isearch-disable)
(define-key god-mode-isearch-map (kbd "M-v") 'scroll-down-command)
(define-key god-mode-isearch-map (kbd "v") 'scroll-up-command)
(define-key god-mode-isearch-map (kbd "n") 'next-line)
(define-key god-mode-isearch-map (kbd "p") 'previous-line)
(defun god-mode-update-cursor ()
  (setq cursor-type (if god-local-mode '(bar . 3) 'box)))
(add-hook 'god-mode-enabled-hook 'god-mode-update-cursor)
(add-hook 'god-mode-disabled-hook 'god-mode-update-cursor)

;; ---- Company configs ----
(setq company-idle-delay 0)
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-code-ignore-case t)
(global-set-key (kbd "M-i") 'company-complete)
(define-key company-active-map (kbd "<return>") nil)
(define-key company-active-map (kbd "RET") nil)
(define-key company-active-map (kbd "M-i") 'company-complete-selection)
(define-key company-active-map (kbd "TAB") 'company-indent-or-complete-common)

;; ---- Smartparens configs ----
(define-key smartparens-mode-map (kbd "M-<backspace>") nil)
(define-key smartparens-mode-map (kbd "s-<backspace>") 'sp-backward-unwrap-sexp)
(define-key smartparens-mode-map (kbd "C-M-SPC") nil)
(define-key smartparens-mode-map (kbd "C-M-u") nil)
(define-key smartparens-mode-map (kbd "C-M-n") nil)
(define-key smartparens-mode-map (kbd "C-M-p") nil)
(define-key smartparens-mode-map (kbd "C-M-j") 'sp-down-sexp)
(define-key smartparens-mode-map (kbd "C-s-<backspace>") 'sp-splice-sexp-killing-around)
(define-key smartparens-mode-map (kbd "C-s-]") 'sp-select-previous-thing)
(define-key smartparens-mode-map (kbd "M-S") 'sp-split-sexp)
(define-key smartparens-mode-map (kbd "s-f") 'sp-forward-symbol)
(define-key smartparens-mode-map (kbd "s-b") 'sp-backward-symbol)

;; ---- Other mode specific setup ----
(set-face-background 'hl-line "#3F3F3F") ;; for deeper-blue theme
(add-hook 'latex-mode-hook 'smartparens-mode)
(add-hook 'latex-mode-hook (lambda() (key-chord-define latex-mode-map "==" "&=& ")))
(key-chord-define c++-mode-map ".." "->")
(define-key c++-mode-map (kbd "C-M-j") nil)
(define-key c++-mode-map (kbd "C-c .") nil)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
(add-to-list 'ido-ignore-files "\.root")
(add-to-list 'ido-ignore-files "\.pcm")
(add-to-list 'ido-ignore-files "\_C.d")
(setq verilog-indent-level 2)
(add-hook 'verilog-mode-hook (lambda() (local-unset-key (kbd "C-;"))))
(add-hook 'verilog-mode-hook (lambda() (local-unset-key (kbd ";"))))
(setq vhdl-end-comment-column 180)
(add-hook 'vhdl-mode-hook (lambda() (local-unset-key (kbd "SPC"))))


;; ---- Keyboard macros ----
(fset 'debug-cout-line (lambda (&optional arg) "Make a cout statement of the current line."
 (interactive "p") (kmacro-exec-ring-item (quote ([1 9 67108896 5 23 99 111 117 116 32 60 60 32 95 95 76 73 78 69 95 95 32 60 60 32 34 58 32 25 61 32 5 32 60 60 32 25 32 60 60 32 101 110 100 108 59 1 14] 0 "%d")) arg)))
(define-key c++-mode-map (kbd "M-g d d") 'debug-cout-line)

(fset 'debug-cout-line-with-fn (lambda (&optional arg) "Make a cout statement of the current line with filename."
 (interactive "p") (kmacro-exec-ring-item (quote ([1 9 67108896 5 23 99 111 117 116 32 60 60 32 95 95 70 73 76 69 95 95 32 60 60 32 39 58 39 32 60 60 32 95 95 76 73 78 69 95 95 32 60 60 32 34 58 32 25 61 32 5 32 60 60 32 25 32 60 60 32 101 110 100 108 59 1 14] 0 "%d")) arg)))
(define-key c++-mode-map (kbd "M-g d f") 'debug-cout-line-with-fn)

(fset 'debug-cout-two-var (lambda (&optional arg) "Make a cout statement of the current line with 2 variables."
 (interactive "p") (kmacro-exec-ring-item (quote ([1 9 67108896 8388614 23 99 111 117 116 32 60 60 32 95 95 76 73 78 69 95 95 32 60 60 32 34 58 32 25 backspace 61 32 6 32 60 60 32 25 67108896 5 23 60 60 32 34 44 32 25 61 32 5 32 60 60 32 25 32 60 60 32 101 110 100 108 59 1] 0 "%d")) arg)))
(define-key c++-mode-map (kbd "M-g d 2") 'debug-cout-two-var)

(fset 'debug-cout-append-one (lambda (&optional arg) "Append the current variable to the cout statement at previous line." 
 (interactive "p") (kmacro-exec-ring-item (quote ([1 9 67108896 5 23 4 16 5 67108896 134217826 backspace 34 44 32 25 61 32 5 32 60 60 32 25 32 60 60 32 101 110 100 108 59 1 14 9] 0 "%d")) arg)))
(define-key c++-mode-map (kbd "M-g d a") 'debug-cout-append-one)

;; ;; verilog mode new indent levels
;; (custom-set-variables
;;  '(verilog-align-ifelse t)
;;  '(verilog-auto-delete-trailing-whitespace t)
;;  '(verilog-auto-inst-param-value t)
;;  '(verilog-auto-inst-vector nil)
;;  '(verilog-auto-lineup (quote all))
;;  '(verilog-auto-newline nil)
;;  '(verilog-auto-save-policy nil)
;;  '(verilog-auto-template-warn-unused t)
;;  '(verilog-case-indent 2)
;;  '(verilog-cexp-indent 2)
;;  '(verilog-highlight-grouping-keywords t)
;;  '(verilog-highlight-modules t)
;;  '(verilog-indent-level 2)
;;  '(verilog-indent-level-behavioral 2)
;;  '(verilog-indent-level-declaration 2)
;;  '(verilog-indent-level-module 2)
;;  '(verilog-tab-to-comment nil))

;; --------------------------------------
;;  File modification required features:
;; --------------------------------------
;; Mod: ~/.emacs.d/modules/prelude-c.el: 39: (c-basic-offset 2)
;; Add: ~/.emacs.d/modules/prelude-c.el: 40: (local-unset-key (kbd "C-M-j"))
;; Mod: ~/.emacs.d/elpa/god-mode/god-mode.el: 45: ("m" . "M-")
;; Mod: ~/.emacs.d/core/prelude-editor.el: 88: (sp-use-smartparens-bindings)
;; Mod: ~/.emacs.d/core/prelude-editor.el: 171-175: ;; ...
;; Mod: ~/.emacs.d/core/prelude-editor.el: 302-305: ;; ...
;; Mod: ~/.emacs.d/core/prelude-mode.el: 46: ;; ...
;; --------------------------------------

;; (custom-set-variables
;;  '(c-offsets-alist (quote ((inlambda . 0))))
;;  (when (display-graphic-p)
;;    '(custom-enabled-themes (quote (deeper-blue))))
;;  (when (not (display-graphic-p))
;;    '(custom-enabled-themes (quote (tango-dark))))
;;  )

(custom-set-variables
'(c-offsets-alist (quote ((inlambda . 0))))
 (when (display-graphic-p)
   '(custom-enabled-themes (quote (deeper-blue))))
 (when (not (display-graphic-p))
   '(custom-enabled-themes (quote (tango-dark))))
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
