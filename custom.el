(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; ---- Disable uncomfortable features ----
(disable-theme 'zenburn)
;; (setq prelude-theme 'solarized-dark)
(setq prelude-whitespace nil)
;; (global-hl-line-mode -1)
(global-set-key [remap move-beginning-of-line]
                'move-beginning-of-line)


;; ---- Wanted features ----
(set-frame-parameter nil 'fullscreen 'fullboth)

(when (not (display-graphic-p))
  (menu-bar-mode -1))
(when  (display-graphic-p)
  (setq confirm-kill-emacs 'yes-or-no-p)
  (scroll-bar-mode -1))

(setq display-time-day-and-date 't)
(setq display-time-24hr-format 't)
(display-time)

;; ---- Change to comfortable keys ----
(global-set-key (kbd "C-\\")  'undo-tree-redo)
(global-set-key (kbd "C-s-p") 'move-text-up)
(global-set-key (kbd "C-s-n") 'move-text-down)
(global-set-key (kbd "C-M-j") 'prelude-duplicate-current-line-or-region)
(global-set-key (kbd "C-s-j") 'prelude-duplicate-and-comment-current-line-or-region)

;; --------------------------------------
;;  File modification required features:
;; --------------------------------------
;; Mod: ~/.emacs.d/modules/prelude-c.el: (c-basic-offset 2)
;; Add: ~/.emacs.d/modules/prelude-c.el: (local-unset-key (kbd "C-M-j"))
;; Mod: ~/.emacs.d/elpa/smartparens/smartparens.el: ("C-s-s" . sp-splice-sexp)
;; --------------------------------------

(custom-set-variables
 (when (display-graphic-p)
   '(custom-enabled-themes (quote (deeper-blue))))
 (when (not (display-graphic-p))
   '(custom-enabled-themes (quote (tango-dark))))
 )
