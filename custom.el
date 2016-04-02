(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; ---- Disable uncomfortable features ----
(disable-theme 'zenburn)
;; (setq prelude-theme 'solarized-dark)
(setq prelude-whitespace nil)
(setq prelude-flyspell nil)
(setq electric-indent-mode nil)
(global-set-key [remap move-beginning-of-line]
                'move-beginning-of-line)


;; ---- Wanted features ----
(set-frame-parameter nil 'fullscreen 'fullboth)

(when (not (display-graphic-p))
  (menu-bar-mode -1)  
  (global-hl-line-mode -1))
(when  (display-graphic-p)
  (setq confirm-kill-emacs 'yes-or-no-p)
  (scroll-bar-mode -1))

(setq display-time-day-and-date 't)
(setq display-time-24hr-format 't)
(display-time)

;; ---- Add comfortable key bindings ----
(global-set-key (kbd "C-\\")  'undo-tree-redo)
(global-set-key (kbd "C-s-p") 'move-text-up)
(global-set-key (kbd "C-s-n") 'move-text-down)
(global-set-key (kbd "C-M-j") 'prelude-duplicate-current-line-or-region)
(global-set-key (kbd "C-s-j") 'prelude-duplicate-and-comment-current-line-or-region)
(global-set-key (kbd "s-SPC") 'just-one-space)
(global-set-key (kbd "C-M-g") 'god-local-mode)

;; ---- Key-chords ----
(key-chord-define-global "OO" 'other-window)
(key-chord-define-global "KK" 'delete-window)
(key-chord-define-global "WW" 'delete-other-windows)
(key-chord-define-global "BB" 'ido-switch-buffer) 
(key-chord-define-global "LL" (lambda()
                                (interactive)
                                (split-window-right)
                                (other-window 1)
                                (prelude-switch-to-previous-buffer)))
(key-chord-define-global "XX" (lambda()
                                (interactive)
                                (kill-buffer (current-buffer))
                                (delete-window)))
(key-chord-define c++-mode-map ";;"  "\C-e;")
;; (key-chord-define latex-mode-map "{}"  "{}\C-b")

;; ---- Multiple-cursors ----
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-M-<mouse-1>") 'mc/add-cursor-on-click)

;; ---- God-mode ----
(require 'god-mode)
(define-key god-local-mode-map (kbd ".") 'repeat)
(define-key god-local-mode-map (kbd "i") 'god-local-mode)
(require 'god-mode-isearch)
(define-key isearch-mode-map (kbd "M-a") 'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "M-a") 'god-mode-isearch-disable)
(defun update-cursor-color ()
  (set-cursor-color (if (or god-local-mode buffer-read-only)
                        'gray
                      'green)))                    ;; theme specific
(add-hook 'post-command-hook 'update-cursor-color) ;; potential problem?

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
