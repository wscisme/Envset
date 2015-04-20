;; Modes to make life easier  
(show-paren-mode 1)
(electric-pair-mode 1)
(column-number-mode 1)

;; Mac specifics
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'alt)
(set-face-attribute 'default nil :height 130)
(set-frame-parameter nil 'fullscreen 'fullboth)


(setq-default indent-tabs-mode nil)
(setq display-time-day-and-date 't)
(setq display-time-24hr-format 't)
(display-time)

(when (not (display-graphic-p))
  (menu-bar-mode -1))
(when  (display-graphic-p)
  (scroll-bar-mode -1))

(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "M-I") 'tab-to-tab-stop)     ;; Move original M-i to M-I
(global-set-key (kbd "M-i") 'company-complete)
;; (global-set-key (kbd "C-i") 'indent-for-tab-command)

;; Trials
;; (setq confirm-kill-emacs 'yes-or-no-p)
;; (setq c-basic-offset 4)

;; Functions and Keys to make life easier

(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)
(global-set-key (kbd "C-S-d") 'duplicate-line)

(defun duplicate-current-line-or-region (arg)
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg))))
)
(global-set-key (kbd "C-M-j") 'duplicate-current-line-or-region)
(global-set-key (kbd "C-c C-d") 'duplicate-current-line-or-region)

;; when you do not have a text selection, copy will just copy the current line. Similar for cut. 
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy the current line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (progn
       (message "Current line is copied.")
       (let (beg (origin (point)))
	 (setq beg (line-beginning-position))
	 (forward-line 1)
	 (list beg (line-beginning-position))))))
)

(defadvice kill-region (before slick-copy activate compile)
  "When called interactively with no active region, cut the current line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (progn
       (message "Current line is cut.")
       (let (beg (origin (point)))
	 (setq beg (line-beginning-position))
	 (forward-line 1)
	 (list beg (line-beginning-position))))))
)

;; (defun kill-region-or-whole-line (arg)
;;   "When called interactively with no active region, cut the current line."
;;   (interactive "p")
;;   (if mark-active
;;       (kill-region (point) (mark)) 
;;     (progn
;;       (message "Current line is cut.")
;;       (dotimes (i arg)
;; 	(kill-whole-line))))
;; )
;; (global-set-key (kbd "C-w") 'kill-region-or-whole-line)
;; (global-set-key (kbd "M-p") 'kill-whole-line)


;; Dangerously mapping kill whole line to M-n
;; (global-set-key (kbd "M-n") 'kill-whole-line)

;; (global-set-key (kbd "M-P") 'backward-list)
;; (global-set-key (kbd "M-N") 'forward-list)

;; (defun move-line-up() 
;;   (interactive)
;;   (transpose-lines 1)
;;   (previous-line 2)
;; )
;; ;;(global-set-key (kbd "C-M-p") 'move-line-up)

;; (defun move-line-down ()
;;   (interactive)
;;   (next-line 1)
;;   (transpose-lines 1)
;;   (previous-line 1)
;; )
;; ;;(global-set-key (kbd "C-M-n") 'move-line-down)

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (if (< arg 0) (forward-line -1))  ;; Fixed for Emacs 24.5 ?
      (move-to-column column t))))
)

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg)
)

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg))
)

(provide 'move-text)

(global-set-key (kbd "M-P") 'move-text-up)
(global-set-key (kbd "M-N") 'move-text-down)


;; Special cases
(add-to-list 'auto-mode-alist '(".bash_aliases" . shell-script-mode))

;; make .h files be font-locked as c++, not c                                                                       
(setq auto-mode-alist (cons (cons "\\.h\\'" 'c++-mode) auto-mode-alist))

;; make .h files be font-locked as c++, not c                                                                       
(setq auto-mode-alist (cons (cons "\\.m\\'" 'octave-mode) auto-mode-alist))


;; use tramp, have tramp use ssh config
(require 'tramp)
(setq tramp-default-method "ssh")
(tramp-set-completion-function "ssh"
			       '((tramp-parse-sconfig "/etc/ssh/ssh_config")
				 (tramp-parse-sconfig "/home/wsc/.ssh/config")))

;;remove splash screen
(setq inhibit-splash-screen t)

;; use tab for autocomplete
;;(local-set-key (kbd "TAB") 'dabbrev-expand)

;; put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosave/" t)

;; backup by copying
(setq backup-by-copying t)

;; control how many backup versions to keep
(setq make-backup-files t               ; make ~ files
      ;; With nil, numbered backups are made only if they already exist.
      ;; A new backup version is made every time the file is loaded.
      version-control t ; set to t below if `backup-directory-alist' exists
      kept-old-versions 2
      kept-new-versions 6
      ;; Preserves permissions of file being edited. Also affects links.
      backup-by-copying nil
      backup-by-copying-when-linked t
      backup-by-copying-when-mismatch nil
      backup-by-copying-when-privileged-mismatch 200
      delete-old-versions t           ; auto-delete excess numbered backups
      delete-auto-save-files t          ; delete auto-save files on save
      auto-save-default t               ; auto-save on every visit
      auto-save-interval 200            ; input events between auto-saves
      auto-save-timeout 30)             ; seconds idleness before autosave


;; AUCTeX
;; (require 'tex-site)

;; Enable restricted functions
(put 'scroll-left 'disabled nil)

;; Integrate Yasnippet into company-mode
;; (define-key company-active-map "\t" 'company-yasnippet-or-completion)
 
;; (defun company-yasnippet-or-completion ()
;;   (interactive)
;;   (if (yas/expansion-at-point)
;;       (progn (company-abort)
;; 	     (yas/expand))
;;     (company-complete-common))
;; )
 
;; (defun yas/expansion-at-point ()
;;   "Tested with v0.6.1. Extracted from `yas/expand-1'"
;;   (first (yas/current-key))
;; ) 

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 (when (display-graphic-p)
   '(custom-enabled-themes (quote (deeper-blue))))
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
