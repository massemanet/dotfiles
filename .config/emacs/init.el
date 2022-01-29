;;; init --- an init file.
;;; Commentary:
;;; an init file.
;;; Code:
;;; an init file.

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(require 'straight (expand-file-name "straight/repos/straight.el/straight.el" user-emacs-directory))

(straight-use-package 'doom-modeline)
(straight-use-package '(flycheck :fork "massemanet/flycheck"))
(straight-use-package 'flycheck-popup-tip)
(straight-use-package 'erlang)
(straight-use-package 'company-erlang)
(straight-use-package 'json-mode)
(straight-use-package 'auctex)
(straight-use-package 'json-reformat)
(straight-use-package 'json-snatcher)
(straight-use-package 'macrostep)
(straight-use-package 'yaml-mode)
(straight-use-package 'magit)
(straight-use-package 'magit-gitflow)
(straight-use-package 'magit-popup)
(straight-use-package 'magit-todos)
(straight-use-package 'markdown-mode)
(straight-use-package 'markdown-toc)
(straight-use-package 'nyan-mode)
(straight-use-package 'rainbow-delimiters)

;; turn on good shit
(show-paren-mode t)
(transient-mark-mode t)
(global-font-lock-mode t)
(delete-selection-mode 1)
(ido-mode t)
(declare-function nyan-mode "ext:")
(nyan-mode 1)
(declare-function global-flycheck-mode "ext:")
(global-flycheck-mode)
(declare-function flycheck-popup-tip-mode "ext:")
(flycheck-popup-tip-mode)
(declare-function doom-modeline-mode "ext:")
(doom-modeline-mode)

(fset 'yes-or-no-p 'y-or-n-p)

;; turn off bad shit
(if (featurep 'tool-bar)   (tool-bar-mode   -1))
(if (featurep 'tooltip)    (tooltip-mode    -1))
(if (featurep 'scroll-bar) (scroll-bar-mode -1))
(if (featurep 'menu-bar)   (menu-bar-mode   -1))

(require 'term)
(eval-after-load "term"
  '(define-key term-raw-map (kbd "C-c C-y") 'term-paste))

(require 'ediff)
(setq
 indent-tabs-mode               nil
 explicit-shell-file-name       "/bin/bash"
 vc-handled-backends            nil
 ediff-window-setup-function    'ediff-setup-windows-plain
 inhibit-startup-screen         t
 visible-bell                   nil
 default-input-method           "rfc1345"
 max-lisp-eval-depth            40000
 scroll-down-aggressively       0.1
 scroll-up-aggressively         0.1)

(defun ido-kill-emacs-hook ()
  "Ido is annoying."
  (declare-function ido-save-history "ext:")
  (ignore-errors (ido-save-history)))

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun prev-window ()
  "Select previous window."
  (interactive)
  (select-window (previous-window (selected-window) nil nil)))

;; keybindings
(global-set-key (kbd "TAB")     `indent-according-to-mode)
(global-set-key (kbd "C-j")     `scroll-down)
(global-set-key (kbd "C-x !")   `shell-command)
(global-set-key (kbd "C-x %")   `query-replace)
(global-set-key (kbd "C-x ,")   'beginning-of-buffer)
(global-set-key (kbd "C-x .")   'end-of-buffer)
(global-set-key (kbd "C-x C-r")	'revert-buffer)
(global-set-key (kbd "C-x O")   `switch-to-previous-buffer)
(global-set-key (kbd "C-x T")   `transpose-words)
(global-set-key (kbd "C-x [")   'flycheck-previous-error)
(global-set-key (kbd "C-x ]")   'flycheck-next-error)
(global-set-key (kbd "C-x a")   'align-regexp)
(global-set-key (kbd "C-x g")   `goto-line)
(global-set-key (kbd "C-x n")   `forward-list)
(global-set-key (kbd "C-x o")   'prev-window)
(global-set-key (kbd "C-x p")   `backward-list)
(global-set-key (kbd "C-x q")   `fill-paragraph)
(global-set-key (kbd "C-x t")   `transpose-lines)
(global-set-key (kbd "C-x v")   `scroll-down)
(global-set-key (kbd "C-x w")   `kill-ring-save)
(global-set-key (kbd "C-x x")   'execute-extended-command)
(global-set-key (kbd "C-x C-y") `yank-pop)
(global-set-key (kbd "C-x {")   `previous-error)
(global-set-key (kbd "C-x }")   `next-error)
(global-set-key (kbd "C-z")     'undo) ; be like a mac
(global-set-key (kbd "M-z")     'undo) ; if screen eats C-z

(let ((map minibuffer-local-map))
  (define-key map (kbd "C-n")   'next-history-element)
  (define-key map (kbd "C-p")   'previous-history-element))

(add-hook
 'after-init-hook
 (lambda ()
   (progn
     (load-theme 'tsdh-dark)
     (add-to-list 'load-path (expand-file-name "masserlang" user-emacs-directory))
     (require 'masserlang)
     (add-to-list 'load-path (expand-file-name "fdlcap" user-emacs-directory))
     (require 'fdlcap))))

(add-hook
 'ediff-mode-hook
 (lambda ()
   (progn
     (set-face-attribute 'ediff-fine-diff-B nil :background "#055505"))))

(provide 'init)
;;; init.el ends here
