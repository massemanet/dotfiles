;;; masserlang --- Summary
;;; Commentary:
;;; masse's erlang setup
;;; Code:

(require 'erlang-start)
(require 'erlang)
(require 'comint)
(require 'flycheck)

(add-hook
 'erlang-shell-mode-hook
 (lambda ()
   (setq comint-history-isearch 'dwim
         comint-input-ignoredups t)
   (local-set-key (kbd "C-n") 'comint-next-input)
   (local-set-key (kbd "C-p") 'comint-previous-input)))

(add-hook
 'erlang-new-file-hook
 (lambda ()
   (insert (concat "-module(" (erlang-get-module-from-file-name) ").\n\n"))
   (insert (concat "-export([]).\n\n"))))

(declare-function acer-init "acer")
(add-hook
 'erlang-mode-hook
 (lambda ()
   (when (locate-library "acer")
     (require 'acer)
     (acer-init))
   (setq erlang-electric-commands nil
         erlang-indent-level 4
         erlang-man-download-url "http://erlang.org/download/otp_doc_man_25.3.tar.gz"
         flycheck-erlang-executable "erlc"
         flycheck-erlang-include-path (rebar-files "include")
         flycheck-erlang-library-path (rebar-files "ebin"))))

(defun rebar-files (type)
  "List of files of TYPE that rebar3 knows about."
  (let ((files (lambda(p) (file-expand-wildcards (concat (flycheck-rebar3-project-root) p type)))))
    (append
     (funcall files "_build/*/lib/*/")
     (funcall files "_checkouts/*/"))))

(provide 'masserlang)
;;; masserlang.el ends here
