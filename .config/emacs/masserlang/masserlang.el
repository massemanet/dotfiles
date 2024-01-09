;;; masserlang --- Summary
;;; Commentary:
;;; masse's erlang setup
;;; Code:

(require 'erlang-start)
(require 'erlang)
(require 'flycheck)
(require 'xref)

(declare-function acer-init "acer")

(add-hook
 'erlang-mode-hook
 (lambda ()
   (erlang-map-unset ";" "," "<" ">" "RET")
   (add-to-list 'load-path "~/git/acer")
   (when (locate-library "acer")
     (require 'acer)
     (acer-init)
     (erlang-map-set "C-." #'xref-find-definitions
                     "C-," #'xref-go-back))
   (unless (memq 'erlang-rebar3 flycheck-disabled-checkers)
     (push 'erlang-rebar3 flycheck-disabled-checkers))
   (setq erlang-electric-commands nil
         erlang-indent-level 4
         erlang-man-download-url "http://erlang.org/download/otp_doc_man_25.3.tar.gz"
         flycheck-erlang-executable "erlc"
         flycheck-erlang-include-path (rebar-files "include")
         flycheck-erlang-library-path (rebar-files "ebin")
         flycheck-protobuf-protoc-executable "protoc -I../../..")))

(add-hook
 'erlang-new-file-hook
 (lambda ()
   (insert (concat "-module(" (erlang-get-module-from-file-name) ").\n\n"))
   (insert (concat "-export([]).\n\n"))))

(defun erlang-map-unset (&rest keys)
  "Unset KEYS in erlang-mode-map."
  (mapc (lambda(key) (keymap-unset erlang-mode-map key t)) keys))

(defun erlang-map-set (&rest defs)
  "Bind KEY to CMD in erlang-mode-map, where DEFS is (KEY . CMD)."
  (while defs
    (let ((k (pop defs))
          (c (pop defs)))
      (keymap-set erlang-mode-map k c))))

(defun rebar-files (type)
  "List of files of TYPE that rebar3 knows about."
  (let ((files (lambda(p) (file-expand-wildcards (concat (flycheck-rebar3-project-root) p type)))))
    (append
     (funcall files "_build/*/lib/*/")
     (funcall files "_checkouts/*/"))))

(provide 'masserlang)
;;; masserlang.el ends here
