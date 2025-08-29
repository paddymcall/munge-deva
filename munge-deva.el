#! /bin/bash
":"; exec emacs -Q --no-site-file --script "$0" -- "$@" # -*-emacs-lisp-*-

;; Try to fix the devanāgarī stuff.

;; See "~/research/emacs-stuff/hacks.el" for these functions:

;; (message (format "Load path is: %s" (mapconcat #'identity load-path "\n")))

(require 'indian-ext
         (expand-file-name "bin/stei/elisp/indian-ext/indian-ext.el"
                           default-directory))

(or
 (require 'munge-deva-libs nil 'no-error)
 (require 'munge-deva-libs
          (expand-file-name "bin/stei/elisp/munge-deva-libs.el"
                            default-directory)))
;; (require 'elp)

(when noninteractive
  ;; (elp-instrument-list
  ;;  (mapcar
  ;;   'intern
  ;;   (all-completions "hacks/" obarray 'elp-profilable-p)))
  (let ((files (cdr command-line-args-left))
        (rng-nxml-auto-validate-flag nil)
	(pst5-stoppers '("p" "l" "lg" "pc")))
    (message
     (format "Munging Devanāgarī in file(s): %s\n\nElements ignored: %s\n"
	     files
	     pst5-stoppers))
    (mapc
     (lambda (file)
       (let ((buff (find-file-noselect file)))
         (with-current-buffer buff
           (goto-char (point-min))
           (let (start end result-buff)
             (save-excursion
               (while (xmltok-forward)
                 (cond
                  ((and (eq xmltok-type 'start-tag)
                        (string= (xmltok-start-tag-local-name) "body"))
                   (setq start xmltok-start))
                  ((and (eq xmltok-type 'end-tag)
                        (string= (xmltok-end-tag-local-name) "body"))
                   (setq end (point))))))
             (when (null start)
               (warn "Could not find start of body element in %s, munging from start"
                     (buffer-name (current-buffer)))
               (setq start (point-min)))
             (when (null end)
               (warn "Could not find end of body element in %s, munging to end"
                     (buffer-name (current-buffer)))
               (setq end (point-max)))
             (narrow-to-region start end)
             (message "Operating on tei:body in %s to %s" start end)
             (setq result-buff (munge-deva-fix-deva-breaks (current-buffer) pst5-stoppers))
             (message "Completed fix, replacing")
             (delete-region start end)
             (message "Inserting results from %s" result-buff)
             (insert-buffer-substring result-buff)
             (widen)
             ;; (elp-results)
             ;; (warn (buffer-string))
             (princ
              (buffer-string))))))
     files)))
