;;; bytecomp-simplify.el --- byte compile warnings for simplifications

;; Copyright 2009, 2010 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 4
;; Keywords: extensions
;; URL: http://user42.tuxfamily.org/bytecomp-simplify/index.html

;; bytecomp-simplify.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; bytecomp-simplify.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This spot of code add warnings to the byte compiler to report on possible
;; simplifications in the code compiled.  Eg. (kill-buffer (current-buffer))
;; can be simplified to (kill-buffer nil).
;;
;; The reports include
;;
;;     char-before          \ (point) argument can be omitted
;;     char-after           /
;;     up-list              \ count==1 can be omitted in Emacs 21 up
;;     down-list            /
;;     delete-windows-on    current-buffer can be omitted in Emacs 23 up
;;     kill-buffer          current-buffer can be nil, or omitted in Emacs 23
;;
;;     re-search-forward    \ constant pattern can be plain search-forward
;;     re-search-backward   /   or search-backward
;;     search-forward       \
;;     search-backward      | (point-min) or (point-max) limit can be nil
;;     re-search-forward    |
;;     re-search-backward   /
;;
;; Things like `delete-windows-on' which are version-dependent are reported
;; only when the Emacs in use allows the simplification.  So
;; `delete-windows-on' is only reported in Emacs 23 up, and the message
;; includes a note that it's for 23 up.
;;
;; The version-dependent bits might have scope for a kind of reverse check,
;; warning about a feature newer than a specified target Emacs supports.
;; Except conditionalized code would trigger it far too often.
;;
;; Checks not done:
;;
;; `local-variable-p' argument `current-buffer' can be omitted in Emacs, but
;; not in XEmacs.  Reporting that would only spam out dual-targeted code.

;;; Install:

;; Put bytecomp-simplify.el somewhere in one of your `load-path' directories
;; and in your .emacs
;;
;;     (eval-after-load "bytecomp" '(require 'bytecomp-simplify))
;;
;; It's normal to byte compile with "emacs -q -no-site-file", which doesn't
;; read your .emacs.  If your makefile or configure has a controllable
;; $(EMACS) you can work bytecomp-simplify.el into it with
;;
;;     make EMACS="emacs -l /path/to/my/files/bytecomp-simplify.el"
;;
;; This is a tedious, but "-q -no-site-file" is the right thing when byte
;; compiling to avoid site or user settings, so there must be special action
;; to use an extension like bytecomp-simplify.

;;; Emacsen:

;; Designed for Emacs 21 and up, works in XEmacs 21.

;;; History:

;; Version 1 - the first version
;; Version 2 - fix customize setup reported by Drew Adams
;;           - fixes for search point-max and negated regexp
;; Version 3 - new up-list,down-list, char-before,char-after
;;           - fixes for emacs21
;; Version 4 - undo defadvice on unload-feature

;;; Code:

;;;###autoload (eval-after-load "bytecomp" '(require 'bytecomp-simplify))


;;-----------------------------------------------------------------------------
;; `simplify' warnings type

;; In emacs21 when byte-compile-warnings is t, the bytecomp code let-binds
;; it to the full `byte-compile-warning-types' list, so must add `simplify'
;; there to have it take effect.  Likewise xemacs21 out of
;; `byte-compile-default-warnings'.
;;
(eval-after-load "bytecomp"
  '(progn
     (when (boundp 'byte-compile-warning-types) ;; not in xemacs21
       (add-to-list 'byte-compile-warning-types 'simplify))

     (when (boundp 'byte-compile-default-warnings) ;; xemacs21 incompatibility
       (add-to-list 'byte-compile-default-warnings 'simplify))

     ;; Add to `byte-compile-warnings' custom options.
     ;; In xemacs21 `byte-compile-warnings' is only a defvar, so nothing there.
     ;;
     ;; Maybe should copy the value, but `copy-tree' needs 'cl in emacs21
     ;; and bringing that in might hide missing cl's in the code being byte
     ;; compiled
     ;;
     (let ((type (get 'byte-compile-warnings 'custom-type)))
       (when (eq 'choice (car-safe type))
         (let ((set (assq 'set type)))
           (when (and set (not (member '(const simplify) set)))
             (nconc set '((const simplify)))))))))


;;-----------------------------------------------------------------------------

(defun bytecomp-simplify-warning-enabled-p ()
  (cond ((eval-when-compile (fboundp 'byte-compile-warning-enabled-p))
         (byte-compile-warning-enabled-p 'simplify))

        ;; emacs21 doesn't have `byte-compile-warning-enabled-p'
        ((eq t byte-compile-warnings)
         t)
        ((eq 'not (car byte-compile-warnings))
         (not (memq 'simplify (cdr byte-compile-warnings))))
        (t
         (memq 'simplify byte-compile-warnings))))

(defun bytecomp-simplify-warn (form)
  "Warn about possible code simplifications in FORM.
A check is made of a plain call (FOO ...), anything else is
ignored.  Just the FOO call is checked, not any calls within
argument expressions."
  (when (bytecomp-simplify-warning-enabled-p)
    (let ((fn (car-safe form)))
      (when (symbolp fn)
        (let ((handler (get fn 'bytecomp-simplify-warn)))
          ;; func or list of functions
          (run-hook-with-args 'handler fn form))))))

;; Is it better to look at `byte-compile-form', or after macro expansions in
;; `byte-compile-normal-call'?  Macros might produce naive code with
;; simplifications which are not interesting, or on the other hand things in
;; the macro expander might in fact have genuine simplifications possible.

;; (defadvice byte-compile-form (before bytecomp-simplify activate)
;;   "Notice simplifications."
;;   ;; (message "form simplify: %S" form)
;;   (bytecomp-simplify-warn form))

(defadvice byte-compile-normal-call (before bytecomp-simplify activate)
  "Notice simplifications."
  (bytecomp-simplify-warn form))

;; emacs23 `char-before'
(defadvice byte-compile-char-before (before bytecomp-simplify activate)
  "Notice simplifications."
  (bytecomp-simplify-warn form))

;; for `char-after'
(defadvice byte-compile-zero-or-one-arg (before bytecomp-simplify activate)
  "Notice simplifications."
  ;; (message "zero-one simplify: %S" form)
  (bytecomp-simplify-warn form))

;; for `char-after' in xemacs21
(defadvice byte-compile-zero-or-one-arg-with-one-extra (before bytecomp-simplify activate)
  "Notice simplifications."
  ;; (message "zero-one-extra simplify: %S" form)
  (bytecomp-simplify-warn form))

(defun bytecomp-simplify-unload-function ()
  (dolist (func '(byte-compile-normal-call
                  byte-compile-char-before
                  byte-compile-zero-or-one-arg
                  byte-compile-zero-or-one-arg-with-one-extra
                  byte-optimize-char-before))
    (when (ad-find-advice func 'before 'bytecomp-simplify)
      (ad-remove-advice   func 'before 'bytecomp-simplify)
      (ad-activate        func)))
  nil) ;; and do normal unload-feature actions too

      
;;-----------------------------------------------------------------------------
;; char-before, char-after

;; APEL poe.el has a bit for mule emacs19 or something when the POS argument
;; to char-before and char-after was mandatory.  Don't think need to worry
;; about that.

(defun bytecomp-simplify-char-beforeafter (fn form)
  (when (equal (cdr form) '((point)))
    (byte-compile-warn "`%S' can be simplified to `(%S)'" form fn)))

(put 'char-before 'bytecomp-simplify-warn 'bytecomp-simplify-char-beforeafter)
(put 'char-after  'bytecomp-simplify-warn 'bytecomp-simplify-char-beforeafter)

;; in emacs21 and xemacs21 `char-before' is byte-optimized to `char-after'
;; before it reaches `byte-compile-form' etc, so catch it before that
(defadvice byte-optimize-char-before (before bytecomp-simplify activate)
  "Notice simplifications."
  ;; (message "byte-optimize-char-before simplify: %S" form)
  (bytecomp-simplify-warn form))


;;-----------------------------------------------------------------------------
;; delete-windows-on

(defconst bytecomp-simplify-delete-windows-on--optarg
  (condition-case nil
      (with-temp-buffer
        (eval '(delete-windows-on))
        t)
    (error nil))
  "Non-nil if `delete-windows-on' buffer arg is optional in this Emacs.")

(put 'delete-windows-on 'bytecomp-simplify-warn
     (lambda (fn form)
       (when (and bytecomp-simplify-delete-windows-on--optarg
                  (equal form '(delete-windows-on (current-buffer))))
         (byte-compile-warn "`%S' can be simplified to `(delete-windows-on)', for Emacs 23 up" form))))


;;-----------------------------------------------------------------------------
;; kill-buffer

(defconst bytecomp-simplify-kill-buffer--optarg
  (condition-case nil
      (with-temp-buffer
        (eval '(kill-buffer))
        t)
    (error nil))
  "Non-nil if `kill-buffer' buffer arg is optional in this Emacs.")

(put 'kill-buffer 'bytecomp-simplify-warn
     (lambda (fn form)
       (when (equal form '(kill-buffer (current-buffer)))
         (if bytecomp-simplify-kill-buffer--optarg
             (byte-compile-warn
              "`%S' can be simplified to `(%s nil), or in Emacs 23 to (%s)'"
              form fn fn)
           (byte-compile-warn "`%S' can be simplified to `(%s nil)" form fn)))

       (when (and bytecomp-simplify-kill-buffer--optarg
                  (equal form '(kill-buffer nil)))
         (byte-compile-warn
          "`%S' can be simplified to `(%s), for Emacs 23 up"
          form fn))))


;;-----------------------------------------------------------------------------
;; search-forward
;; search-backward

(defun bytecomp-simplify-search-limit (fn form)
  (let ((default (if (string-match "forward" (symbol-name fn))
                     '(point-max)
                   '(point-min))))
    (when (equal (nth 2 form) default)
      (byte-compile-warn "`%s' argument %S can be simplified to `nil'"
                         fn (nth 2 form)))))

(put 'search-forward 'bytecomp-simplify-warn
     'bytecomp-simplify-search-limit)
(put 'search-backward 'bytecomp-simplify-warn
     'bytecomp-simplify-search-limit)


;;-----------------------------------------------------------------------------
;; re-search-forward
;; re-search-backward

(defun bytecomp-simplify-regexp-fixed-p (regexp)
  "Return non-nil if REGEXP matches only a fixed string.
All backslashed alphabeticals like \\X are treated as not a fixed
match.  Unknown ones will match only a literal X, but you
shouldn't rely on that in case the regexp engine gets new
specials in the future."
  (string-match (concat "\\`\\("
                        "[^.*+?[^$\\]"     ;; not a special
                        "\\|"              ;; OR
                        "\\\\[.*+?[^$\\]"  ;; a special but backslashed
                        "\\|"              ;; OR
                        "\\[[^^]]"         ;; a char-class of single character
                        "\\)*\\'")
                regexp))

(defun bytecomp-simplify-re-search-fixed (fn form)
  (when (and (stringp (nth 1 form))
             (bytecomp-simplify-regexp-fixed-p (nth 1 form)))
    (byte-compile-warn
     "`%s' with fixed-string regexp can be simplified to `%s'"
     fn (substring (symbol-name fn) 3))))

(put 're-search-forward 'bytecomp-simplify-warn
     '(bytecomp-simplify-re-search-fixed
       bytecomp-simplify-search-limit))
(put 're-search-backward 'bytecomp-simplify-warn
     '(bytecomp-simplify-re-search-fixed
       bytecomp-simplify-search-limit))


;;-----------------------------------------------------------------------------
;; up-list, down-list

(defconst bytecomp-simplify-updown-list--optarg
  (condition-case nil
      (with-temp-buffer
        (insert "()")
        (goto-char (point-min))
        (eval '(down-list))
        t)
    (error nil))
  "Non-nil if `up-list' and `down-list' count arg is optional in this Emacs.")

(defun bytecomp-simplify-updown-list (fn form)
  (when (and bytecomp-simplify-updown-list--optarg
             (equal (cdr form) '(1)))
    (byte-compile-warn "`%S' can be simplified to `(%S)', for Emacs 21 up"
                       form fn)))

(put 'down-list 'bytecomp-simplify-warn 'bytecomp-simplify-updown-list)
(put 'up-list   'bytecomp-simplify-warn 'bytecomp-simplify-updown-list)


;;-----------------------------------------------------------------------------

(provide 'bytecomp-simplify)

;;; bytecomp-simplify.el ends here
