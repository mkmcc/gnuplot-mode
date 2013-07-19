;;; gnuplot-mode.el --- Major mode for editing gnuplot scripts

;; Copyright (C) 2010-2013 Mike McCourt
;;
;; Authors: Mike McCourt <mkmcc@astro.berkeley.edu>
;; URL: https://github.com/mkmcc/gnuplot
;; Version: 1.1.0
;; Keywords: gnuplot, plotting

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Defines a major mode for editing gnuplot scripts.  I wanted to keep
;; it simpler than other modes -- just syntax hilighting, indentation,
;; and a command to plot the file.

;; Some of this code is adapted from a more full-featured version by
;; Bruce Ravel (available here http://xafs.org/BruceRavel/GnuplotMode;
;; CC license).

;; Thanks to everyone, including Christopher Gilbreth and Ralph Möritz,
;; for sending suggestions, improvements, and fixes.

;;; Installation:

;; Use package.el. You'll need to add MELPA to your archives:

;; (require 'package)
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Alternatively, you can just save this file and do the standard
;; (add-to-list 'load-path "/path/to/gnuplot-mode.el")

;;; Configuration:

;; If you installed this via `package.el', you should take advantage
;; of autoloading.  You can customize features using `defvar' and
;; `eval-after-load', as illustrated below:
;;
;; ;; specify the gnuplot executable (if other than "gnuplot")
;; (defvar gnuplot-program "/sw/bin/gnuplot")
;;
;; ;; set gnuplot arguments (if other than "-persist")
;; (defvar gnuplot-flags "-persist -pointsize 2")
;;
;; ;; if you want, add a mode hook.  e.g., the following turns on
;; ;; spell-checking for strings and comments and automatically cleans
;; ;; up whitespace on save.
;; (eval-after-load 'gnuplot-mode
;;   '(add-hook 'gnuplot-mode-hook
;;              (lambda ()
;;                (flyspell-prog-mode)
;;                (add-hook 'before-save-hook
;;                          'whitespace-cleanup nil t))))

;; If you installed this file manually, you probably don't want to
;; muck around with autoload commands.  Instead, add something like
;; the following to your .emacs:

;; (require 'gnuplot-mode)
;;
;; ;; specify the gnuplot executable (if other than "gnuplot")
;; (setq gnuplot-program "/sw/bin/gnuplot")
;;
;; ;; set gnuplot arguments (if other than "-persist")
;; (setq gnuplot-flags "-persist -pointsize 2")
;;
;; ;; if you want, add a mode hook.  e.g., the following turns on
;; ;; spell-checking for strings and comments and automatically cleans
;; ;; up whitespace on save.
;; (add-hook 'gnuplot-mode-hook
;;           (lambda ()
;;             (flyspell-prog-mode)
;;             (add-hook 'before-save-hook
;;                       'whitespace-cleanup nil t)))

;;; TODO:
;;  1. the indentation commands use regular expressions, which
;;     probably isn't ideal.  is it possible to rework them to use the
;;     syntax table?
;;
;;  2. (related) the indentation scheme fails to recognize the last
;;     lines of commands if they have a "\" anywhere in them (e.g., in
;;     a string).  this is ok, but not ideal
;;
;;  3. make the gnuplot errors work like a compile buffer.  e.g., have
;;     commands like `next-error' and `previous-error'.  that would be
;;     nice, but it sounds like a lot of work.
;;

;;; Code:

(defvar gnuplot-program "gnuplot"
  "Command to run gnuplot.")

(defvar gnuplot-flags "-persist"
  "Flags to pass to gnuplot.")

(defvar gnuplot-mode-hook nil
  "Hook to run after `gnuplot-mode'.")

(defvar gnuplot-continued-commands-regexp
  (regexp-opt '("splot" "plot" "fit") 'words)
  "Regexp which matches all commands which might continue over
multiple lines.  Used in `gnuplot-find-indent-column' and in
`gnuplot-last-line-p'.")

(defvar gnuplot-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x p")   'gnuplot-run-buffer)
    (define-key map (kbd "C-c C-c") 'gnuplot-run-buffer)
    map)
  "Keymap for `gnuplot-mode'.")

(defvar gnuplot-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?*  "."  st)
    (modify-syntax-entry ?+  "."  st)
    (modify-syntax-entry ?-  "."  st)
    (modify-syntax-entry ?/  "."  st)
    (modify-syntax-entry ?%  "."  st)
    (modify-syntax-entry ?'  "\"" st)
    (modify-syntax-entry ?`  "w"  st)
    (modify-syntax-entry ?_  "w"  st)
    (modify-syntax-entry ?#  "<"  st)
    (modify-syntax-entry ?\n ">"  st)
    st)
  "Syntax table for `gnuplot-mode'.")



;; font lock.  first, explicitly define syntax types
(defvar gp-math-functions
  (regexp-opt
   '("abs"     "acos"   "acosh"    "arg"     "asin"
     "asinh"   "atan"   "atan2"    "atanh"   "besj0"
     "besj1"   "besy0"  "besy1"    "ceil"    "cos"
     "cosh"    "erf"    "erfc"     "exp"     "floor"
     "gamma"   "ibeta"  "inverf"   "igamma"  "imag"
     "invnorm" "int"    "lambertw" "lgamma"  "log"
     "log10"   "norm"   "rand"     "real"    "sgn"
     "sin"     "sinh"   "sqrt"     "tan"     "tanh")
   'words)
  "Gnuplot math functions.")

(defvar gp-other-functions
  (regexp-opt
   '("gprintf"      "sprintf"    "strlen"   "strstrr"
     "substr"       "strftime"   "strptime" "system"
     "word"         "words"      "column"   "exists"
     "stringcolumn" "timecolumn" "tm_hour"  "tm_mday"
     "tm_min"       "tm_mon"     "tm_sec"   "tm_wday"
     "tm_yday"      "tm_year"    "valid")
   'words)
  "Gnuplot other functions.")

(defvar gp-reserved-modifiers
  (regexp-opt
   '("axes"   "every" "index"     "title"     "notitle"
     "ps"     "pt"    "pointsize" "pointtype" "linetype"
     "ls"     "lw"    "lt"        "linestyle" "linewidth"
     "smooth" "thru"  "using"     "with")
   'words)
  "Gnuplot reserved words.")

(defvar gp-other-keywords
  (regexp-opt
   '("term" "xrange" "yrange" "logscale" "out" "output")
   'words)
  "Gnuplot keywords")

(defvar gp-term-types
  (regexp-opt
   '("dumb" "x11" "postscript" "png" "gif" "enhanced")
   'words)
  "Gnuplot term types")

(defvar gp-plot-types
  (regexp-opt
   '("lines" "points" "linespoints" "lp" "impulses" "dots" "steps"
     "errorbars" "xerrorbars" "yerrorbars" "xyerrorbars" "boxes"
     "boxerrorbars" "boxxyerrorbars" "candlesticks" "financebars"
     "histeps" "vector")
   'words)
  "Gnuplot plot styles")

(defvar gp-commands
  (regexp-opt
   '("plot" "splot" "fit" "replot" "set" "unset")
   'words)
  "Gnuplot commands")


;; apply font lock commands
(defvar gnuplot-font-lock-keywords
  `((,gp-commands           . font-lock-constant-face)
    (,gp-math-functions     . font-lock-function-name-face)
    (,gp-other-functions    . font-lock-constant-face)
    (,gp-reserved-modifiers . font-lock-type-face)
    (,gp-other-keywords     . font-lock-preprocessor-face)
    (,gp-term-types         . font-lock-string-face)
    (,gp-plot-types         . font-lock-function-name-face)
    ("\$[0-9]+"             . font-lock-string-face)   ; columns
    ("\\[\\([^]]+\\)\\]"    1 font-lock-string-face))) ; brackets



;; indentation
(defun gnuplot-find-indent-column ()
  "Find the column to indent to.

Start with the value `back-to-indentation' gives for the previous
line.  Next, check whether the previous line starts with a plot
command *and* ends with line continuation.  If so, increment the
indent column by the size of the plot command."
  (save-excursion
    ;; start with the indentation of the previous line
    (forward-line -1)
    (back-to-indentation)
    ;; check if there's a plot or fit command and a line
    ;; continuation.  if so, adjust the indentation.
    ;;
    ;; example:
    ;;   plot sin(x) w l,\
    ;;
    ;; we want to indent under "sin", not "plot"
    (let ((indent (current-column))
          (continuation-regexp         ; matches a continued line
           (concat "\\(" gnuplot-continued-commands-regexp "\\s-+" "\\)"
                   ".*" (regexp-quote "\\") "$")))
      (cond
       ((looking-at continuation-regexp)
        (let ((offset (length (match-string 1))))
          (+ indent offset)))
       (t
        indent)))))

(defun gnuplot-last-line-p ()
  "Determine whether we're just after the last line of a
multi-line plot command.  If so, we don't want to indent to the
previous line, but instead to the beginning of the command.  See
comments for details.

Returns nil if nothing needs to be done; otherwise return the
column to indent to."
  (save-excursion
    ;; check that the previous line does *not* end in a continuation,
    ;; and that the line before it *does*.  if so, we just ended a
    ;; multi-line command.  thus, we should not match indentation of
    ;; the previous line (as above), but the indentation of the
    ;; beginning of the command
    ;;
    ;; example:
    ;;   plot sin(x) w l,\
    ;;        cos(x) w l,\
    ;;        tan(x)
    ;;
    ;; we want to indent to under "plot," not "tan".
    ;;
    ;; TODO: this regexp doesn't work if the last line has a \
    ;;       *anywhere* in it. so if some perverse person puts forward
    ;;       slashes in their plot labels, those lines won't indent
    ;;       properly
    (end-of-line -1)                    ; go back *two* lines
    (forward-char -1)
    (when (looking-at "\\\\
\\s-+[^\\\\
]+$")
      (when (re-search-backward gnuplot-continued-commands-regexp nil t)
        (current-column)))))

(defun gnuplot-indent-line ()
  "Indent the current line.

See `gnuplot-find-indent-column' for details."
  (interactive)

  (let ((indent
         ; check last-line-p first!
         (or (gnuplot-last-line-p)
             (gnuplot-find-indent-column))))
    (save-excursion
      (unless (= (current-indentation) indent)
        (beginning-of-line)
        (delete-horizontal-space)
        (insert (make-string indent ? ))))

    (when (< (current-column) indent)
      (back-to-indentation))))



;; function to call gnuplot on the buffer
(defun gnuplot-run-file (file)
  "Runs gnuplot synchronously.

Run gnuplot as `gnuplot-program', operating on FILE, with the
arguments stored in `gnuplot-flags'.  Store the output in the
buffer *gnuplot errors*, and raise it if gnuplot returns an exit
code other than zero.  Hitting 'q' inside the *gnuplot errors*
buffer kills the buffer and restores the previous window
configuration."
  (let ((gp-exit-status (call-process gnuplot-program file
                                      "*gnuplot errors*" nil gnuplot-flags)))
    (message "Running gnuplot...")
    (cond
     ((eq gp-exit-status 0)
      (kill-buffer "*gnuplot errors*")
      (message "Running gnuplot... done."))
     (t
      (window-configuration-to-register :gnuplot-errors)
      (switch-to-buffer-other-window "*gnuplot errors*")
      (read-only-mode)
      (local-set-key (kbd "q")
                     (lambda () (interactive)
                       (kill-buffer)
                       (jump-to-register :gnuplot-errors)))
      (message "Gnuplot encountered errors.")))))

;;;###autoload
(defun gnuplot-run-buffer ()
  "Runs gnuplot -persist as a synchronous process and passes the
current buffer to it.  Buffer must be visiting a file for it to
work."
  (interactive)
  (if (or (buffer-modified-p) (eq (buffer-file-name) nil))
    (message "buffer isn't saved")
    (gnuplot-run-file (buffer-file-name))))



;;;###autoload
(define-derived-mode gnuplot-mode fundamental-mode
  "Gnuplot"
  "Major mode for editing gnuplot files"
  :syntax-table gnuplot-mode-syntax-table

  ;; indentation
  (set (make-local-variable 'indent-line-function) 'gnuplot-indent-line)

  ;; comment syntax for `newcomment.el'
  (set (make-local-variable 'comment-start)      "# ")
  (set (make-local-variable 'comment-end)        "")
  (set (make-local-variable 'comment-start-skip) "#+\\s-*")

  ;; font lock
  (set (make-local-variable 'font-lock-defaults)
       '(gnuplot-font-lock-keywords))
  (setq show-trailing-whitespace t)

  ;; run user hooks
  (run-hooks 'gnuplot-mode-hook))

;;;###autoload
(dolist (pattern '("\\.gnuplot\\'" "\\.gp\\'"))
  (add-to-list 'auto-mode-alist (cons pattern 'gnuplot-mode)))

(provide 'gnuplot-mode)

;;; gnuplot-mode.el ends here
