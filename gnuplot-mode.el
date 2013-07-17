;;; gnuplot-mode.el --- Major mode for editing gnuplot scripts

;; Copyright (C) 2010-2012 Mike McCourt
;;
;; Authors: Mike McCourt <mkmcc@astro.berkeley.edu>
;; URL: https://github.com/mkmcc/gnuplot
;; Version: 1.0.0
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

;;; Configuration:

;; Add the following to your .emacs:

;; (require 'gnuplot-mode)

;; ;; specify the gnuplot executable (if other than /usr/bin/gnuplot)
;; (setq gnuplot-program "/sw/bin/gnuplot")

;; ;; automatically open files ending with .gp or .gnuplot in gnuplot mode
;; (add-to-list 'auto-mode-alist
;;              '("\\.\\(gp\\|gnuplot\\)$" . gnuplot-mode) t)

;;; Code:

(defvar gnuplot-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x p") 'gnuplot-run-buffer)
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

;; explicitly define syntax types
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set up indentation
(defun gnuplot-indent-line ()
  "Set indentation in gnuplot buffer.  For most lines, set
indentation to previous level of indentation.  Add additional
indentation for continued plot and splot lines."
  (interactive)

  (let ((indent)
        (offset))
    ; first determine where we should indent
    (save-excursion
      ; set indent to match the first non-whitespace character on the
      ; previous line.
      (end-of-line 0)
      (back-to-indentation)
      (setq indent (current-column))

      ; if it's a continuation of a plot command, indent to reflect
      ; that.
      (when (looking-at "s?pl\\(o?\\|\\(ot\\)?\\)[ \t]+.?")
        (setq offset (length (match-string 0)))
        (end-of-line)
        (backward-char 1)
        (if (looking-at (regexp-quote "\\"))
          (setq indent  (+ indent offset -1)))))

    ; now perform the indent
    (save-excursion
      ; if the line is not already indented properly, delete all the
      ; whitespace at the beginning of the line and pad with (indent)
      ; spaces. ('? ' is the decimal value of ' ')
      (unless (= (current-indentation) indent)
        (beginning-of-line)
        (delete-horizontal-space)
        (insert (make-string indent ? ))))

    ; skip over the indent, if necessary
    (when (< (current-column) indent)
      (back-to-indentation))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function to call gnuplot on the buffer
(defun gnuplot-run-file (file)
  "Runs gnuplot -persist on the file given as an argument.
Gnuplot program is stored in the variable gnuplot-program"
  (let ((gp-exit-status (call-process (if (boundp 'gnuplot-program)
                                          gnuplot-program
                                        "gnuplot")
                                      file
                                      "*gnuplot errors*" nil "-persist")))
    (message "Running gnuplot...")
    (cond
     ((eq gp-exit-status 0)
      (kill-buffer "*gnuplot errors*")
      (message "Running gnuplot... done."))
     (t
      (switch-to-buffer-other-window "*gnuplot errors*")
      (toggle-read-only)
      (local-set-key (kbd "q") (lambda () (interactive) (kill-buffer)))
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define the mode
;;;###autoload
(define-derived-mode gnuplot-mode fundamental-mode
  "Gnuplot"
  "Major mode for editing gnuplot files"
  :syntax-table gnuplot-mode-syntax-table

  ;; other stuff
  (set (make-local-variable 'indent-line-function) 'gnuplot-indent-line)
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "#+\\s-*")

  ;; font lock
  (set (make-local-variable 'font-lock-defaults)
       '(gnuplot-font-lock-keywords))
  (setq show-trailing-whitespace t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'gnuplot-mode)

;;; gnuplot-mode.el ends here
