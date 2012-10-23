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

;; (require 'gnuplot)

;; ;; specify the gnuplot executable (if other than /usr/bin/gnuplot)
;; (setq gnuplot-program "/sw/bin/gnuplot")

;; ;; automatically open files ending with .gp or .gnuplot in gnuplot mode
;; (add-to-list 'auto-mode-alist
;;              '("\\.\\(gp\\|gnuplot\\)$" . gnuplot-mode) t)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; explicitly define syntax types
(defvar gp-math-functions
  '("abs"   "acos"     "acosh"  "arg"   "asin"   "asinh"  "atan" "atan2" "atanh"
    "besj0" "besj1"    "besy0"  "besy1" "ceil"   "cos"    "cosh" "erf"   "erfc"
    "exp"   "floor"    "gamma"  "ibeta" "inverf" "igamma" "imag" "invnorm"
    "int"   "lambertw" "lgamma" "log"   "log10"  "norm"   "rand" "real"
    "sgn"   "sin"      "sinh"   "sqrt"  "tan"    "tanh")
  "Gnuplot math functions.")

(defvar gp-other-functions
  '("gprintf"      "sprintf"    "strlen"  "strstrr" "substr" "strftime"
    "strptime"     "system"     "word"    "words"   "column" "exists"
    "stringcolumn" "timecolumn" "tm_hour" "tm_mday" "tm_min"
    "tm_mon"       "tm_sec"     "tm_wday" "tm_yday" "tm_year" "valid")
  "Gnuplot other functions.")

(defvar gp-reserved-modifiers
  '("axes"   "every" "index"     "title"     "notitle"
    "ps"     "pt"    "pointsize" "pointtype" "linetype"
    "ls"     "lw"    "lt"        "linestyle" "linewidth"
    "smooth" "thru"  "using"     "with")
  "Gnuplot reserved words.")

(defvar gp-other-keywords
  '("term" "xrange" "yrange" "logscale" "out" "output")
  "Gnuplot keywords")

(defvar gp-term-types
  '("dumb" "x11" "postscript" "png" "gif" "enhanced")
  "Gnuplot term types")

(defvar gp-plot-types
  '("lines" "points" "linespoints" "lp" "impulses" "dots" "steps"
    "errorbars" "xerrorbars" "yerrorbars" "xyerrorbars" "boxes"
    "boxerrorbars" "boxxyerrorbars" "candlesticks" "financebars"
    "histeps" "vector")
  "Gnuplot plot styles")

(defvar gp-commands
  '("plot" "splot" "fit" "replot" "set" "unset")
  "Gnuplot commands")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make regexps of the types (faster and less memory)
(defvar gp-math-functions-regexp
  (regexp-opt gp-math-functions 'words))

(defvar gp-other-functions-regexp
  (regexp-opt gp-other-functions 'words))

(defvar gp-reserved-modifiers-regexp
  (regexp-opt gp-reserved-modifiers 'words))

(defvar gp-other-keywords-regexp
  (regexp-opt gp-other-keywords 'words))

(defvar gp-term-types-regexp
  (regexp-opt gp-term-types 'words))

(defvar gp-plot-types-regexp
  (regexp-opt gp-plot-types 'words))

(defvar gp-commands-regexp
  (regexp-opt gp-commands 'words))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; apply font lock commands
(setq gnuplot-font-lock-keywords
  `((,gp-commands-regexp           . font-lock-constant-face)
    (,gp-math-functions-regexp     . font-lock-function-name-face)
    (,gp-other-functions-regexp    . font-lock-constant-face)
    (,gp-reserved-modifiers-regexp . font-lock-type-face)
    (,gp-other-keywords-regexp     . font-lock-preprocessor-face)
    (,gp-term-types-regexp         . font-lock-string-face)
    (,gp-plot-types-regexp         . font-lock-function-name-face)
    ("\$[0-9]+"                    . font-lock-string-face)   ; columns
    ("\\[\\([^]]+\\)\\]"           1 font-lock-string-face))) ; brackets
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
;; comment function
(defun gnuplot-comment-dwim (arg)
  (interactive "*P")
  (require 'newcomment)
  (let ((deactivate-mark nil)
        (comment-start   "#")
        (comment-end     ""))
    (comment-dwim arg)))
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

  ;; other stuff
  (set (make-local-variable 'indent-line-function) 'gnuplot-indent-line)
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-column) 32)
  (set (make-local-variable 'comment-start-skip) "#[ \t]*")

  ;; fix the syntax table
  (modify-syntax-entry ?*  "."    gnuplot-mode-syntax-table)
  (modify-syntax-entry ?+  "."    gnuplot-mode-syntax-table)
  (modify-syntax-entry ?-  "."    gnuplot-mode-syntax-table)
  (modify-syntax-entry ?/  "."    gnuplot-mode-syntax-table)
  (modify-syntax-entry ?%  "."    gnuplot-mode-syntax-table)
  (modify-syntax-entry ?'  "\""   gnuplot-mode-syntax-table)
  (modify-syntax-entry ?`  "w"    gnuplot-mode-syntax-table)
  (modify-syntax-entry ?_  "w"    gnuplot-mode-syntax-table)
  (modify-syntax-entry ?#  "< b"  gnuplot-mode-syntax-table)
  (modify-syntax-entry ?\n "> b"  gnuplot-mode-syntax-table)

  ;; font lock
  (setq font-lock-defaults '((gnuplot-font-lock-keywords)))
  (setq show-trailing-whitespace t)

  ;; clear variables to save memory
  (setq gp-math-functions nil)
  (setq gp-other-functions nil)
  (setq gp-reserved-modifiers nil)
  (setq gp-plot-types nil)
  (setq gp-commands nil)

  ;; apply keybindings
  (local-set-key (kbd "C-x p")   'gnuplot-run-buffer)
  (local-set-key (kbd "C-c C-c") 'comment-region)
  (local-set-key (kbd "C-c C-u") 'uncomment-region)
  (define-key gnuplot-mode-map [remap comment-dwim] 'gnuplot-comment-dwim))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'gnuplot-mode)

;;; gnuplot-mode.el ends here
