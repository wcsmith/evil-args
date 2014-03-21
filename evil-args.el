;;; evil-args.el

;; Author:
;;	Connor Smith

;; This file is NOT part of GNU Emacs.

;; Commentary:
;;
;; evil-args provides evil

(require 'evil)


(defvar evil-arg-openers '("(" "{" "["))
(defvar evil-arg-closers '(")" "}" "]"))
(defvar evil-arg-delimiters '("," ";"))

(setq evil-arg-openers-regexp (regexp-opt evil-arg-openers))
(setq evil-arg-closers-regexp (regexp-opt evil-arg-closers))
(setq evil-arg-delimiters-regexp (regexp-opt evil-arg-delimiters))

(setq evil-arg-search-regexp
      (regexp-opt (append evil-arg-openers
			  evil-arg-closers
			  evil-arg-delimiters)))

(defun evil-args--backward-delimiter (&optional count)
  (let ((begin -1)
	(count (or count 1)))
    (save-excursion
      (while (and (< begin 0)
		  (> count 0))
	;; search backwards for delimiter, opener, or closer
	(if (not (re-search-backward evil-arg-search-regexp nil t))
	    ;; not found
	    (setq begin (- (point-at-bol) 1))
	  ;; found:
	  ;; skip over any matching pairs if necessary
	  (while (looking-at-p evil-arg-closers-regexp)
	    (evil-jump-item)
	    (backward-char))
	  ;; if looking at an opener, stop
	  (if (looking-at-p evil-arg-openers-regexp)
	      (setq begin (point))
            ;; looking at a delimiter: decrement count and check if done
	    (when (and (looking-at-p evil-arg-delimiters-regexp)
		       (<= (setq count (- count 1)) 0))
	      (setq begin (point)))))))
    (if begin (goto-char begin))))

(defun evil-args--forward-delimiter (&optional count)
  (let ((end -1)
	(count (or count 1)))
    (save-excursion
      (while (and (< end 0)
		  (> count 0))
	;; search forward for a delimiter, opener, or closer
	(if (not (re-search-forward evil-arg-search-regexp nil t))
	    ;; not found
	    (setq end (point-at-eol))
	  ;; found:
	  ;; skip over any matching pairs if necessary
	  (backward-char)
	  (while (looking-at-p evil-arg-openers-regexp)
	    (evil-jump-item)
	    (forward-char))
	  ;; if looking at a closer, stop
	  (if (looking-at-p evil-arg-closers-regexp)
	      (setq end (point))
            ;; looking at a delimiter: decrement count and check if done
	    (when (looking-at-p evil-arg-delimiters-regexp)
	      (if (<= (setq count (- count 1)) 0)
		  (setq end (point))
		(forward-char)))))))
    (if end (goto-char end))))


(defun evil-args--backward-arg-no-skip (count)
  (evil-args--backward-delimiter (or count 1))
  (forward-char)
  (when (looking-at " ")
    (forward-char))
  (when (looking-at "\n")
    (evil-next-line)
    (evil-first-non-blank)))

(evil-define-motion evil-backward-arg (count)
  (evil-args--backward-arg-no-skip
   (+ (if (looking-back (concat evil-arg-delimiters-regexp
				"[\t\n ]*")) 1 0) (or count 1))))

(evil-define-motion evil-forward-arg (count)
  (evil-args--forward-delimiter (or count 1))
  (when (not (looking-at-p evil-arg-closers-regexp))
    (forward-char)
    (when (looking-at " ")
      (forward-char))
    (when (looking-at "\n")
      (evil-next-line)
      (evil-first-non-blank))))

(evil-define-text-object evil-inner-arg (count &optional beg end type)
  "Select inner delimited argument."
  (let ((begin (save-excursion (evil-args--backward-arg-no-skip 1) (point)))
        (end (save-excursion (evil-args--forward-delimiter) (point))))
    (evil-range begin end)))

(evil-define-text-object evil-outer-arg (count &optional beg end type)
  "Select a delimited argument."
  (let ((begin nil)
        (end-on-closer nil)
        (end nil))
    (save-excursion
      (evil-args--forward-delimiter)
      (if (member (string (char-after)) evil-arg-delimiters)
          (evil-forward-arg 1)
        (setq end-on-closer t))
      (setq end (point)))
    (save-excursion
      (evil-args--backward-arg-no-skip 1)
      (if (and end-on-closer
	       (not (looking-back (concat evil-arg-openers-regexp
					  "[\t\n ]*"))))
          (progn (evil-args--backward-delimiter)
                 (setq begin (point)))
        (setq begin (point))))
     (evil-range begin end)))

(evil-define-motion evil-jump-out-args (count)
  (setq count (or count 1))
  (while (> count 0)
    (let ((begin -1)
	  (success nil))
      (save-excursion
	(while (< begin 0)
	  (if (not (re-search-backward evil-arg-search-regexp nil t))
	      (setq begin (- (point-at-bol)) 1)
	    (while (looking-at-p evil-arg-closers-regexp)
	      (evil-jump-item)
	      (backward-char))
	    (when (looking-at-p evil-arg-openers-regexp)
	      (setq begin (point))
	      (setq success t))))
	(when success
	  (evil-backward-word-end)
	  (if (not (looking-at evil-arg-closers-regexp))
	      (evil-backward-word-begin)
	    (evil-jump-item)
	    (forward-char))
	  (setq begin (point))))
      (if begin (goto-char begin)))
    (setq count (- count 1))))

  (provide 'evil-args)
