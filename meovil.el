;;; meovil.el --- Evil actions for meow -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; I want to replicate part of Vi actions in meow. Why not use evil instead? Meow respects emacs as it is much more than evil, integrating perfectly into it. Evil on the other hand changes way too much, to the point where you question if this is even emacs anymore. Yet, I still want to use muscle memory of Vi motions to work in emacs. Thus you will see a lot of garbage code, glued together with duct tape and sticks. This is what it is, and it works for me perfectly. And this is what matters really.

;;; Code:
  (defun schrenker/meow-expand-or-digit-argument (&optional n)
    (interactive)
    (if (and meow--expand-nav-function
             (region-active-p)
             (meow--selection-type))
        (if n (meow-expand n) (meow-expand))
      (meow-digit-argument)))

  (defun schrenker/meow-next-or-expand ()
    (interactive)
    (if (and meow--expand-nav-function
             (region-active-p)
             (meow--selection-type))
        (call-interactively #'meow-next-expand)
      (call-interactively #'meow-next)))

  (defun schrenker/meow-prev-or-expand ()
    (interactive)
    (if (and meow--expand-nav-function
             (region-active-p)
             (meow--selection-type))
        (call-interactively #'meow-prev-expand)
      (call-interactively #'meow-prev)))

  (defun schrenker/meow-left-or-expand ()
    (interactive)
    (if (and meow--expand-nav-function
             (region-active-p)
             (meow--selection-type))
        (call-interactively #'meow-left-expand)
      (call-interactively #'meow-left)))

  (defun schrenker/meow-right-or-expand ()
    (interactive)
    (if (and meow--expand-nav-function
             (region-active-p)
             (meow--selection-type))
        (call-interactively #'meow-right-expand)
      (call-interactively #'meow-right)))

  (defun schrenker/meow-visual ()
    (interactive)
    (meow--select (meow--make-selection '(expand . char) (point) (point))))


  (defun schrenker/meow-yank-forward ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'meow-yank)))


(defun schrenker/meow-append-to-eol ()
  "Go to the end of the line and enter insert mode."
  (interactive)
  (call-interactively #'meow-line)
  (call-interactively #'meow-append))

(defun schrenker/meow-insert-at-bol ()
  "Go to the beginnig of the line and enter insert mode."
  (interactive)
  (call-interactively #'meow-join)
  (call-interactively #'meow-append))

(defun schrenker/meow-join-below ()
  "Join line below to current line"
  (interactive)
  (call-interactively #'meow-next)
  (call-interactively #'meow-join)
  (call-interactively #'meow-kill))

(defun schrenker/meow-smart-append ()
  (interactive)
  (if (eolp)
      (call-interactively #'meow-insert)
    (call-interactively #'meow-append)))

(defun schrenker/meow-find-backwards ()
  (interactive)
  (schrenker/call-negative 'meow-find))

(defun schrenker/meow-till-backwards ()
  (interactive)
  (schrenker/call-negative 'meow-till))

(defun schrenker/meow-search (ARG)
  "Sometimes, when searching for a string that resides within truncated org link, it will add the search string to 'regexp-search-ring' with additional remnants of org link, making further search impossible. This function checks for problematic strings that appear within the car of regexp-search-string, and if they are found, it pops to a previous search string."
  (interactive "P")
  (meow--direction-forward)
  (when (or
         (string-match-p "\\[.?$" (car regexp-search-ring))
         (string-match-p "\\] - ?$" (car regexp-search-ring))
         (string-match-p "\\[file:" (car regexp-search-ring)))
    (meow-pop-search))
  (meow-search ARG))

(defun schrenker/meow-search-backwards (ARG)
  "Sometimes, when searching for a string that resides within truncated org link, it will add the search string to 'regexp-search-ring' with additional remnants of org link, making further search impossible. This function checks for problematic strings that appear within the car of regexp-search-string, and if they are found, it pops to a previous search string."
  (interactive "P")
  (meow--direction-backward)
  (when (or
         (string-match-p "\\[.?$" (car regexp-search-ring))
         (string-match-p "\\] - ?$" (car regexp-search-ring))
         (string-match-p "\\[file:" (car regexp-search-ring)))
    (meow-pop-search))
  (meow-search ARG))


(defun schrenker/meow-change-to-eol ()
  (interactive)
  (call-interactively #'kill-line)
  (call-interactively #'schrenker/meow-smart-append))


(defun schrenker/meow-selection-p ()
  (and (region-active-p)
       (meow--selection-type)))

(defun schrenker/meow-kill-to-bol ()
  (interactive)
  (meow-beginning-of-thing ?l)
  (call-interactively #'meow-kill))


(defun schrenker/meow-change-line ()
  (interactive)
  (meow-inner-of-thing ?l)
  (call-interactively #'meow-change))

(defun schrenker/meow-change-to-bol ()
  (interactive)
  (meow-beginning-of-thing ?l)
  (call-interactively #'meow-change))

(defun schrenker/meow-change ()
  (interactive)
  (if (schrenker/meow-selection-p)
      (call-interactively 'meow-change)
    (set-transient-map schrenker/meow-c nil nil "Meow change command... $0d" 5)))

(defun schrenker/meow-kill ()
  (interactive)
  (if (schrenker/meow-selection-p)
      (call-interactively 'meow-kill)
    (set-transient-map schrenker/meow-d nil nil "Meow delete command... $0d" 5)))



(defvar-keymap schrenker/meow-d
  "d" #'meow-kill-whole-line
  "$" #'kill-line
  "0" #'schrenker/meow-kill-to-bol)

(defvar-keymap schrenker/meow-c
  "c" #'schrenker/meow-change-line
  "$" #'schrenker/meow-change-to-eol
  "0" #'schrenker/meow-change-to-bol)

(provide 'meovil)
;;; meovil.el ends here.
