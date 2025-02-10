;;; meovim.el --- Vim actions for meow -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; I want to replicate part of Vi actions in meow. Why not use evil instead? Meow respects Emacs as it is much more than evil, integrating perfectly into it. Evil on the other hand changes way too much, to the point where you question if this is even Emacs anymore. Yet, I still want to use muscle memory of Vi motions to work in Emacs. Thus you will see a lot of garbage code, glued together with duct tape and sticks. This is what it is, and it works for me perfectly. And this is what matters really.

;;; Code:
(defun schrenker/meow-expand-or-digit-argument (&optional n)
  "Supply digit argument if there is no region selected.
If there is region selected with meow navigation function, then expand it."
  (interactive)
  (if (and meow--expand-nav-function
           (region-active-p)
           (meow--selection-type))
      (if n (meow-expand n) (meow-expand))
    (meow-digit-argument)))

(defun schrenker/meow-next ()
  (interactive)
  (cond ((derived-mode-p 'magit-mode)(call-interactively #'magit-next-line))
        ((derived-mode-p 'dired-mode)(call-interactively #'dired-next-line))
        ((region-active-p) (if (eq (cdr (car meow--selection)) 'line)
                               ;; Select lines. If going with the direction, select more lines, deselect if going against the direction.
                               (if (meow--direction-forward-p)
                                   (call-interactively #'meow-line)
                                 (meow-pop-selection))
                             (call-interactively #'meow-next-expand)))
        (t (call-interactively #'meow-next))))

(defun schrenker/meow-prev ()
  (interactive)
  (cond ((derived-mode-p 'magit-mode)(call-interactively #'magit-previous-line))
        ((derived-mode-p 'dired-mode)(call-interactively #'dired-previous-line))
        ((region-active-p) (if (eq (cdr (car meow--selection)) 'line)
                               ;; Select lines. If going with the direction, select more lines, deselect if going against the direction.
                               (if (meow--direction-backward-p)
                                   (call-interactively #'meow-line)
                                 (meow-pop-selection))
                             (call-interactively #'meow-prev-expand)))
        (t (call-interactively #'meow-prev))))

(defun schrenker/meow-left ()
  (interactive)
  (cond ((derived-mode-p 'eat-mode) (if (schrenker/prompt-line-p)
                                        (eat-self-input 1 'left)
                                      (call-interactively #'meow-left)))
        ((region-active-p)(call-interactively #'meow-left-expand))
        (t (call-interactively #'meow-left))))

(defun schrenker/meow-right ()
  (interactive)
  (cond ((derived-mode-p 'eat-mode) (if (schrenker/prompt-line-p)
                                        (eat-self-input 1 'right)
                                      (call-interactively #'meow-right)))
        ((region-active-p)(call-interactively #'meow-right-expand))
        (t (call-interactively #'meow-right))))

(defun schrenker/meow-visual ()
  "Start selection at point."
  (interactive)
  (meow--select (meow--make-selection '(expand . char) (point) (point))))


(defun schrenker/meow-yank-forward ()
  "Paste after point."
  (interactive)
  (let ((current-prefix-arg '(4)))
      (if (derived-mode-p 'eat-mode)
          (eat-yank)
        (call-interactively 'meow-yank))))

(defun schrenker/meow-yank ()
  "Paste before point."
  (interactive)
  (if (derived-mode-p 'eat-mode)
      (eat-yank)
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
  (if (meow-beacon-mode-p)
      (call-interactively #'meow-beacon-append)
    (call-interactively #'meow-append)))

(defun schrenker/meow-join-below ()
  "Join line below to current line."
  (interactive)
  (meow-end-of-thing ?l)
  (call-interactively #'meow-next)
  (call-interactively #'meow-join)
  (call-interactively #'meow-kill))

(defun schrenker/meow-smart-append ()
  (interactive)
  (if (eolp)
      (if (meow-beacon-mode-p) (call-interactively #'meow-beacon-insert) (call-interactively #'meow-insert))
    (if (meow-beacon-mode-p) (call-interactively #'meow-beacon-append) (call-interactively #'meow-append))))

(defun schrenker/meow-find-backwards ()
  (interactive)
  (schrenker/call-negative 'meow-find))

(defun schrenker/meow-till-backwards ()
  (interactive)
  (schrenker/call-negative 'meow-till))

(defun schrenker/meow-visit ()
  "Meow visit, but with prompt preselection in vertico"
  (interactive)
  (let ((vertico-preselect 'prompt))
    (call-interactively #'meow-visit)))

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
  (when (region-active-p)
    (goto-char (region-beginning)))
  (meow-end-of-thing ?l)
  (call-interactively #'meow-change))

(defun schrenker/meow-kill-to-bol ()
  (interactive)
  (meow-beginning-of-thing ?l)
  (call-interactively #'meow-kill))

(defun schrenker/meow-kill-to-eol ()
  (interactive)
  (meow-end-of-thing ?l)
  (call-interactively #'meow-kill))

(defun schrenker/meow-change-line ()
  (interactive)
  (meow-inner-of-thing ?l)
  (call-interactively #'meow-change))

(defun schrenker/meow-delete-line ()
  (interactive)
  (meow-bounds-of-thing ?l)
  (call-interactively #'meow-kill))

(defun schrenker/meow-change-to-bol ()
  (interactive)
  (meow-beginning-of-thing ?l)
  (call-interactively #'meow-change))

(defun schrenker/meow-copy-to-eol ()
  (interactive)
  (save-excursion
    (when (region-active-p)
      (goto-char (region-beginning)))
    (meow-end-of-thing ?l)
    (call-interactively #'meow-save)))

(defun schrenker/meow-copy-line ()
  (interactive)
  (save-excursion
    (meow-bounds-of-thing ?l)
    (call-interactively #'meow-save)))

(defun schrenker/meow-copy-to-bol ()
  (interactive)
  (save-excursion
    (meow-beginning-of-thing ?l)
    (call-interactively #'meow-save)))

(defun schrenker/meow-delete ()
  "Delete char."
  (interactive)
  (if (derived-mode-p 'eat-mode)
      (eat-self-input 1 ?\C-d)
    (call-interactively 'meow-delete)))


(defun schrenker/meow-change ()
  (interactive)
  (cond ((meow-beacon-mode-p) (call-interactively #'meow-beacon-change))
        ((region-active-p) (call-interactively #'meow-change))
        (t (set-transient-map schrenker/meow-c nil nil "Meow change command... $0c" 5))))

(defun schrenker/meow-kill ()
  (interactive)
  (cond ((meow-beacon-mode-p) (call-interactively #'meow-beacon-kill-delete))
        ((region-active-p) (call-interactively #'meow-kill))
        (t (set-transient-map schrenker/meow-d nil nil "Meow delete command... $0d" 5))))

(defun schrenker/meow-copy ()
  (interactive)
  (cond ((region-active-p) (call-interactively #'meow-save))
        (t (set-transient-map schrenker/meow-y nil nil "Meow copy command... $0y" 5))))

(defvar-keymap schrenker/meow-d
  "d" #'schrenker/meow-delete-line
  "$" #'schrenker/meow-kill-to-eol
  "0" #'schrenker/meow-kill-to-bol)

(defvar-keymap schrenker/meow-c
  "c" #'schrenker/meow-change-line
  "$" #'schrenker/meow-change-to-eol
  "0" #'schrenker/meow-change-to-bol)

(defvar-keymap schrenker/meow-y
  "y" #'schrenker/meow-copy-line
  "$" #'schrenker/meow-copy-to-eol
  "0" #'schrenker/meow-copy-to-bol)

(add-to-list 'meow--eldoc-commands 'schrenker/meow-left)
(add-to-list 'meow--eldoc-commands 'schrenker/meow-right)
(add-to-list 'meow--eldoc-commands 'schrenker/meow-prev)
(add-to-list 'meow--eldoc-commands 'schrenker/meow-next)
(add-to-list 'meow--eldoc-commands 'schrenker/meow-append-to-eol)
(add-to-list 'meow--eldoc-commands 'schrenker/meow-smart-append)
(add-to-list 'meow--eldoc-commands 'schrenker/meow-insert-at-bol)

(provide 'meovim)
;;; meovim.el ends here.
