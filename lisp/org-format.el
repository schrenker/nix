;;; org-format.el --- Auto-format org buffers.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Adapted from: https://emacs.stackexchange.com/a/28268

;; Example configuration:
;;
;;   (use-package org-format
;;     :hook (org-mode . org-format-on-save-mode))

;;; Code:

(require 'org)
(require 'thingatpt)

(defgroup org-format nil
  "Automatically format org buffers on save."
  :group 'productivity
  :prefix "org-format-")

(defcustom org-format-blank-lines-before-subheadings 1
  "Number of blank lines between a heading and preceding content.
Only applies to subheadings."
  :group 'org-format
  :type 'integer)

(defcustom org-format-blank-lines-before-first-heading 1
  "Number of blank lines between a heading and preceding content.
Only applies to first level-1 heading in the document, and
supercedes the setting for
`org-format-blank-lines-before-level-1-headings'."
  :group 'org-format
  :type 'integer)

(defcustom org-format-blank-lines-before-level-1-headings 1
  "Number of blank lines between a heading and preceding content.
Only applies to level-1 headings in the document."
  :group 'org-format
  :type 'integer)

(defcustom org-format-blank-lines-before-content 0
  "Number of blank lines after the heading line and any property drawers."
  :group 'org-format
  :type 'integer)

(defcustom org-format-blank-lines-before-meta 0
  "Number of blank lines between headers and subsequent planning & drawers."
  :group 'org-format
  :type 'integer)

(defun org-format--ensure-empty-lines (n)
  (save-excursion
    (goto-char (line-beginning-position))
    (unless (bobp)
      (forward-char -1)
      (let ((start (point)))
        (when (search-backward-regexp (rx (not (any space "\n"))))
          (ignore-errors
            (forward-char 1)
            (delete-region (point) start))))
      (insert (make-string n ?\n)))))

(defun org-format--in-archived-heading-p ()
  (save-excursion
    (when (org-before-first-heading-p)
      (org-forward-heading-same-level 1))
    (let ((tags (org-get-tags)))
      (seq-contains-p tags org-archive-tag))))

(defun org-format--delete-blank-lines ()
  "Modified version of `delete-blank-lines'."
  (beginning-of-line)
  (when (looking-at "[ \t]*$")
    (delete-region (point)
                   (if (re-search-backward "[^ \t\n]" nil t)
                       (progn (forward-line 1) (point))
                     (point-min))))
  ;; Handle the special case where point is followed by newline and eob.
  ;; Delete the line, leaving point at eob.
  (when (looking-at "^[ \t]*\n\\'")
    (delete-region (point) (point-max))))

(defun org-format-all-headings ()
  "Ensure that blank lines exist between headings and their contents."
  (interactive)
  (let ((scope (if (org-format--in-archived-heading-p)
                   ;; archive files can be enormous--just format the heading at
                   ;; point after archiving.
                   'tree
                 'file))
        (seen-first-heading-p))
    (schrenker/org-map-entries (lambda ()
                       ;; Widen so we can see space preceding the current
                       ;; headline.
                       (org-with-wide-buffer
                        (let* ((level (car (org-heading-components)))
                               (headline-spacing (cond
                                                  ((and (equal 1 level) (not seen-first-heading-p))
                                                   (setq seen-first-heading-p t)
                                                   org-format-blank-lines-before-first-heading)
                                                  ((equal 1 level)
                                                   org-format-blank-lines-before-level-1-headings)
                                                  (t
                                                   org-format-blank-lines-before-subheadings))))
                          (org-format--ensure-empty-lines headline-spacing)))

                       (unless (and (fboundp 'org-transclusion-within-transclusion-p)
                                    (org-transclusion-within-transclusion-p))
                         (forward-line 1)
                         (org-format--delete-blank-lines)
                         (org-format--ensure-empty-lines org-format-blank-lines-before-meta)
                         (org-end-of-meta-data t)
                         (org-format--ensure-empty-lines org-format-blank-lines-before-content)))
                     t
                     scope)

    (org-with-wide-buffer
     ;; Clean up trailing whitespace.
     (goto-char (point-max))
     (org-format--delete-blank-lines)

     ;; Format transcluded headings as if they were really there.
     (goto-char (point-min))
     (while (search-forward-regexp (rx bol "#+transclude:") nil t)
       (save-excursion
         (unless (search-forward ":only-content" (line-end-position) t)
           (goto-char (line-beginning-position))
           (org-format--ensure-empty-lines org-format-blank-lines-before-subheadings)))))))

;; NB: Set this higher than the default to avoid interfering with things like
;; org-transclusion, etc.
(defvar org-format-on-save-mode-hook-depth 95)

(define-minor-mode org-format-on-save-mode
  "Minor mode to enable formatting on buffer save in `org-mode'."
  :lighter nil
  (cond
   (org-format-on-save-mode
    (add-hook 'before-save-hook
              (lambda ()
                (org-element-cache-reset)
                (org-format-all-headings))
              org-format-on-save-mode-hook-depth t))
   (t
    (remove-hook 'before-save-hook 'org-format-all-headings t))))

(defun schrenker/org-map-entries (func &optional match scope &rest skip)
  "Call FUNC at each headline selected by MATCH in SCOPE.

FUNC is a function or a Lisp form.  The function will be called without
arguments, with the cursor positioned at the beginning of the headline.
The return values of all calls to the function will be collected and
returned as a list.

The call to FUNC will be wrapped into a `save-excursion' form, so FUNC
does not need to preserve point.  After evaluation, the cursor will be
moved to the end of the line (presumably of the headline of the
processed entry) and search continues from there.  Under some
circumstances, this may not produce the wanted results.  For example,
if you have removed (e.g. archived) the current (sub)tree it could
mean that the next entry will be skipped entirely.  In such cases, you
can specify the position from where search should continue by making
FUNC set the variable `org-map-continue-from' to the desired buffer
position.

MATCH is a tags/property/todo match as it is used in the agenda tags view.
Only headlines that are matched by this query will be considered during
the iteration.  When MATCH is nil or t, all headlines will be
visited by the iteration.

SCOPE determines the scope of this command.  It can be any of:

nil     The current buffer, respecting the restriction if any
tree    The subtree started with the entry at point
region  The entries within the active region, if any
region-start-level
        The entries within the active region, but only those at
        the same level than the first one.
file    The current buffer, without restriction
file-with-archives
        The current buffer, and any archives associated with it
agenda  All agenda files
agenda-with-archives
        All agenda files with any archive files associated with them
\(file1 file2 ...)
        If this is a list, all files in the list will be scanned

The remaining args are treated as settings for the skipping facilities of
the scanner.  The following items can be given here:

  archive    skip trees with the archive tag
  comment    skip trees with the COMMENT keyword
  function or Emacs Lisp form:
             will be used as value for `org-agenda-skip-function', so
             whenever the function returns a position, FUNC will not be
             called for that entry and search will continue from the
             position returned

If your function needs to retrieve the tags including inherited tags
at the *current* entry, you can use the value of the variable
`org-scanner-tags' which will be much faster than getting the value
with `org-get-tags'.  If your function gets properties with
`org-entry-properties' at the *current* entry, bind `org-trust-scanner-tags'
to t around the call to `org-entry-properties' to get the same speedup.
Note that if your function moves around to retrieve tags and properties at
a *different* entry, you cannot use these techniques."
  (unless (and (or (eq scope 'region) (eq scope 'region-start-level))
	       (not (org-region-active-p)))
    (let* ((org-agenda-archives-mode nil) ; just to make sure
	   (org-agenda-skip-archived-trees (memq 'archive skip))
	   (org-agenda-skip-comment-trees (memq 'comment skip))
	   (org-agenda-skip-function
	    (car (org-delete-all '(comment archive) skip)))
	   (org-tags-match-list-sublevels t)
	   (start-level (eq scope 'region-start-level))
	   matcher res
	   org-todo-keywords-for-agenda
	   org-done-keywords-for-agenda
	   org-todo-keyword-alist-for-agenda
	   org-tag-alist-for-agenda
	   org--matcher-tags-todo-only)

      (cond
       ((eq match t)   (setq matcher t))
       ((eq match nil) (setq matcher t))
       (t (setq matcher (if match (cdr (org-make-tags-matcher match)) t))))

      (save-excursion
	(save-restriction
	  (cond ((eq scope 'tree)
		 (org-back-to-heading t)
		 (org-narrow-to-subtree)
		 (setq scope nil))
		((and (or (eq scope 'region) (eq scope 'region-start-level))
		      (org-region-active-p))
		 ;; If needed, set start-level to a string like "2"
		 (when start-level
		   (save-excursion
		     (goto-char (region-beginning))
		     (unless (org-at-heading-p) (outline-next-heading))
		     (setq start-level (org-current-level))))
		 (narrow-to-region (region-beginning)
				   (save-excursion
				     (goto-char (region-end))
				     (unless (and (bolp) (org-at-heading-p))
				       (outline-next-heading))
				     (point)))
		 (setq scope nil)))

	  (if (not scope)
	      (progn

		(setq res
		      (org-scan-tags
		       func matcher org--matcher-tags-todo-only start-level)))
	    ;; Get the right scope
	    (cond
	     ((and scope (listp scope) (symbolp (car scope)))
	      (setq scope (eval scope t)))
	     ((eq scope 'agenda)
	      (setq scope (org-agenda-files t)))
	     ((eq scope 'agenda-with-archives)
	      (setq scope (org-agenda-files t))
	      (setq scope (org-add-archive-files scope)))
	     ((eq scope 'file)
	      (setq scope (and buffer-file-name (list buffer-file-name))))
	     ((eq scope 'file-with-archives)
	      (setq scope (org-add-archive-files (list (buffer-file-name))))))
	    (dolist (file scope)
	      (with-current-buffer (org-find-base-buffer-visiting file)
		(org-with-wide-buffer
		 (goto-char (point-min))
		 (setq res
		       (append
			res
			(org-scan-tags
			 func matcher org--matcher-tags-todo-only)))))))))
      res)))

(provide 'org-format)

;;; org-format.el ends here
