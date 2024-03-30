;;; general-utils.el --- Utility functions -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; This file contains utilities, that don't follow any particular theme.

;;; Code:
(defun schrenker/remove-from-list-variable ()
  "Remove value from variable list interactively."
  (interactive)
  (let* ((var (intern
               (completing-read "From variable: "
                                (let (symbols)
                                  (mapatoms
                                   (lambda (sym)
                                     (when (and (boundp sym)
                                                (seqp (symbol-value sym)))
                                       (push sym symbols))))
                                  symbols) nil t)))
         (values (mapcar (lambda (item)
                           (setq item (prin1-to-string item))
                           (concat (truncate-string-to-width
                                    (nth 0 (split-string item "\n"))
                                    (window-body-width))
                                   (propertize item 'invisible t)))
                         (symbol-value var)))
         (index (progn
                  (when (seq-empty-p values) (error "Already empty"))
                  (seq-position values (completing-read "Delete: " values nil t)))))
    (unless index (error "Eeek. Something's up"))
    (set var (append (seq-take (symbol-value var) index)
                     (seq-drop (symbol-value var) (1+ index))))
    (message "Deleted: %s" (truncate-string-to-width
                            (seq-elt values index)
                            (- (window-body-width) 9)))))

(defun schrenker/flip-first-two-elements (input)
  "Flip the first two elements of INPUT list."
  (if (and input (cdr input))
      (let ((first (car input))
            (second (cadr input)))
        (setcar input second)
        (setcar (cdr input) first)))
  input)

(defun schrenker/block-undo (fn &rest args)
  "Apply FN to ARGS in such a way that it can be undone in a single step."
  (let ((marker (prepare-change-group)))
    (unwind-protect (apply fn args)
      (undo-amalgamate-change-group marker))))

(defun schrenker/retry-until-success (func max-tries)
  "Run FUNC every second, until non-nil is returned, or MAX-TRIES is reached."
  (let ((counter 0)
        (timer nil))
    (setq timer
          (run-with-timer
           0 1
           (lambda ()
             (if (or (ignore-errors (funcall func)) (> counter max-tries))
                 (cancel-timer timer)
               (setq counter (1+ counter))))))))

(defmacro schrenker/killing-new-buffers (&rest body)
  "Run BODY and kill any buffers that were not already open."
  (declare (debug t))
  (cl-with-gensyms (initial-buffers)
	`(let ((,initial-buffers (buffer-list)))
	   (unwind-protect
	       ,(macroexp-progn body)
	     (dolist (b (buffer-list)) (unless (memq b ,initial-buffers) (kill-buffer b)))))))

(provide 'solarized-overlay)
;;; solarized-overlay.el ends here.
