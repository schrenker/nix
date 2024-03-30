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

(defun schrenker/kill-buffer--possibly-save (buffer)
    "Ask the user to confirm killing of a modified BUFFER.

If the user confirms, optionally save BUFFER that is about to be
killed, or give a choice of showing diff from saved version."

  (let ((response
         (cadr
          (read-multiple-choice
           (format "Buffer %s modified; kill anyway?"
                   (buffer-name))
           '((?y "kill" "kill buffer without saving")
             (?n "cancel" "exit without doing anything")
             (?s "save and then kill" "save the buffer and then kill it")
             (?d "diff" "diff the buffer with original file" ))
           nil nil (and (not use-short-answers)
                        (not (use-dialog-box-p)))))))
    (cond ((equal response "cancel") nil)
          ((equal response "kill") t)
          ((equal response "diff") (with-current-buffer buffer (diff-buffer-with-file buffer) nil))
          (t (with-current-buffer buffer (save-buffer)) t))))

(defun schrenker/kill-this-buffer ()
  "Kill current buffer without confirmation."
  (interactive) (kill-buffer (current-buffer)))

(defun schrenker/split-and-follow-horizontally ()
  "Split current window down, and then switch to the newly created window."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun schrenker/split-and-follow-vertically ()
  "Split current window right, and then switch to the newly created window."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun schrenker/backward-kill-word ()
  "Backward kill word without changing clipboard contents."
  (interactive)
  (delete-region (point) (progn (forward-word -1) (point))))

(defun schrenker/zoom-frame (&optional amt frame)
  "Increaze FRAME font size by amount AMT. Defaults to selected frame if FRAME is nil, and to 1 if AMT is nil."
  (interactive "p")
  (let* ((frame (or frame (selected-frame)))
         (font (face-attribute 'default :font frame))
         (size (font-get font :size))
         (amt (or amt 1))
         (new-size (+ size amt)))
    (set-frame-font (font-spec :size new-size) t `(,frame))
    (message "Frame's font new size: %d" new-size)))

(defun schrenker/zoom-frame-out (&optional amt frame)
  "Decrease FRAME font size by amount AMT. Defaults to selected frame if FRAME is nil, and to 1 if AMT is nil."
  (interactive "p")
  (schrenker/zoom-frame (- (or amt 1)) frame))

(defmacro schrenker/call-negative (form)
  "Macro for calling any command with negative argument. FORM in this case is function you want called."
  `(let ((current-prefix-arg -1))
     (call-interactively ,form)))

(provide 'solarized-overlay)
;;; solarized-overlay.el ends here.
