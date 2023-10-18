(cl-defun prism-set-colors
    (&key shuffle save local
          (num prism-num-faces) (colors prism-colors)
          (attribute prism-color-attribute)
          (desaturations prism-desaturations) (lightens prism-lightens)
          (comments-fn (lambda (color)
                         (--> color
                           (color-desaturate-name it 30)
                           (color-lighten-name it -10))))
          (strings-fn (lambda (color)
                        (--> color
                          (color-desaturate-name it 20)
                          (color-lighten-name it 10))))
          (parens-fn (lambda (color)
                       (prism-blend color (if (eq emacs-appearance 'light) "#fdf4e3" "#002b36" ) 0.5))))
  "Set `prism' faces.  Call after loading a new theme.
Call also when COLORS has been set to a list of faces and those
faces have been modified.

NUM is the number of faces to set, i.e. the depth to make faces
for.

When SAVE is non-nil, save attributes to `prism-' customization
options for future use by default.

When LOCAL is t (interactively, with one universal prefix), remap
faces buffer-locally; when `reset' (interactively, with two
prefixes), clear local remapping and don't set any faces; when
nil (the default), set faces globally.

COLORS is a list of one or more color name strings (like
\"green\" or \"#ff0000\") or face symbols (of which the
foreground color is used).

DESATURATIONS and LIGHTENS are lists of integer percentages
applied to colors as depth increases; they need not be as long as
NUM, because they are extrapolated automatically.

COMMENTS-FN, PARENS-FN, and STRINGS-FN are functions of one
argument, a color name or hex RGB string, which return the color
having been modified as desired for comments, parens, or strings,
respectively."
  (declare (indent defun))
  (interactive)
  (when (called-interactively-p 'any)
    (setf local (pcase current-prefix-arg
                  ('(16) 'reset)
                  ('(4) t))))
  (when shuffle
    (setf colors (prism-shuffle colors)))
  ;; MAYBE: Extrapolate desaturations and lightens cleverly, instead
  ;; of requiring the user to call `prism-extrapolate'.
  (cl-labels ((faces (colors &optional suffix (fn #'identity))
                     (setf suffix (if suffix
                                      (concat "-" suffix)
                                    ""))
                     (cl-loop for i from 0 below num
                              for face = (intern (format "prism-level-%d%s" i suffix))
                              for color = (funcall fn (nth i colors))
                              for description = (format "`prism' face%s #%d" suffix i)
                              do (set-face face attribute color description)
                              collect (cons i face)))
              (set-face (face attribute color description)
                        (pcase local
                          ('nil
                           (when (internal-lisp-face-p face)
                             ;; Delete existing face, important if e.g. changing :foreground to :background.
                             (face-spec-set face nil 'customized-face))
                           (custom-declare-face face '((t)) description :group 'prism-faces)
                           (set-face-attribute face nil attribute color))
                          ('reset (reset-face face))
                          (_ (face-remap-add-relative face (list attribute color)))))
              (reset-face (face)
                          (--when-let (alist-get face face-remapping-alist)
                            (face-remap-remove-relative (cons (-last-item it) (car (butlast it)))))))
    (let* ((colors (->> colors
                     (--map (pcase-exhaustive it
                              ((pred facep) (face-attribute it :foreground nil 'default))
                              ((pred stringp) it)
                              ((pred functionp) (funcall it))
                              (`(themed ,color) (prism-theme-color color))))
                     (--remove (string-prefix-p "unspecified-" it))
                     -cycle
                     (prism-modify-colors :num num
                                          :desaturations desaturations
                                          :lightens lightens
                                          :colors)
                     ;; Use only two digits per component.  HTML export of code (e.g. with Org
                     ;; Export, htmlize, etc.)  doesn't work well with colors like "#01234567890a",
                     ;; even if Emacs can handle them internally.  Maybe it's Web browsers that
                     ;; can't handle them.  Anyway, we shouldn't use them if it breaks that.
                     (--map (--> (color-name-to-rgb it)
                              (-let (((r g b) it))
                                (color-rgb-to-hex r g b 2)))))))
      (cl-macrolet ((set-vars (&rest pairs)
                              `(progn
                                 ,@(cl-loop for (var val) on pairs by #'cddr
                                            collect `(pcase local
                                                       ('nil  ;; Set global faces.
                                                        (set ',var ,val))
                                                       ('reset  ;; Clear local remappings.
                                                        ,val)
                                                       (_  ;; Remap locally.
                                                        (set (make-local-variable ',var) ,val)))))))
        (set-vars prism-faces (faces colors)
                  prism-faces-strings (faces colors "strings" strings-fn)
                  prism-faces-comments (faces colors "comments" comments-fn)
                  prism-faces-parens (faces colors "parens" parens-fn)))
      (when (and save (not local))
        ;; Save arguments for later saving as customized variables,
        ;; including the unmodified (but shuffled) colors.
        (setf prism-colors colors
              prism-desaturations desaturations
              prism-lightens lightens
              prism-num-faces num
              prism-comments-fn comments-fn
              prism-strings-fn strings-fn
              prism-parens-fn parens-fn)
        (prism-save-colors)))))
