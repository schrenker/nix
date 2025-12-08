;;; early-init.el --- Early Init File -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:
;; Early init file. Here I disable built-in package manager in favor of elpaca, and set additional paths for native-comp dependencies.

;;; Code:

(setq package-enable-at-startup nil)

;; Fix for ld: library not found for -lemutls_w libgccjit.so: error: error invoking gcc driver
;; (when (eq system-type 'darwin)
;;   (customize-set-variable 'native-comp-driver-options '("-Wl,-w"))
;;   (setq native-comp-async-jobs-number 8)
;;   ;; (setenv "LIBRARY_PATH" "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib")
;;   ;; (setenv "LIBRARY_PATH"
;;   ;;         (let*
;;   ;;             ((base "/opt/homebrew/opt/gcc/lib/gcc/")
;;   ;;              (version (car (directory-files base nil "[^a-zA-Z.]" nil 1)))
;;   ;;              (macversion (car (directory-files (concat base version "/gcc/") nil "[^.]" nil 1)))
;;   ;;              (gcc (concat base version))
;;   ;;              (libgccjit (concat "/opt/homebrew/opt/libgccjit/lib/gcc/" version))
;;   ;;              (macgcc (concat base version "/gcc/" macversion "/" version)))
;;   ;;           (string-join (list gcc libgccjit macgcc) ":")))
;;   )

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(provide 'early-init)
;;; early-init.el ends here.
