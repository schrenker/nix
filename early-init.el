;;; early-init.el --- Early Init File -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:
;; Early init file. Here I disable built-in package manager in favor of elpaca, and set additional paths for native-comp dependencies.

;;; Code:

(setq package-enable-at-startup nil)

;; Fix for ld: library not found for -lemutls_w libgccjit.so: error: error invoking gcc driver
(when (eq system-type 'darwin)
  (customize-set-variable 'native-comp-driver-options '("-Wl,-w"))
  (setenv "LIBRARY_PATH" "/opt/homebrew/opt/gcc/lib/gcc/12:/opt/homebrew/opt/libgccjit/lib/gcc/12:/opt/homebrew/opt/gcc/lib/gcc/12/gcc/aarch64-apple-darwin21/12"))

(provide 'early-init)
;;; early-init.el ends here.
