;;; vanilla/early-init.el -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

;; Fix for ld: library not found for -lemutls_w libgccjit.so: error: error invoking gcc driver
(setenv "LIBRARY_PATH" "/opt/homebrew/opt/gcc/lib/gcc/12:/opt/homebrew/opt/libgccjit/lib/gcc/12:/opt/homebrew/opt/gcc/lib/gcc/12/gcc/aarch64-apple-darwin21/12")
