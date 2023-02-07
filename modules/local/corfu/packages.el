;; -*- no-byte-compile: t; -*-
;;; completion/corfu/packages.el

(package! corfu :recipe (:host github :repo "minad/corfu" :files ("*.el" "extensions/*.el")) :pin "b5458a132c678b5fe97b4a7819b9bb1dba31aee2")
(package! cape)
(package! dabbrev)
(when (modulep! +icons)
  (package! kind-icon))
(when (modulep! :os tty)
  (package! corfu-terminal))
