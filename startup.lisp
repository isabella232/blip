;;; When loading, we need 2 passes to accomodate definition-after-use
;;; Could be avoided with FASL format but why bother?
(defvar blip-root "/depot/synthesis/blip/")
(defvar blip-platform "smartos")
(load "blip.lisp")
(reload)
