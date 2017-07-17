;;; When loading, we need 2 passes to accomodate definition-after-use
;;; Could be avoided with FASL format but why bother?
(load "blip.lisp")
(reload)
