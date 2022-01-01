;;;; Support for SBCL-specifc features

(in-package sbcl-support)

;; SBCL workaround for redefining constants, see the SBCL manual
(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

