;;;; Support for SBCL-specifc features

(in-package sbcl-support)

;; SBCL hack for defining constants, see the SBCL manual
(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

