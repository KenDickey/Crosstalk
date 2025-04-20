#!r6rs
;;; File: "st-fundamental-classes.ss"
;;; IMPLEMENTS: Fundamental Smalltalk Class which surround the core
;;;  UndefinedObject, Boolean, True, False
;;; LANGUAGE: Scheme (R6RS; Chez Scheme)
;;; AUTHOR: Ken Dickey
;;; DATE: April 2025

;; (load "st-core-classes.ss")
;; (load "st-core-methods.ss")


(define Boolean
  ($::: Object 'newSubclassName:iVars:cVars:
	'Boolean
	st-nil
	st-nil))

(define True
  ($::: Boolean 'newSubclassName:iVars:cVars:
	'True
	st-nil
	st-nil))
(rebase-mdict! UndefinedObject st-true-behavior)

(define False
  ($::: Boolean 'newSubclassName:iVars:cVars:
	'False
	st-nil
	st-nil))
(rebase-mdict! UndefinedObject st-false-behavior)



;;;			--- E O F ---			;;;
