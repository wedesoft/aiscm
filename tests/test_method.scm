;; AIscm - Guile extension for numerical arrays and tensors.
;; Copyright (C) 2013, 2014, 2015, 2016, 2017, 2018 Jan Wedekind <jan@wedesoft.de>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
(use-modules (srfi srfi-26)
             (srfi srfi-64)
             (oop goops)
             (aiscm asm)
             (aiscm jit)
             (aiscm bool)
             (aiscm int)
             (aiscm obj)
             (aiscm method))


(test-begin "aiscm method")

(define ctx (make <context>))

(define main                (dynamic-link))
(define guile-aiscm-tests   (dynamic-link "libguile-aiscm-tests"))

(define cabs (dynamic-func "abs" main))
(define jit-side-effect     (dynamic-func "jit_side_effect"     guile-aiscm-tests))
(define jit-constant-fun    (dynamic-func "jit_constant_fun"    guile-aiscm-tests))
(define jit-subtracting-fun (dynamic-func "jit_subtracting_fun" guile-aiscm-tests))
(define jit-seven-arguments (dynamic-func "jit_seven_arguments" guile-aiscm-tests))
(define jit-boolean-not     (dynamic-func "jit_boolean_not"     guile-aiscm-tests))

(let [(method (make-native-method <int> '() jit-constant-fun))]
  (test-equal "Query function pointer of method"
    jit-constant-fun (function-pointer method))
  (test-eq "Query return type of method"
    <int> (return-type method))
  (test-assert "Query argument list of constant method"
    (null? (argument-types method)))
  (test-eqv "Compile method call to function returning constant value"
    42 ((jit ctx '() (const (make-native-function (make-native-method <int> '() jit-constant-fun)))))))
(test-eqv "Compile function call and plus operation to test that caller-saved registers get blocked"
  63 ((jit ctx (list <int>)
           (lambda (x) (+ x (make-native-function (make-native-method <int> (list <int>) jit-constant-fun)))))
      21))
(test-eqv "Compile function call taking two arguments after swapping them"
  2 ((jit ctx (list <int> <int>)
          (lambda (x y) (make-native-function (make-native-method <int> (list <int> <int>) jit-subtracting-fun) y x)))
      5 7))
(test-eqv "Pass result of expression to function call"
  5 ((jit ctx (make-list 3 <int>)
          (lambda (x y z) (make-native-function (make-native-method <int> (list <int> <int>) jit-subtracting-fun) x (+ y z))))
     10 2 3))
(test-eqv "Convert result of expression before passing to native function call"
  5 ((jit ctx (list <int> <obj> <obj>)
          (lambda (x y z) (make-native-function (make-native-method <int> (list <int> <int>) jit-subtracting-fun) x (+ y z))))
     10 2 3))
(test-eqv "Compile function call with seven arguments (requires stack parameters)"
  42 ((jit ctx (list <int> <int>)
           (lambda (a b) (make-native-function (make-native-method <int> (make-list 7 <int>) jit-seven-arguments) a a a a a a b)))
      123 42))
(test-equal "Compile and run native boolean negation function"
  '(#t #f)
  (map (jit ctx (list <bool>) (cut make-native-function (make-native-method <bool> (list <bool>) jit-boolean-not) <>))
       '(#f #t)))
(test-equal "call C standard library abs function"
  42 ((jit ctx (list <int>) (cut make-native-function (make-native-method <int> (list <int>) cabs) <>)) -42))
(test-eq "check type of native value"
  <int> (return-type (native-value <int> 42)))
(test-eqv "check value of native value"
  42 (get (native-value <int> 42)))

(test-end "aiscm method")
