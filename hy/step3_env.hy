#!/usr/bin/env hy

(import [hy.models [HySymbol :as Sym]])
(import sys traceback)
(import [reader [read-str Blank]])
(import [printer [pr-str]])
(import [env [env-new env-get env-set]])

;; read
(defn READ [str]
  (read-str str))

;; eval
(defn eval-ast [ast env]
  ;;(print "eval-ast:" ast (type ast))
  (if
    (symbol? ast)         (env-get env ast)
    (instance? dict ast)  (dict (map (fn [k]
                                       [(EVAL k env) (EVAL (get ast k) env)])
                                     ast))
    (instance? tuple ast) (tuple (map (fn [x] (EVAL x env)) ast))
    (instance? list ast)  (list (map (fn [x] (EVAL x env)) ast))
    True                  ast))

(defn EVAL [ast env]
  ;;(print "EVAL:" ast (type ast) (instance? tuple ast))
  (if (not (instance? tuple ast))
    (eval-ast ast env)

    ;; apply list
    (do
      (setv [a0 a1 a2] [(nth ast 0) (nth ast 1) (nth ast 2)])
      (if
        (none? a0)
        ast

        (= (Sym "def!") a0)
        (env-set env a1 (EVAL a2 env))

        (= (Sym "let*") a0)
        (do
          (setv let-env (env-new env))
          (for [[b e] (partition a1 2)]
            (env-set let-env b (EVAL e let-env)))
          (EVAL a2 let-env))

        ;; apply
        (do
          (setv el (eval-ast ast env)
                f (first el)
                args (rest el))
          (apply f args))))))

;; print
(defn PRINT [exp]
  (pr-str exp True))

;; repl

(def repl-env {'+ +
               '- -
               '* *
               '/ (fn [a b] (int (/ a b)))})

(defn REP [str]
  (PRINT (EVAL (READ str) repl-env)))

(while True
  (try
    (do (setv line (raw_input "user> "))
        (if (= "" line) (continue))
        (print (REP line)))
    (except [EOFError] (break))
    (except [Blank])
    (except []
      (print (.join "" (apply traceback.format_exception (.exc_info sys)))))))
