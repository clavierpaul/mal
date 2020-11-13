(def grammar
  ~{:main (capture (some :input))
    :input (choice :gap :form)
    :gap (choice :ws :comment)
    :ws (set " \f\n\r\t,")
    :comment (sequence ";"
                       (any (if-not (set "\r\n")
                                    1)))
    :form (choice :boolean :nil :number :keyword :symbol
                  :string :list :vector :hash-map
                  :deref :quasiquote :quote :splice-unquote :unquote
                  :with-meta)
    :name-char (if-not (set " \f\n\r\t,[]{}()'`~^@\";")
                       1)
    :boolean (sequence (choice "false" "true")
                       (not :name-char))
    :nil (sequence "nil"
                   (not :name-char))
    :number (drop (cmt
                   (capture (some :name-char))
                   ,scan-number))
    :keyword (sequence ":"
                       (any :name-char))
    :symbol (some :name-char)
    :string (sequence "\""
                      (any (if-not (set "\"\\")
                                   1))
                      (any (sequence "\\"
                                     1
                                     (any (if-not (set "\"\\")
                                                  1))))
                      (choice "\""
                              (error (constant "unbalanced \""))))
    :hash-map (sequence "{"
                        (any :input)
                        (choice "}"
                                (error (constant "unbalanced }"))))
    :list (sequence "("
                    (any :input)
                    (choice ")"
                            (error (constant "unbalanced )"))))
    :vector (sequence "["
                      (any :input)
                      (choice "]"
                              (error (constant "unbalanced ]"))))
    :deref (sequence "@" :form)
    :quasiquote (sequence "`" :form)
    :quote (sequence "'" :form)
    :splice-unquote (sequence "~@" :form)
    :unquote (sequence "~" :form)
    :with-meta (sequence "^" :form (some :gap) :form)
    }
  )

(comment

  (peg/match grammar " ")
  # => @[" "]

  (peg/match grammar "; hello")
  # => @["; hello"]

  (peg/match grammar "true")
  # => @["true"]

  (peg/match grammar "false")
  # => @["false"]

  (peg/match grammar "nil")
  # => @["nil"]

  (peg/match grammar "18")
  # => @["18"]

  (peg/match grammar "sym")
  # => @["sym"]

  (peg/match grammar ":alpha")
  # => @[":alpha"]

  (peg/match grammar "\"a string\"")
  # => @["\"a string\""]

  (peg/match grammar "(+ 1 2)")
  # => @["(+ 1 2)"]

  (peg/match grammar "[:a :b :c]")
  # => @["[:a :b :c]"]

  (peg/match grammar "{:a 1 :b 2}")
  # => @{"{:a 1 :b 2}"]

  )

(defn unescape
  [a-str]
  (->> a-str
       (peg/replace-all "\\\\" "\u029e") # XXX: a hack?
       (peg/replace-all "\\\"" "\"")
       (peg/replace-all "\\n" "\n")
       (peg/replace-all "\u029e" "\\")
       string))

(def enlive-grammar
  (let [cg (table ;(kvs grammar))]
    (each kwd [# :comment # XX: don't capture comments
               :boolean :keyword :nil :number
               :symbol
               # :ws # XXX: dont' capture whitespace
              ]
          (put cg kwd
               ~(cmt (capture ,(in cg kwd))
                     ,|{:tag (keyword kwd)
                        :content $})))
    (put cg :string
            ~(cmt (capture ,(in cg :string))
                  ,|{:tag :string
                     # discard surrounding double quotes
                     :content (unescape (slice $ 1 -2))}))
    (each kwd [:deref :quasiquote :quote :splice-unquote :unquote]
          (put cg kwd
               ~(cmt (capture ,(in cg kwd))
                     ,|{:tag :list
                        :content [{:tag :symbol
                                   :content (string kwd)}
                                  ;(slice $& 0 -2)]})))
    (each kwd [:list :vector]
          (put cg kwd
               (tuple # array needs to be converted
                 ;(put (array ;(in cg kwd))
                       2 ~(cmt (capture ,(get-in cg [kwd 2]))
                               ,|{:tag (keyword kwd)
                                  :content (slice $& 0 -2)})))))
    (put cg :hash-map
         (tuple # array needs to be converted
           ;(put (array ;(in cg :hash-map))
                 2 ~(cmt (capture ,(get-in cg [:hash-map 2]))
                         ,|{:tag :hash-map
                            :content (struct ;(slice $& 0 -2))}))))
    (put cg :with-meta
            ~(cmt (capture ,(in cg :with-meta))
                  ,|{:tag :list
                     :content [{:tag :symbol
                                :content "with-meta"}
                               (get $& 1)
                               (get $& 0)]}))
    # tried using a table with a peg but had a problem, so use a struct
    (table/to-struct cg)))

(comment

  (peg/match enlive-grammar "nil")

  (peg/match enlive-grammar "true")

  (peg/match enlive-grammar ":hi")

  (peg/match enlive-grammar "sym")

  (peg/match enlive-grammar "'a")

  (peg/match enlive-grammar "@a")

  (peg/match enlive-grammar "`a")

  (peg/match enlive-grammar "~a")

  (peg/match enlive-grammar "~@a")

  (peg/match enlive-grammar "(a b c)")
  ``
  '@[{:content ({:content "a"
                 :tag :symbol}
                {:content "b"
                 :tag :symbol}
                {:content "c"
                 :tag :symbol})
      :tag :list} "(a b c)"]
  ``

  (peg/match enlive-grammar "(a [:x :y] c)")

  (peg/match enlive-grammar "^{:a 1} [:x :y]")
  ``
  '@[{:content ({:content "with-meta"
                 :tag :symbol}
                {:content ({:content ":x"
                            :tag :keyword}
                           {:content ":y"
                            :tag :keyword})
                 :tag :vector}
                {:content {{:content ":a"
                            :tag :keyword}
                           {:content "1"
                            :tag :number}}
                 :tag :hash-map})
      :tag :list} "^{:a 1} [:x :y]"]
  ``

  (peg/match enlive-grammar ";; hi")

  (peg/match enlive-grammar "[:x ;; hi\n :y]")

  (peg/match enlive-grammar "  7  ")

  (peg/match enlive-grammar "  abc  ")

  (peg/match enlive-grammar "  \nabc  ")

  )

(defn read_str
  [code-str]
  (let [[parsed _] (peg/match enlive-grammar code-str)]
    (when (= (type parsed) :struct)
      parsed)))

(comment

  (read_str "(+ 1 2)")

  (read_str ";; hello")

  (read_str "\"1\"")

  )
