(ns ai.logical.hacker)



(def execute-bug-classifier
  (fn [& args]))
(def prev-program-bindings
  (fn [& args]))
(def execute
  (fn [& args]))
(def refresh-program
  (fn [& args]))



;;; UTILITY FUNCTIONS
(defn successful-return?
  "Returns true if the state is not in error mode, and if (pred
return-val) is true."
  ([state] (successful-return? state (comp not not)))
  ([state pred]
     (and (not (:error state))
          (pred (:return state)))))



(defn rounded-brackets
  "Converts all sequences to lists."
  [x]
  (if-not (coll? x) x
          (map rounded-brackets x)))


(defn exchange-keyvals
  "Returns a new map with keys and vals interchanged."
  [m]
  (zipmap (vals m) (keys m)))

(defn alter-val
  "Alters the map m so that the key k is associated with (f current-val & args)."
  [m k f & args]
  (assoc m k (apply f (get m k) args))) 

(defn alter-val-in
  "Alters the map m so that the key sequence ks is associated with (f current-val & args)."
  [m [k & ks] f & args]
  (assoc-in m (cons k ks) (apply f (get-in m (cons k ks)) args))) 


(defn filter-keys
  "Returns a map with only keys matching pred."
  [pred m]
  (into {}
        (filter (comp pred key) m)))
  
  
;;;; CONSTANTS AND STATEFUL DATABASES


(def ^:const !unsatisfied-prerequisite '!unsatisfied-prerequisite)
(def ^:const !fatal-error '!fatal-error)
(def ^:const !unknown-goal '!unknown-goal)
(def ^:const !failed-conditional '!failed-conditional)



(def answer-library (atom [])) 
(def hacker-notebook (atom [])) 
(def blocksworld-knowledge (atom [])) 
(def programming-techniques (atom [])) 
(def patch-types (atom [])) 


(def ^:const default-answer-library
  (vector
   '[to [make [on :a :b]]
     [program make-on
      [line :line1 [put-on :a :b]]]]

   '[to [test [place-for :x :y]]
     [program wrapper-test-place-for
      [line :line2 [test-place-for :x :y]]]]
   
   '[to [test [cleared-top :x]]
     [program wrapper-test-cleared-top
      [line :line3 [test-cleared-top :x]]]]

   '[to [test [not [= :x :y]]]
     [program wrapper-test-inequality
      [line :line4 [test-not= :x :y]]]]
   
   '[to [test [on :x :y]]
     [program wrapper-test-on
      [line :line4 [test-on :x :y]]]]

   '[to [test [possibly :condition]]
     [program wrapper-possibly
      [line :line4 [possibly :condition]]]]

   ))

(def ^:const default-hacker-notebook
  (vector
   '[goal make-on [make [on :a :b]]]
   '[purpose :line1 [make [on :a :b]] make-on]

   ))

(def ^:const default-blocksworld-knowledge
  (vector

   ;; To put x on y, there must be a place for x on y.
   '[fact [prerequisite [put-on :x :y] [place-for :x :y]]]

   ;; An :expression requires the top of :object to be cleared
   ;; if executing the :expression causes the :object to move.
   ;; ("have" is an existential quantifier.)
   '[fact [prerequisite :expr [cleared-top :object]]
     [have [] [moves :expr :object]]]

   ;; Executing [put-on :x :y] causes :x to move.
   '[fact [moves [put-on :x :y] :x]]

   ;; [cleared-top :x] means that there is no object :y on :x.
   '[fact [meaning-of [cleared-top :x]
           [not [exists [:y] [on :y :x]]]]]

   ;; To show that x is not on y, it is enough to show that x is on
   ;; some different object z.
   '[fact [suffices-for [not [on :x :y]]
           [exists [:z] [not [= :z :y]] [on :x :z]]]]
     
   ))

(def ^:const default-programming-techniques
  (list

   ;; A test for [not [= :x :y]] is just [not [= :x :y]]
   '[fact [code [test [not [= :x :y]]]
           [not [= :x :y]]]]

   ;; A test for whether an expression is possible is to check whether
   ;; its negation is protected.
   '[fact [code [test [possibly :expr]]
           [not [protected? [not :expr]]]]]

   
   ;; If an argument has a synonymous meaning, you can replace the
   ;; argument with its synonym.
   '[fact [code [:function :argument] :script]
     [have [:meaning] [meaning-of :argument :meaning]
      [replace-code :script [:function :meaning]]]]

   ;; If an argument has a sufficient condition, you can replace the
   ;; argument with the sufficient condition.
   '[fact [code [:function :argument] :script]
     [have [:condition] [suffices-for :argument :condition]
      [replace-code :script [:function :condition]]]]


   
   ;; To remove all instances of variables matching pattern, iterate
   ;; over all variables matching pattern, and make them not-pattern.
   '[fact [code [achieve [not [exists :vars :expr]]]
           [until :vars [cannot [assign :vars :expr]]
            [make [not :expr]]]]]

   ;; To make one of the objects satisfy a condition, choose one of
   ;; the objects for which it is possibly to satisfy the condition,
   ;; then make it satisfy the condition.
   '[fact [code [make [exists :vars :qualification :action]]
           [choose :vars 
            [and
             [test :qualification]
            [test [possibly :action]]]
            [make :action]]]
     ;;[replace-code :universe (selector :vars)]

     ]


   
            )

  )

(def ^:const default-patch-types
  (list
   '[fact [patch [!prerequisite-missing :program :line# :prerequisite]
           [add-newline! :program
            [achieve :prerequisite]
            :line#
            ]]]

   ))




(defn restart! []
  (swap! answer-library (constantly default-answer-library)) 
  (swap! hacker-notebook (constantly default-hacker-notebook)) 
  (swap! blocksworld-knowledge (constantly default-blocksworld-knowledge))
  (swap! programming-techniques (constantly default-programming-techniques))
  (swap! patch-types (constantly default-patch-types))
  ) 





;;;;;;; FUNCTION POINTER MANIPULATION


(defn current-line [state]
  (let [[program-name line# [& lines]] (peek (:callstack state))]
    (nth lines line#)))

(defn prev-line [state]
  (let [[program-name line# [& lines]] (peek (:callstack state))]
    (nth lines (dec line#))))

(defn prev-program-name [state]
  (let [[program-name line# [& lines]] (peek (:callstack state))]
    program-name))

(def prev-program (comp peek :callstack))

(defn goto-next-line [state]
  (let [stack (:callstack state)
        [program-name line# [& lines]] (peek stack)
        ]
    ;; TODO: perhaps clear the "return" and/or "error" flags.
    
    (alter-val state :callstack
               (fn [stack]
                 (conj (pop stack) [program-name (inc line#) lines])))))


(defn end-of-line? [state]
  (let [[program-name line# [& lines]] (peek (:callstack state))]

    (<= (count lines) line#)

    ))





;;;;;;;;; PATTERN-MATCHING


(def variable? keyword?)

(defn merge-bindings
  [bind-1 bind-2]
  (when (and bind-1 bind-2)

    (loop [b1 bind-1 b2 bind-2 accumulate nil]
      (cond
       (empty? b1)
       (merge b2 accumulate)
       (empty? b2)
       (merge b1 accumulate)

       :else
       (let [k (first (keys b1))]
         (when (or (not (get b2 k))
                   (= (get b1 k) (get b2 k)))
           (recur (dissoc b1 k)
                  (dissoc b2 k)
                  (assoc accumulate k (get b1 k)))))))))
              

(defn pattern-match
  "Match two s-expressions, returning a map of bindings. Pattern
  variables are indicated by keywords."

  ([ptn expr]
     (pattern-match ptn expr {}))

  ([ptn expr bindings]

     (cond   
      (and (coll? ptn) (coll? expr))
      (if (or (empty? ptn) (empty? expr))
        bindings
        (if-let [x (pattern-match (first ptn) (first expr) bindings)]
          (recur (rest ptn) (rest expr) x)))
      
      (variable? ptn)
      (merge-bindings bindings {ptn expr})
       
      (and (not (coll? expr)) (not (coll? ptn)))
      (when (= ptn expr) bindings)
      
      )))



(defn pattern-match*
  "Loosely match two s-expressions, returning a map of
  bindings. Pattern variables are indicated by keywords, or
  single-letter symbols."

  ([ptn expr]
     (pattern-match* ptn expr {}))

  ([ptn expr bindings]
     
     (cond
      (and (coll? ptn) (coll? expr))
      (if (or (empty? ptn) (empty? expr))
        bindings
        (if-let [x (pattern-match* (first ptn) (first expr) bindings)]
          (recur (rest ptn) (rest expr) x)))
      
      (or (variable? ptn) (and (symbol? ptn) (= 1 (count (str ptn)))))
      (merge-bindings bindings {ptn expr})
       
      (and (not (coll? expr)) (not (coll? ptn)))
      (when (= ptn expr) bindings)
      
      )))




(defn pattern-bind [bind expr]
  (if (coll? expr) 
;;    (if (every? variable? expr) expr ;; hack for existential quantifiers
    (map (partial pattern-bind bind) expr)
    ;;)
    (get bind expr expr)))

(defn pattern-bind* [bind expr]
  (if (coll? expr)
    (if (= 'have (first expr))
      (conj (pattern-bind* bind (drop 2 expr)) (second expr) (first expr))
      (map (partial pattern-bind* bind) expr))
    (get bind expr expr)))


(defn pattern-align
  "Attempts to match the pattern with the expression using
  pattern-match. If successful, returns the bound match. If
  unsuccessful, returns nil."
  [ptn expr]
  (when-let [b (pattern-match ptn expr)]
    (pattern-bind b expr)))

(defn pattern-search
  "Returns a list of all expressions in coll which match ptn, having
  been bound."
  [ptn coll]
  (remove nil?
          (map (partial pattern-align ptn) coll)))




;;;;;;;;;;;;;;;;;;;;   SETTING UP BLOCKS WORLD SCENARIOS


(def *table* 'TABLE)

(defn block
  ([name width]
     {:width width :name name})
  ([width]
     (block width (gensym "block"))))


(defn table []
  {:width Double/POSITIVE_INFINITY
   :name *table*})

(defn scene-init
  "Create a new blocks world scenario, containing only a table."
  []
  {:facts []
   :blocks {*table* (table)}})

(defn add-block
  ([scene block]
     (alter-val scene :blocks #(assoc % (:name block) block))))



;;; ----------- primitives




(defn toppings
  "Return a list of names of objects on the given surface."
  [scene surface]
  (filter (fn [expr]
            (when-let
                [[f x y pos] expr]
              (and (= 'on f)
                   (= surface y) ;; todo: reference by name/object
                   )))
          (:facts scene)))


(defn vacancies
  ([scene surface object]
     (let [intervals (vacancies scene surface)
           width (get-in scene [:blocks object :width] 0)
           ]

       
       (->> intervals
            (filter (fn [[a b]] (<= width (- b a))))
            (map (fn [[a b]]
                   (if (Double/isInfinite b) a
                     (/ (+ a b (- width)) 2)
                     ))))))
  ([scene surface]
     (let [toppings (toppings scene surface)
           object-endpoints
           (map (fn [[_ obj _ n]]
                  [n (+ n (get-in scene [:blocks obj :width] 0))])
                (sort-by second toppings))


           left-endpoint (when object-endpoints
                           (apply min 0 (map first object-endpoints)))
           right-endpoint (when object-endpoints
                            (apply max Double/POSITIVE_INFINITY (map second object-endpoints)))


           fenceposts (flatten (concat [0]
                                       object-endpoints
                                       [(get-in scene [:blocks surface :width])]))
           
           ]
    
       (remove (partial apply =) ;; remove zero-width intervals
               (partition 2 fenceposts))

       )))


(defn test-place-for [state object surface]
  (assoc state :return
         (-> state
             :scene
             (vacancies surface object)
             first nil? not)))
  

(defn test-cleared-top [state surface]
  (assoc state :return
         (-> state
             :scene
             (toppings surface)
             empty?)))

(defn test-on [state x y]

  ;; (println "asdf" [x y] (->> state :scene :facts
  ;;                      (some (partial pattern-match ['on x y])) ))
  
  (->> state
       :scene
       :facts
       (filter (partial pattern-match ['on x y]))
       first
       (assoc state :return))
  )



(defn add-newline! [state program-name expr purpose]
  (let [new-line# (keyword (gensym "line"))]

    (swap! answer-library
           (constantly (for [[_ goal [_ name & lines :as program] :as entry] @answer-library]
                         (if-not (= name program-name)
                           entry
                           ['to goal (apply conj ['program] program-name ['line new-line# expr] lines)])
                         )))



    (swap! hacker-notebook #(conj %
                                  ['purpose new-line# expr purpose]))

    (assoc state :return true)
    ))



(defn replace-line-here [expr state]
  (let [
        [_ line# _](current-line state)]

    (let [state*

    (alter-val state :callstack
               (fn [stack]
                 (conj (pop (vec stack))
                       (let [[_ _ lines :as top] (last stack)]
                         (conj (vec (take 2 top))
                               (for [[_line line## goal] lines]
                                 [_line line## (if (= line## line#) expr goal)]))))))]

      ;;(clojure.pprint/pprint (:callstack state*))
      state*


      )))

    
    
  
  
(defn replace-line! [program-name line# expr]
  (swap! answer-library
         (constantly (for [[_to goal [_program name & lines :as program] :as entry] @answer-library]
                       (if-not (= name program-name)
                         entry

                         [_to goal (apply conj [_program name]
                                          (for [l lines]
                                            (if (= line# (second l))
                                              [(first l) (second l) expr] ;;[(first l) (second l) expr]
                                              l
                                              )

                                          ))]


                         
                       )))))




(defn place-for? [scene object surface]
  (first (vacancies scene surface object)))

(defn put-on [state object surface]

  (let [;;_ (println object surface)
        scene (get state :scene)
        tops (toppings scene object)

        ] 

    ;;;(println "PUT ON" object surface)
    (if-let [[_ stuff] (first tops)] ;; if the object has stuff on it
      ;; Can't move an object if it has stuff on it.
      (assoc state :error
             [!unsatisfied-prerequisite ['not ['on stuff object]]])
       
      (if-let [x (first (vacancies scene surface object))]
        (assoc state :scene
               (->
                scene
                ;; remove object from all other surfaces
                (alter-val :facts
                           (partial remove
                                    #(let [[fn x] %]
                                       (and (= 'on fn)
                                    (= x object)))))
                
                ;; place object on new surface
                (alter-val :facts
                           #(conj %
                                  (list 'on object surface x)
            ;;                      (list 'on object surface) ;; TODO: DXH DANGER HERE
                                  ))
                
                ))
        
        (assoc state :error
               [!fatal-error 'ERROR_NO_SPACE_ON_BLOCK])))))
  


(defn execute-choice [state expr]
  (let [[_choose variables condition action] expr
        candidates (->> condition
                        ((eval 'find-all-interpretations) state)
                        reverse ;; TABLE first
                        (filter (partial (eval 'hacker-solve) (:scene state)))
                        (map (comp

                              #(alter-val state :callstack conj
                                          [(gensym "choice") 0
                                           (list ['line (gensym "line")
                                                  %])])
                              
                              #(pattern-bind % action)
                              (partial pattern-match condition))))
        ]
    
    (first candidates)
  
    
    ))







(defn primitive-operator? [expr]
  (->> expr
       first
       #{'possibly
         'put-on
         'test-not=
         'test-place-for
         'test-cleared-top
         'test-on
         'add-newline!
         'until
         'assign
         'cannot
         'choose
         'and
         }))

       
(defn execute-primitive-operator [state [f & args :as expr]]
  (let [;;proceed one line forward in the current subroutine
        state* (goto-next-line state)]

    (cond
     (= f 'and)
     (do
       ;;(println "AND" (vec args))
       (assoc state* :return
              (every? successful-return?
                      (map (partial (eval 'hacker-solve) (:scene state))
                           args)))

     )

     (= f 'not)
     (assoc state* :return
            (not (successful-return?
            (execute (:scene state) (first args)))))

     (= f 'test-not=)
     (assoc state* :return (not (apply = args)))
     
     (= f 'choose)
     (execute-choice state* expr)

     (= f 'possibly)
     (assoc state* :return true) ;;; TODO: Protection mechanism
         
     (= f 'until)
     ((eval 'execute-until-loop) state (cons f args))

     (= f 'assign)
     ((eval 'execute-assign) state (cons f args))

     (= f 'cannot)
     (let [x ((eval 'hacker-solve) (:scene state) (first args))]
       ;;(println "CANNOT" (first args) (:return x) (successful-return? x))
       (if (successful-return? x)
         (assoc state* :error [!failed-conditional (first args)]
                :return (:return x)
                ) state*
                  ))

     
     :else
     (apply (eval f) state* args))
    ;;(apply conj [] f state* args)
  ))




(defn find-matches-in-library
  "Search the library for expressions that pattern-match to [datatype
  goal ...]. Return the list of all bound matches. "
  [library datatype goal]

  (->> library
       (map
        (fn [answer]
          (if-let [ptn (pattern-match (take 2 answer) [datatype goal])]
            (pattern-bind ptn answer))))
       (remove nil?)))


(defn unbound-vars
  "Returns a list of all variables in the expression. Different algorithm, same effect."
  [expr]
  (if (coll? expr)
    (distinct (mapcat unbound-vars expr))
    (when (variable? expr) [expr])
    ))



(defn find-all-interpretations
  "Returns a list of all possible ways to bind the unbound variables
in expr to objects in state, without checking their truth value."
  ([state expr]
     (let [
           vars (unbound-vars expr)
           universe (-> state
                        :scene
                        :blocks
                        keys)
           ;;_ (println "FINDALL"  state universe)
           ]
           ;; scene (:scene state)
           ;; universe (keys (:blocks scene))]
       (loop [open vars accumulated [expr]]

         (if (empty? open)
           accumulated
           (recur
            (rest open)
            (for [e accumulated
                  x universe]
              (pattern-bind {(first vars) x} e))))))))
         
         




(defn print-current-line! [state]
  (let [depth (dec (count (:callstack state)))
        line (nth (current-line state) 2)]
    (println (str
              (apply str (repeat depth \tab))
              (pr-str (rounded-brackets line))

              ))))






(defn execute-assign [state expr]
  (let [[_assign variables expr*] expr
        state* (goto-next-line state)

        candidates (->> expr*
                        (find-all-interpretations state*)
                        (map (comp
                              (partial (eval 'hacker-solve) (:scene state*))
                              (partial vector 'test)))

                        (filter :return))


        candidate (when-let [x (first candidates)]
                    (->> x :return
                         (pattern-match expr*)
                         (filter-keys (set variables))))

        ;;_ (println "ASSIGN" expr* (->> expr* (find-all-interpretations state)))
        
        ]
    (assoc state* :return candidate)
    
    
    ))
  
(defn execute-until-loop [state expr]

  (let [[_until variables condition action] expr
        condition-value ((eval 'hacker-solve) (:scene state) condition)
        ;; candidates (find-all-interpretations state [condition action])
        ;; domain (-> state :scene :blocks keys)
        ;; _ (println "CVAL" condition-value)
        ]



    (if (= !failed-conditional (first (:error condition-value)))
      (do 
        ;;(println "UNTIL's CMD" (pattern-bind (:return condition-value) action))


        (let [p
              (alter-val state :callstack
                         #(conj % [(gensym "loop") 0 [['line (gensym :line) (pattern-bind (:return condition-value) action)]]]))]

          ;;(clojure.pprint/pprint (:callstack p))
          p

          )
        
    
        )

      ;;(assoc (-> state goto-next-line goto-next-line) :error [!fatal-error (current-line (goto-next-line state))] )

      (-> state goto-next-line)
    
      ;;(-> state goto-next-line goto-next-line)
   ;;   (assoc (-> state ) :error [!fatal-error "I don't know how to perform until loops"])

   )))



(defn parse-existential [state goal]
  (if-let [[_ vars expr] goal]

    (let [matching-facts
          (map (comp #(pattern-align % expr) second)
                  @blocksworld-knowledge)]

      (assoc state :return
             (some (comp not nil?) matching-facts)))))

    
    ;; (->> state
    ;;      (assoc :return
    ;;        (filter (comp
    ;;                 #(partial pattern-match % goal)
    ;;                 second
    ;;                 ) @blocksworld-knowledge))
    ;;        empty? not)))
    



(defn thread-in [arg expr]
  (conj (rest expr) arg (first expr))) 

(defn replace-code
  "Replace every instance of key with val in the expression." 
  [expr key val]
  (pattern-bind {key val} expr))

(defn parse-in-environment
  "If the fact has any qualifications or environmental conditions,
  returns a (potentially empty) list of all the ways of making them
  hold. Otherwise, returns a singleton list containing fact."
  [state fact]

  
  (let [[_fact goal & environment] fact]
    (if-let [e (first environment)]
      (let [;;_ (clojure.pprint/pprint ["ENV" fact e] )
            [_have vars condition & exprs :as existential] e
            ]   
        ;; SEARCHING FOR SOLUTIONS TO EXISTENTIAL QUANTIFIER
      (->> @blocksworld-knowledge
           (map (fn [[_fact x :as entry]]
                  
                  (when-let [bind (pattern-match* condition x)]
                    (if-let [e (first exprs)]
                      (let [reverse-bind (exchange-keyvals (apply dissoc bind vars))
                            command (pattern-bind reverse-bind (pattern-bind bind (thread-in goal e)))
                            
                            ;_ (println [condition x (pattern-match* condition x)])
                            ]
                        
                        (apply (eval (first command)) (rest command))
                        
                        )
                      goal)
                    
                    )
                  ))

           (remove nil?)           
           ))

      (do
;;        (println "life" goal)      
        [goal])
        
  )))

(defn execute-code-writer [state]
  (let [[_ _ goal] (current-line state)
        
        solutions 
        ;; SEARCH THE PROGRAMMING TECHNIQUES LIBRARY FOR SOLUTIONS
        (->> @programming-techniques
             (map (fn [[_ [_code goal* & lines ] & environment :as solution]]
                    (when-let [ptn (pattern-match goal* goal)]
                      (pattern-bind ptn solution))))
             (remove nil?)
             (mapcat (partial parse-in-environment state))
             )

        ;;_ (println "--- entering code writer")
        ;;_ (println solutions)
        ]

      (if (empty? solutions)
        (assoc state :error [!fatal-error [!unknown-goal goal]])

        ;; TODO: Try multiple solutions, not just the first.
        (let [variables (prev-program-bindings state)
              general-solution (pattern-bind variables (first solutions))
              [_ _ replacement-code] general-solution

              specific-solution (first solutions) 

              
              ;; _ (clojure.pprint/pprint ["GENERAL SOLUTION"  general-solution (first solutions) (:callstack state)])

              ]

          (replace-line! (prev-program-name state) (second (current-line state)) (nth general-solution 2))


          (->> state
               (refresh-program variables)
               (replace-line-here (nth specific-solution 2))
 ;;              (#(assoc % :callstack []))
               
               execute)
          
          ;; (execute (refresh-program variables state))
          
          )
          ;;(first solutions)
      )
    
  ))

(defn execute
  ([state] (execute state false))
  (
  [state silently?]
 ;; (if (not (end-of-line? state))
   ;; ))


    (if-not (or true  silently?)
      (do
 ;; (println "-----------------")
        ;; (clojure.pprint/pprint state)
        )
        )
  
  (cond
   (:error state)
   (execute-bug-classifier state)
   
   (empty? (:callstack state)) state
   (end-of-line? state) (recur (alter-val state :callstack pop) silently?)
   
   :else
   (let [[program-name line# [& lines]] (peek (get state :callstack))
         goal (nth (current-line state) 2)
         state* (goto-next-line state)
         _ (when (and (not silently?)

                      (#{'make} (first goal))

                      ) (print-current-line! state))
         ]
     (cond (primitive-operator? goal)
           (recur (execute-primitive-operator state goal) silently?)

           (= 'have (first goal))
           (parse-existential state goal)
           
           :else
           ;; find and iterate over all canned subroutines
           ;; for solving this problem
           (let [
                 strategies
                 (->>
                  @answer-library
                  (map
                   (fn [answer]
                     (if-let
                         [ptn (pattern-match (take 2 answer) ['to goal])]
                       (pattern-bind ptn answer))))
                  (remove nil?)


                  (map
                   (fn [[_ _ [_ program-name & lines]]]
                     (alter-val state* :callstack
                                #(conj % [program-name 0 lines]))

                     ))
                  )

                 ]
             (if (empty? strategies)
               ;; TODO: ask the code-writer to code up a solution
               ;; TODO : call exec on this so that it gets
               ;; passed to the debugger before returning
               (execute-code-writer state)
               
               ;; TODO: Iteratively evaluate each strategy,
               ;; returning the first result without a fatal
               ;; error. If all results have fatal errors, throw
               ;; a fatal error.
               (execute (first strategies))

               )
             )
           )

     ))))


(defn hacker-solve
  ([scene expr & exprs]

     (let [state {:scene scene
                  :callstack [['USER_INPUT 0
                               (for [e (cons expr exprs)]
                                 ['line (gensym "line") e])
                               ]]}]
       (execute state)
       

       )))









(defn find-program-by-name [program-name]
  (->> @answer-library
       (map (fn [[_ _ [_ name & _ :as program]]]
              (when (= name program-name)
                program)))
       (some identity)))


(defn refresh-program
  "Replace the top program on the stack with its most up-to-date
  version, and move the function pointer to the start of that
  program."
  [variables state]



  (if-let [[_ program-name & lines]
        (pattern-bind (exchange-keyvals variables)
                      (find-program-by-name (prev-program-name state)))
        ]
  

    (-> state
        (alter-val :callstack
                   (fn [stack]
                     (conj (pop stack) 
                           [program-name 0 (map vec lines)])))

        (dissoc :error)
        )


    state
    ))




(defn prev-program-bindings
  "Return a list of bindings made by the top function pointer on the stack."
  [state]
  (let [program (prev-program state)
        [_ _ & lines]  (find-program-by-name (first program))
        ]
    (pattern-match* (nth program 2) lines)
    ))


(defn execute-patch [state]
  (println "patching error: " (:error state))
  
  ;;(println (prev-program state))
  (let [[bug-type program-name line# arg :as error] (:error state)
        ;; var-program (->> program-name
        ;;                  find-program-by-name
        ;;                  (drop 2)
        ;;                  (list :program-name :line#))
        
        ;; variables
        ;; (-> var-program
        ;;     (pattern-match (prev-program state))
        ;;     (dissoc :program-name :line#)
        ;;     exchange-keyvals)

        variables (prev-program-bindings state)
        
        
        var-error (pattern-bind variables error)
        
        matching-patches
        (->> @patch-types
             (map (fn [answer]
                    (when-let [ptn (pattern-match
                                  (take 2 (second answer))
                                  ['patch var-error])]
                      (pattern-bind ptn answer))))
             (remove nil?)
             
             )

        
        ]

    (if-not variables
      (assoc state :error [!fatal-error "Patcher can't unbind variables."])
      (-> (some identity matching-patches) ;; TODO: search over /all/ patches
          second
          (nth 2)
          ((partial hacker-solve (:scene state)))
          :return
          (when (execute (refresh-program variables state)))
          )
       
        )))

(defn execute-bug-classifier [state]
  (println "error" (:error state))

  (cond
   (= !failed-conditional (first (:error state)))
   state
   
   (= !fatal-error (first (:error state)))
   state
   
   :prerequisite-missing
   (let [[_ line# goal :as badline] (prev-line state)
         matching-prereqs
         (->> @blocksworld-knowledge
              (map (fn [answer]
                     (when-let [ptn (pattern-match 
                                        (take 2 (second answer))
                                        ['prerequisite goal])]
                       (pattern-bind ptn answer)
                       
                            )))
              (remove nil?)
              )

         test-prereqs
         (for [fact* (take 2 matching-prereqs)
               fact (find-all-interpretations state fact*)]
           (let [[_fact [_prereq _ goal*] & preconditions] fact]

             ;; TODO: LESS KLUDGY PRECONDITION CHECK
             (when (or (empty? preconditions)
                       (successful-return? (hacker-solve (:scene state) (first preconditions))))
               [(hacker-solve (:scene state) ['test goal*]) goal*])

             ))      
         ]
       ;; (map (partial take 2) @blocksworld-knowledge))
       
     (if-let [bug-cause (some (fn [[ret goal]] (and (successful-return? ret not) goal))
                              (remove nil? test-prereqs))]

       (execute-patch (assoc state :error ['!prerequisite-missing (prev-program-name state) (second (prev-line state))  bug-cause]))

       (alter-val state :error (partial vector !fatal-error)))
     
       
     ;;state

     )))




(def ^:const default-scene
  (-> (scene-init)
      (add-block (block 'A 1))
      (add-block (block 'B 1))
      (add-block (block 'C 1))
      ))

(defn t1 []
  (let [scene
        (-> (scene-init)
            (add-block (block 'A 1))
            (add-block (block 'B 1))
            (add-block (block 'C 1))
            )]

    (hacker-solve scene '[make [on B A]])

    ))




(defn t2 []

  (restart!)
  (let [scene
        (-> (scene-init)
            (add-block (block 'A 1))
            (add-block (block 'B 1))
            (add-block (block 'C 1))
            )]

    (hacker-solve scene
                  '[make [on A B]]
                  ;;'[test [on B C]]
                  '[make [on B C]]
                  '[make [on C A]]
                  )

    ))


(defn t3 []
  (let [scene
        (-> (scene-init)
            (add-block (block 'A 1))
            (add-block (block 'B 1))
            (add-block (block 'C 3))
            )]



    (hacker-solve scene
                  '[make [on C TABLE]]
                  '[make [on B C]]
                  '[make [on A B]]
                  '[make [on C A]]

                  )
    
    ;; (-> scene
    ;;     (vacancies 'A 'B)
    ;;     first
    ;;     nil? not
    ;;     )
  
    ;; (test-place-for {:scene scene} 'A 'C)

    ))




(defn t4 []

  (restart!)
  (let [scene
        (-> (scene-init)
            (add-block (block 'A 1))
            (add-block (block 'B 1))
            (add-block (block 'C 1))
            )]

    (hacker-solve scene
                  '[make [on A B]]
                  ;;'[test [on B C]]
                  '[make [on C A]]

                  )

    ))




;;; i'm stateful! :)
(restart!)
