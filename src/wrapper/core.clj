(ns wrapper.core
  (:require [ai.logical.hacker :refer :all])
  (:gen-class))

(defn run-demo-1 []
  (restart!)
  (let [scene
        (-> (scene-init)
            (add-block (block 'A 1))
            (add-block (block 'B 1))
            )]
    (hacker-solve scene 
                      ;; Set up the scenario
                      '[make [on A TABLE]]
                      '[make [on B TABLE]]

                      ;; Ask to put make block A on block B.
                      '[make [on A B]]
                      )))


(defn run-demo-2 []
  (restart!)
  (let [scene
        (-> (scene-init)
            (add-block (block 'A 1))
            (add-block (block 'B 1))
            (add-block (block 'C 1)))]
    (hacker-solve scene
                  '[make [on B TABLE]]
                  '[make [on C TABLE]]
                  '[make [on A B]]

                  ;; Ask to make block B on block C,
                  ;; which will fail because there's no room.
                  '[make [on B C]])))

(defn -main
  "Runs both demos"
  [& args]
  (println "HACKER block stacking demos are meant to run
  interactively. Run \"lein repl\". Then (run-demo-1)
  and (run-demo-2).\n"))
