(ns treemap-clj.treem
  (:require [treemap-clj.view :refer [treemap-explore
                                      wrap-treemap-events
                                      render-depth
                                      render-hierarchy-lines
                                      render-keys]]
            [membrane.component :as component]
            [treemap-clj.core :refer [keyed-treemap
                                      treemap
                                      make-rect]]
            [membrane.ui :as ui]
            [membrane.toolkit :as tk]
            [clojure.data.json :as json]
           ))

(defonce toolkit
  (delay
    (if-let [tk (resolve 'membrane.skia/toolkit)]
         @tk
         @(requiring-resolve 'membrane.java2d/toolkit))))


(defn app
  ([obj]
   (app obj [800 800]))
  ([obj [w h]]
   (let [tm (keyed-treemap obj (make-rect w h)
                           #_(merge treemap-options-defaults
                                    {:padding 0}))
         tm-render (wrap-treemap-events
                    tm
                    [
                     (render-depth tm 0.4)
                     ;; (render-hierarchy-lines tm)
                     (render-keys tm)
                     ])
         ]
     (tk/run @toolkit (component/make-app #'treemap-explore {:tm-render (ui/->Cached tm-render)})))))


(defn -main [& args]
  (if (not= (count args) 1)
    (println "usage: lein run -m treemap-clj/treem <edn or json file>")
    (let [fname (first args)]
      (println fname)
      (if (not (.exists (clojure.java.io/file fname)))
        (println "could not open " fname)
        (let [error (Object.)
              obj (try
                    ((requiring-resolve 'json/read)
                     (clojure.java.io/reader fname))
                    (catch Exception e
                      (try
                        (read
                         (java.io.PushbackReader.
                          (clojure.java.io/reader fname)))
                        (catch Exception e
                          error))))]
          (if (identical? error obj)
            (println "error parsing " fname)
            (let [[w h] [800 800]
                  tm (keyed-treemap obj (make-rect w h)
                                    #_(merge treemap-options-defaults
                                             {:padding 0}))
                  tm-render (wrap-treemap-events
                             tm
                             [
                              (render-depth tm 0.4)
                              (render-keys tm)])
                  ]
              (tk/run-sync @toolkit (component/make-app #'treemap-explore {:tm-render (ui/->Cached tm-render)}))))))))
  )

(comment
  (app (read-string (slurp "deps.edn")))
  ,)
