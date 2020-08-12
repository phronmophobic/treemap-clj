(ns treemap-clj.util)


(defn read-source
   ([]
    (read-source
     "src/treemap_clj/core.cljc"))
   ([fname]
    (with-open [rdr (java.io.PushbackReader.
                     (clojure.java.io/reader
                      (clojure.java.io/file fname)))]
      (loop [forms []]
        (let [form (read {:read-cond :allow
                          :eof nil}
                         rdr)]
          (if form
            (recur (conj forms form))
            forms))))))
