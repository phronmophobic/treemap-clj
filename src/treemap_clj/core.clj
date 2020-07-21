(ns treemap-clj.core
  (:require [membrane.ui :as ui
             :refer [on]]
            [membrane.skia :as skia]
            [membrane.component :as component
             :refer [defui]])


  (:gen-class))

(defn treemap-size [obj]
  (if (and (seqable? obj) (not (string? obj)))
    (reduce + (map treemap-size obj))
    (cond

      (number? obj)
      (count (str obj))

      (instance? clojure.lang.Named obj)
      (count (name obj))

      (string? obj)
      (count obj)


      :else 1

      )))

(defn calc-density [obj [w h :as rect] size]
  (/ (size obj) (* w h)))

(def algorithm :squarify)
(def my-rect [100 100])
(def my-obj ["12" 3 4 5])
(def my-density (calc-density my-obj my-rect treemap-size))

(defn vertical-layout
  "Returns a graphical elem of elems stacked on top of each other"
  [& elems]
  (let [elems (seq elems)
        first-elem (first elems)
        offset-y (+ (ui/height first-elem)
                    (ui/origin-y first-elem))]
    (when elems
      (loop [elems (next elems)
             offset-y offset-y
             group-elems [first-elem]]
        (if elems
          (let [elem (first elems)
                dy (+ (ui/height elem)
                      (ui/origin-y elem))]
            (recur
             (next elems)
             (+ offset-y dy)
             (conj group-elems
                   (ui/translate 0 offset-y
                                 elem))))
          group-elems)))))

(defn horizontal-layout
  "Returns a graphical elem of elems layed out next to eachother."
  [& elems]
  (let [elems (seq elems)
        first-elem (first elems)
        offset-x (+ (ui/width first-elem)
                    (ui/origin-x first-elem))]
    (when elems
      (loop [elems (next elems)
             offset-x offset-x
             group-elems [first-elem]]
        (if elems
          (let [elem (first elems)
                dx (+ (ui/width elem)
                      (ui/origin-x elem))]
            (recur
             (next elems)
             (+ offset-x dx)
             (conj group-elems
                   (ui/translate offset-x 0
                              elem))))
          group-elems)))))

;; (defn alg-sort [nodes]
;;   (case algorithm
;;     (:traditional :strip)
;;     nodes

;;     :squarify
;;     (sort-by (comp - size) nodes)))



(defn stack [[ox oy] items]
  (mapv (fn [i item]
          (ui/translate (* i ox)
                        (* i oy)
                        item))
        (range) items))

(defn aspect-ratio [[w h]]
  (min (/ w h)
       (/ h w)))



(defn subdivide [strip [w h :as rect] size density horizontal?]
  (let [strip-size (reduce + (map size strip))
        rects (if horizontal?
                (let [child-h (/ strip-size
                                 (* density w))]
                  (for [obj strip
                        :let [child-w (/ (size obj)
                                         (* density child-h))]]
                    [child-w child-h]))
                (let [child-w (/ strip-size
                                 (* density h))]
                  (for [obj strip
                        :let [child-h (/ (size obj)
                                         (* density child-w))]]
                    [child-w child-h])))]
    rects))



(defn render-strip
  ([strip]
   (render-strip [100 100]))
  ([strip [w h :as rect] size]
   (let [total-size (reduce + (map size strip))
         density (/ total-size (* w h))]
     (render-strip strip rect density true)))
  ([strip rect size density horizontal?]
   (ui/with-style
     ::ui/style-stroke
     (apply (if horizontal?
              horizontal-layout
              vertical-layout)
            (map (fn [[w h]] (ui/rectangle w h)) (subdivide strip rect size density horizontal?))))))



(comment
  (skia/run (let [rect [100 100]
                  [w h] rect
                  horizontal? false
                  items [1 2 3 4 "asf"]
                  total-size (reduce + (map treemap-size items))
                  density (/ total-size (* w h))]
              (constantly
               ((if horizontal?
                  vertical-layout
                  horizontal-layout)
                (render-strip (take 1 items) rect density horizontal?)
                (render-strip (take 2 items) rect density horizontal?))))))

(defn worst [strip rect size density horizontal?]
  (reduce min 1 (map aspect-ratio (subdivide strip rect size density horizontal?))))

(defn avg [strip rect size density horizontal?]
  (let [total  (reduce + (map aspect-ratio (subdivide strip rect density horizontal?)))]
    (/ total (count strip))))




(defn add-node-to-strip? [strip node rect size density horizontal?]
  (case algorithm
    :traditional true
    :squarify (or (empty? strip)
                  (< (worst strip rect size density horizontal?)
                     (worst (conj strip node)
                            rect size density horizontal?)))
    :strip (or (empty? strip)
               (< (avg strip rect size density horizontal?)
                  (avg (conj strip node)
                       rect  size density horizontal?))))
  )

(defn direction-method [depth [w h]]
  (case algorithm
    (:traditional :strip)
    (even? depth)

    :squarify (not (> w h))))

(defrecord Rect [x y w h])
(defn make-rect
  ([w h]
   (Rect. 0 0 w h))
  ([x y w h]
   (Rect. x y w h)))
(defn area [rect]
  (* (:w rect) (:h rect)))
(defn translate [rect [sx sy]]
  (-> rect
      (update :x + sx)
      (update :y + sy)))





(defn treemap-layout
  ([nodes]
   (treemap-layout nodes [100 100]))
  ([nodes rect]
   (treemap-layout nodes rect (calc-density nodes rect treemap-size) 0))
  ([nodes [w h :as rect] size density depth]
   (loop [strips []
          strip []
          rect rect
          nodes (seq (filter #(pos? (size %)) nodes))
          ]
     (if nodes
       (let [c (first nodes)
             horizontal? (direction-method depth rect)]
         (if (add-node-to-strip? strip c rect size density horizontal?)
           (recur strips
                  (conj strip c)
                  rect
                  (next nodes))
           (recur (conj strips strip)
                  [c]
                  (let [strip-size (reduce + (map size strip))]
                    (if horizontal?
                      (let [strip-height (/ strip-size
                                            (* density w))]
                        [w (- h strip-height)])
                      (let [strip-width (/ strip-size
                                           (* density h))]
                        [(- w strip-width) h])))
                  (next nodes))))
       (if (seq strip)
         (conj strips strip)
         strips))))
  )

(def treemap-options-defaults
  {:branch? (fn [obj] (and (seqable? obj) (not (string? obj))))
   :children seq
   :size treemap-size
   :layout treemap-layout})

(defn treemap
  ([obj [w h :as rect]]
   (treemap obj rect {}))
  ([obj [w h :as rect] {:keys [branch?
                   children] :as options}]
   (let [{:keys [branch? children size layout]}
         (merge treemap-options-defaults options)
         total-size (size obj)
         density (/ total-size (* w h))]
     (treemap obj rect options density 0)))
  ([obj
    [w h :as rect]
    {:keys [branch?
            children
            size
            layout] :as options}
    density
    depth]
   (let [{:keys [branch?
                 children
                 size
                 layout] :as options}
         (merge treemap-options-defaults options)]
     (if (branch? obj)
       (let [childs (children obj)
             strips (layout childs rect size density depth)
             horizontal? (direction-method depth rect)
             strip-rects (map #(subdivide % rect size density horizontal?) strips)]
         (loop [[ox oy :as offset] [0 0]
                strips (seq strips)
                tree-rects []]
           (if strips
             (let [strip (first strips)
                   rects (subdivide strip rect size density horizontal?)
                   strip-offsets (reduce (fn [offsets [w h]]
                                           (let [[ox oy] (last offsets)]
                                             (conj offsets
                                                   (if horizontal?
                                                     [(+ w ox) oy]
                                                     [ox (+ h oy)]))))
                                         [[0 0]]
                                         rects)]
               (recur (if horizontal?
                        [ox (+ oy (-> rects first second))]
                        [(+ ox (-> rects first first)) oy])
                      (next strips)
                      (into tree-rects
                            (mapcat (fn [item rect [strip-ox strip-oy]]
                                      (map #(translate % [(+ strip-ox ox) (+ strip-oy oy)])
                                           (treemap item rect options density (inc depth))))
                                    strip
                                    rects
                                    strip-offsets))))
             tree-rects)))
       [(assoc (make-rect w h)
               :data obj)]))))

(defui treemap-explore [& {:keys [tm selected]}]
  (horizontal-layout
   (on
    :select
    (fn [data]
      [[:set $selected data]])
    tm)
   (when selected
     (let [s (with-out-str
               (clojure.pprint/pprint selected))]
       (ui/label (subs s 0 (min (count s) 500)))))
   ))


(defn read-source
  ([]
   (read-source
    "src/treemap_clj/core.clj"))
  ([fname]
   (let [rdr (java.io.PushbackReader.
              (clojure.java.io/reader
               (clojure.java.io/file fname))
              )
         ]
     (loop [forms []]
       (let [form (read rdr false nil)]
         (if form
           (recur (conj forms form))
           forms))))))


(def type-colors
  {
   clojure.lang.Symbol [42 40 250]
   clojure.lang.Keyword [250 151 137]
   java.lang.String [112 210 250]
   java.lang.Double [250 221 87 98]
   java.lang.Long [250 221 87 98]
   java.lang.Character [112 210 250]
   java.lang.Boolean [100 250 178]

   })

(defn data-color [obj]
  (mapv #(/ % 255.0) (get type-colors (type obj) [0 0 0])))

(defn render-treemap [tm]
  (ui/with-style ::ui/style-fill
    (mapv (fn [{:keys [x y w h data]}]
            (ui/translate x y
                          (ui/scissor-view
                           [0 0] [w h]
                           [(on
                             :mouse-move
                             (fn [_]
                               [[:select data]])
                             (ui/with-color (data-color data)
                               (ui/rectangle (dec w) (dec h))))
                            #_(when data
                                (ui/with-style ::ui/style-fill
                                  (ui/label (pr-str data)
                                            (ui/font nil 10))))])))
          tm)))

(defn lets-explore
  ([]
   (lets-explore (doall (read-source))))
  ([obj]
   (let [
         tm (render-treemap
             (treemap obj [400 400]
                      {:branch? (fn [obj]
                                  (and (seqable? obj)
                                       (not (string? obj))
                                       (let [m (meta obj)]
                                         (when-let [depth (get m ::depth 0)]
                                           (< depth 2)))))
                       :children (fn [obj]
                                   (let [parent-depth (get (meta obj) ::depth 0)]
                                     (map (fn [child]
                                            (if (instance? clojure.lang.Obj child)
                                              (vary-meta child
                                                         (fn [m]
                                                           (assoc m ::depth (inc parent-depth))))
                                              child))
                                          obj)))}))]
     (skia/run (component/make-app #'treemap-explore {:tm (skia/->Cached tm)})))))







(defn treemap-traditional
  ([obj]
   (treemap obj [100 100]))
  ([obj [w h :as size]]
   (treemap obj size true))
  ([obj [w h] split-horizontal?]
   (if (seqable? obj)
     (let [c (count obj)
           child-w (if split-horizontal?
                     (/ w c)
                     w)
           child-h (if split-horizontal?
                     h
                     (/ h c))
           child-treemap #(treemap % [child-w child-h] (not split-horizontal?))
           child-layout (if split-horizontal?
                          #(stack [child-w 0] %)
                          #(stack [0 child-h] %))]
       (child-layout (map child-treemap obj)))
     (ui/scissor-view [0 0]
                      [(inc w)
                       (inc h)]
                      [(ui/rectangle w h)
                       (ui/with-style ::ui/style-fill
                         (ui/label (pr-str obj)))
                       ]))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))






