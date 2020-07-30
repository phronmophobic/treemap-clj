(ns treemap-clj.core
  (:require [membrane.ui :as ui
             :refer [on]]
            [clojure.zip :as z]
            [membrane.skia :as skia]
            [clojure.data.json :as json]
            [membrane.component :as component
             :refer [defui]])


  (:gen-class))


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


(defn treemap-size [obj]
  (if (and (seqable? obj) (not (string? obj)))
    (reduce + (map treemap-size obj))
    (cond

      (number? obj)
      ;; obj
      1
      #_(count (str obj))

      (instance? clojure.lang.Named obj)
      1
      #_(count (name obj))

      (string? obj)
      1
      #_(count obj)


      :else 1

      )))

(defn calc-density [obj {:keys [w h] :as rect} size]
  (/ (size obj) (* w h)))

(defrecord Rect [x y w h obj children])
(defn make-rect
  ([w h]
   (Rect. 0 0 w h nil nil))
  ([x y w h]
   (Rect. x y w h nil nil)))
(defn area [rect]
  (* (:w rect) (:h rect)))
(defn translate [rect [sx sy]]
  (-> rect
      (update :x + sx)
      (update :y + sy)))

(def algorithm :squarify)
(def my-rect (make-rect 100 100))
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



(defn offset-stack [[ox oy] items]
  (mapv (fn [i item]
          (ui/translate (* i ox)
                        (* i oy)
                        item))
        (range) items))

(defn aspect-ratio [{:keys [w h]}]
  (min (/ w h)
       (/ h w)))



(defn subdivide-strip [sizes {:keys [x y w h] :as rect} density horizontal?]
  (let [strip-size (reduce + sizes)
        rects (if horizontal?
                (let [child-h (/ strip-size
                                 (* density w))]
                  (loop [strip []
                         sizes (seq sizes)
                         x-offset x]
                    (if-not sizes
                      strip
                      (let [size (first sizes)
                            child-w (/ size
                                       (* density child-h))]
                        (recur (conj strip (make-rect x-offset y
                                                      child-w child-h))
                               (next sizes)
                               (+ x-offset child-w))))))
                (let [child-w (/ strip-size
                                 (* density h))]
                  (loop [strip []
                         sizes (seq sizes)
                         y-offset y]
                    (if-not sizes
                      strip
                      (let [size (first sizes)
                            child-h (/ size
                                       (* density child-w))]
                        (recur (conj strip (make-rect x y-offset
                                                      child-w child-h))
                               (next sizes)
                               (+ y-offset child-h)))))))]
    rects))



#_(defn render-strip
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

(defn worst [strip rect density horizontal?]
  (reduce min 1 (map aspect-ratio (subdivide-strip (map second strip) rect density horizontal?))))



;; (defn avg [strip rect size density horizontal?]
;;   (let [total  (reduce + (map aspect-ratio (subdivide-strip (map size strip) rect density horizontal?)))]
;;     (/ total (count strip))))




;; (defn add-node-to-strip? [strip node rect size density horizontal?]
;;   (case algorithm
;;     :traditional true
;;     :squarify (or (empty? strip)
;;                   (< (worst strip rect size density horizontal?)
;;                      (worst (conj strip node)
;;                             rect size density horizontal?)))
;;     :strip (or (empty? strip)
;;                (< (avg strip rect size density horizontal?)
;;                   (avg (conj strip node)
;;                        rect  size density horizontal?))))
;;   )

;; (defn direction-method [depth [w h]]
;;   (case algorithm
;;     (:traditional :strip)
;;     (even? depth)

;;     :squarify (not (> w h))))







;; (defn treemap-layout
;;   ([nodes]
;;    (treemap-layout nodes [100 100]))
;;   ([nodes rect]
;;    (treemap-layout nodes rect (calc-density nodes rect treemap-size) 0))
;;   ([nodes [w h :as rect] size density depth]
;;    (loop [strips []
;;           strip []
;;           rect rect
;;           nodes (seq (filter #(pos? (size %)) nodes))
;;           ]
;;      (if nodes
;;        (let [c (first nodes)
;;              horizontal? (direction-method depth rect)]
;;          (if (add-node-to-strip? strip c rect size density horizontal?)
;;            (recur strips
;;                   (conj strip c)
;;                   rect
;;                   (next nodes))
;;            (recur (conj strips strip)
;;                   [c]
;;                   (let [strip-size (reduce + (map size strip))]
;;                     (if horizontal?
;;                       (let [strip-height (/ strip-size
;;                                             (* density w))]
;;                         [w (- h strip-height)])
;;                       (let [strip-width (/ strip-size
;;                                            (* density h))]
;;                         [(- w strip-width) h])))
;;                   (next nodes))))
;;        (if (seq strip)
;;          (conj strips strip)
;;          strips))))
;;   )



(defn traditional-layout
  ([objs sizes {:keys [w h] :as rect}]
   (traditional-layout objs sizes rect nil nil))
  ([objs sizes {:keys [w h] :as rect} horizontal? total-size]
   (let [total-size (or total-size (reduce + sizes))
         density (/ total-size
                    (* w h))
         horizontal? (if (nil? horizontal?)
                       (> w h)
                       horizontal?)]
     (if horizontal?
       (second
        (reduce (fn [[x-offset childs] [obj size]]
                  (let [child-w (/ size
                                   (* density h))
                        child-rect (assoc (make-rect x-offset 0 child-w h)
                                          :obj obj)]

                    [(+ x-offset child-w)
                     (conj childs child-rect)]))
                [0 []]
                (map vector objs sizes)))
       (second
        (reduce (fn [[y-offset childs] [obj size]]
                  (let [child-h (/ size
                                   (* density w))
                        child-rect (assoc (make-rect 0 y-offset w child-h)
                                          :obj obj)]
                    [(+ y-offset child-h)
                     (conj childs child-rect)]))
                [0 []]
                (map vector objs sizes)))))))

(defn strip-rect [total-size {:keys [w h]} horizontal? density]
  (if horizontal?
    (let [strip-h (/ total-size
                     (* density w))
          strip-rect (make-rect w strip-h)]
      strip-rect)
    (let [strip-w (/ total-size
                     (* density h))
          strip-rect (make-rect strip-w h)]
      strip-rect)))

(defn add-to-strip? [size strip {:keys [w h] :as rect} horizontal? density]

  (let [strip-size (reduce + strip)
        strip-rects (traditional-layout strip
                                        strip
                                        (strip-rect strip-size rect horizontal? density)
                                        horizontal?
                                        (reduce + strip))
        worst-ratio (reduce min 1 (map aspect-ratio strip-rects))
        ;; avg-ratio (/ (reduce + (map aspect-ratio strip-rects))
        ;;              (count strip-rects))

        new-strip (conj strip size )
        new-rects (traditional-layout new-strip
                                      new-strip
                                      (strip-rect (+ strip-size size) rect horizontal? density)
                                      horizontal?
                                      (reduce + new-strip))
        worst-new-ratio (reduce min 1 (map aspect-ratio new-rects))
        ;; avg-new-ratio (/ (reduce + (map aspect-ratio new-rects))
        ;;                  (count new-rects))

        ]
    (< worst-ratio
       worst-new-ratio)
    #_(< avg-ratio
       avg-new-ratio)))


(defn make-squarified-strip [strip rect horizontal? density]
  (let [objs (map first strip)
        sizes (map second strip)
        strip-size (reduce + sizes)]
    (traditional-layout objs
                        sizes
                        (strip-rect strip-size rect horizontal? density)
                        horizontal?
                        nil)))

(defn squarified-layout
  ([objs sizes rect]
   (squarified-layout objs sizes rect nil))
  ([objs sizes {:keys [w h] :as rect} density]
   (let [density (if density
                   density
                   (/ (reduce + sizes)
                      (* w h)))
         horizontal? (not (> w h))]
     (loop [strip []
            objs-sizes (seq (->> (map vector objs sizes)
                                 (filter (fn [[obj size]]
                                           (pos? size)))))]
       (if-not objs-sizes
         (make-squarified-strip strip rect horizontal? density)
         (let [[obj size] (first objs-sizes)]
           (if (or (empty? strip)
                   (add-to-strip? size (map second strip) rect horizontal? density))
             (recur (conj strip [obj size])
                    (next objs-sizes))
             (let [strip-rects (make-squarified-strip strip rect horizontal? density)
                   maxx (reduce max
                                0
                                (map #(+ (:x %) (:w %)) strip-rects))
                   maxy (reduce max
                                0
                                (map #(+ (:y %) (:h %)) strip-rects))
                   ;; items in a strip get layout out horizontally
                   ;; but strips will be layed out vertically
                   [ox oy :as offset] (if horizontal?
                                        [0 maxy]
                                        [maxx 0])
                   rest-of-rect (make-rect (- w ox)
                                           (- h oy))]
               (into strip-rects
                     (map
                      (fn [rect]
                        (translate rect offset))
                      (squarified-layout (map first objs-sizes) (map second objs-sizes) rest-of-rect)))))))))))


(def treemap-options-defaults
  {:branch? (fn [obj] (and (seqable? obj) (not (string? obj))))
   :children seq
   :size treemap-size
   :layout traditional-layout})

(defn render-treemap [rect]
  (loop [to-visit (seq [[0 0 rect]])
         view []]
    (if to-visit
      (let [[ox oy rect] (first to-visit)]
        (if-let [children (:children rect)]
          (let [ox (+ ox (:x rect))
                oy (+ oy (:y rect))]
            (recur (into (next to-visit)
                         (map #(vector ox oy %) children))
                   view))
          (recur (next to-visit)
                 (conj view
                       (ui/translate (+ (:x rect) ox) (+ (:y rect) oy)
                                     (let [data (:obj rect)]
                                       (on
                                        :mouse-move
                                        (fn [_]
                                          [[:select data]])
                                        (ui/with-color (data-color data)
                                          (ui/rectangle (max 1 (dec (:w rect)))
                                                        (max 1 (dec (:h rect))))
                                          ))))))))
      (ui/with-style ::ui/style-fill
        view))))


(defn treemap [obj {:keys [w h] :as rect} {:keys [branch?
                                                  children
                                                  size
                                                  layout
                                                  min-area] :as options}]
  (if (and (branch? obj)
           (> (* w h)
              (or min-area 0)))
    (let [childs (children obj)
          child-rects (layout childs (map size childs) rect)]
      (assoc rect
             :children
             (map (fn [child-rect]
                    (treemap (:obj child-rect) child-rect options))
                  child-rects)))
    rect))

(defn treemap-zip [obj {:keys [w h] :as rect} {:keys [branch?
                                                      children
                                                      size
                                                      layout] :as options}]
  (let [root (assoc rect
                    :obj obj)
        zip (z/zipper :children :children
                      (fn [node children]
                        (assoc node
                               :children children))
                      root)
        treemap-edit (fn [node]
                       (let [obj (:obj node)]
                         (if (branch? obj)
                           (let [childs (children obj)
                                 child-rects (layout childs (map size childs) node)
                                 child-rects (map (fn [child rect]
                                                    (assoc rect :obj child))
                                                  childs child-rects)]
                             (assoc node
                                    :children child-rects))
                           node)))]
    (loop [zip zip]
      (if (z/end? zip)
        (z/root zip)
        (recur (-> zip
                   (z/edit treemap-edit)
                   (z/next)))))))


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

(defn zip-depth [loc]
  (-> loc second :pnodes count))

(defn wrap-depth [root branch? children]
  (let [zip (z/zipper :children :children
                      (fn [node children]
                        (assoc node
                               :children children))
                      root)
        depth-edit (fn [loc]
                     (let [depth (zip-depth loc)]
                       (z/edit loc #(merge {:obj %
                                            :depth depth}
                                           (when (branch? %)
                                             {:children (children %)})))))]
    (loop [zip zip]
      (if (z/end? zip)
        (z/root zip)
        (recur (-> zip
                   depth-edit
                   (z/next)))))))




(defn lets-explore
  ([]
   (lets-explore (doall (read-source))))
  ([obj]
   (let [
         tm (render-treemap
             (treemap obj (make-rect 1200 800)
                      {:branch? #(and (not (string? %)) (seqable? %))
                       :children seq
                       :size treemap-size
                       :layout squarified-layout
                       :min-area 1}))]
     (skia/run (component/make-app #'treemap-explore {:tm (skia/->Cached tm)})))))



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))




