(ns treemap-clj.core
  (:require [membrane.ui :as ui
             :refer [on]]
            [clojure.zip :as z]
            [treemap-clj.rtree :as rtree]
            clojure.pprint
            #?(:clj [membrane.skia :as skia])
            #?(:clj [clojure.data.json :as json])
            [membrane.component :as component
             :refer [defui
                     defeffect]])


  #?(:clj (:gen-class)))


#?(:clj
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
              forms)))))))


(defn type-color [obj]
  (cond
   (symbol? obj) [42 40 250]
   (keyword? obj) [250 151 137]
   (string? obj) [112 210 250]
   (number? obj)  [250 221 87]
   (char? obj) [112 210 250]
   (boolean? obj) [100 250 178]
   :else [0 0 0]))

(defn data-color [obj]
  (mapv #(/ % 255.0) (type-color obj)))

(defn type-color-legend []

  (ui/table-layout
   (for [[title example] '[["symbol" a]
                           ["keyword" :a]
                           ["string" "A"]
                           ["number" 1]
                           ;; ["character "\a]
                           ["boolean" true]
                           ["other" nil]]]
     [[(ui/rectangle 17 17)
       (ui/translate 1 1
        (ui/filled-rectangle (data-color example)
                             15 15))]
      (ui/label title)])
   5 5))


(defn default-keypath-fn [obj]
  (cond
    (map-entry? obj)
    '[key val]

    (map? obj)
    (map #(list 'find %) (keys obj))

    :else
    (do
      (assert (seqable? obj))
      (map #(list 'nth %) (range (count obj))))))

(defn treemap-size [obj]
  (if (and obj (seqable? obj) (not (string? obj)))
    (reduce + (map treemap-size obj))
    (cond

      (number? obj)
      ;; obj
      1
      #_(count (str obj))

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

(defn scale [rect [sx sy]]
  (-> rect
      (update :w * sx)
      (update :h * sy)))

(defn rect-parent [rect]
  (some-> rect
          meta
          ::treemap-parent
          deref))
(defn rect-keypath [rect]
  (::treemap-keypath (meta rect)))


(defn treezip [tm]
  (z/zipper :children :children
                      (fn [node children]
                        (assoc node
                               :children children))
                      tm))

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
  {:branch? #(and % (not (string? %)) (seqable? %))
   :children seq
   :size treemap-size
   :padding nil
   :layout squarified-layout
   :keypath-fn default-keypath-fn
   :min-area  1})

(defn render-treemap
  ([rect]
   (render-treemap rect 0.2))
  ([rect opacity]
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
                                           [[:select data nil]])
                                         (ui/with-color (conj (data-color data) opacity)
                                           (ui/rectangle (max 1 (dec (:w rect)))
                                                         (max 1 (dec (:h rect))))
                                           ))))))))
       (ui/with-style ::ui/style-fill
         view)))))


(defn zip-depth [loc]
  (-> loc second :pnodes count))

(defn max-depth [rect]
  (loop [zip (treezip rect)
         max-depth 0]
    (if (z/end? zip)
      max-depth
      (recur (z/next zip)
             (max max-depth (zip-depth zip))))))

(defn color-gradient
  "Given a number between 0 and 1. Returns a color."
  [num]
  (let [i (* 4.0 num)]
    (cond
      (> i 3)
      (let [n (- i 3)]
        [1 (- 1 n) 0])

      (> i 2)
      (let [n (- i 2)]
        [n 1 0])

      (> i 1)
      (let [n (- i 1)]
        [0 1  (- 1 n)])

      :else
      [0 i 1])))

(defn depth-color-legend []
  (let [steps 20
        width 200
        height 50
        colors (apply
                horizontal-layout
                (for [i (range steps)
                      :let [n (/ i steps)]]
                  (ui/filled-rectangle (color-gradient n)
                                       (/ width steps)
                                       height)))
        right-label (ui/label "max depth")
        ]
    [colors
     (ui/translate 0 height
                   [(ui/label "min depth")
                    (ui/translate (- width (ui/width right-label)) 0
                                  right-label)])]))

(defn render-depth
  ([rect]
   (render-depth rect 0.2))
  ([rect opacity]
   (let [mdepth (max-depth rect)]
     (loop [to-visit (seq [[0 0 0 rect]])
            view []]
       (if to-visit
         (let [[depth ox oy rect] (first to-visit)]
           (if-let [children (:children rect)]
             (let [ox (+ ox (:x rect))
                   oy (+ oy (:y rect))]
               (recur (into (next to-visit)
                            (map #(vector (inc depth) ox oy %) children))
                      view))
             (recur (next to-visit)
                    (conj view
                          (ui/translate (+ (:x rect) ox) (+ (:y rect) oy)
                                        (ui/with-color (conj (color-gradient (/ depth mdepth)) opacity)
                                          (ui/rectangle (max 1 (dec (:w rect)))
                                                        (max 1 (dec (:h rect))))))))))
         (ui/with-style ::ui/style-fill
           view))))))

(defn coll-color [obj]
  (cond

    (map? obj)
    [0.13333333333333333 0.5450980392156862 0.7098039215686275]
    (map-entry? obj)
    [0.49019607843137253 0.6509803921568628 0.403921568627451]
    (seqable? obj)
    [0.9019607843137255 0.3568627450980392 0.3058823529411765]
    ;; (seqable? obj)
    ;; [0.47843137254901963 0.09019607843137255 0.16862745098039217]
    :else
    (do
      [0 0 0])))

(defn coll-opacity [depth]
  (min 1 (+ 0.2 (* depth 0.2)))
  )

(defn linetree-stroke-width [depth]
  (max 1 (- 10 (* 2 depth))))


(defn depth-line-legend []
  (ui/table-layout
   (for [[title example] [["vector" []]
                          ["map" {:a 1}]
                          ["map entry" (first {:a 1})]]]
     (let [[x1 y1] [0 10]
           [x2 y2] [30 10]
           depth 3]
       [[(ui/translate (+ x1) (+ y1)
                       (ui/filled-rectangle [0 0 0]
                                            2 2))

         (ui/translate x2 y2
                       (ui/filled-rectangle [0 0 0]
                                            2 2))

         (ui/with-style ::ui/style-stroke
           (ui/with-stroke-width (linetree-stroke-width depth)
             (ui/with-color (conj (coll-color example)
                                  (coll-opacity depth))
               (ui/path [x1 y1]
                        [x2 y2]))))]
        (ui/label title)]))
   5 5))

(defn render-linetree [rect]
  (loop [to-visit (seq [[[] 0 0 0 rect]])
         view []]
    (if to-visit
      (let [[ppath depth ox oy rect] (first to-visit)]
        (if-let [children (:children rect)]
          (let [ox (+ ox (:x rect))
                oy (+ oy (:y rect))]
            (recur (into (next to-visit)
                         (map #(vector (conj ppath rect) (inc depth) ox oy %) children))
                   (if (zero? depth)
                     view
                     (conj view
                           (ui/with-stroke-width (linetree-stroke-width depth)
                             (ui/translate
                              ox oy
                              (ui/with-color (conj (coll-color (:obj rect))
                                                   (coll-opacity depth))
                                (vec
                                 (for [child children]
                                   (ui/path [0 0]
                                            [(:x child)
                                             (:y child)]))))))))))
          (recur (next to-visit)
                 (conj view
                       (ui/translate (+ (:x rect) ox) (+ (:y rect) oy)
                                     (ui/filled-rectangle [0 0 0]
                                                          2 2))))))
      (ui/with-style ::ui/style-stroke
        (vec (reverse view))))))

(defn bubble-width [depth]
  (max 1 (- 20 (* 2 depth))))

(defn render-bubbles [rect]
  (loop [to-visit (seq [[[] 0 0 0 rect]])
         view []]
    (if to-visit
      (let [[ppath depth ox oy rect] (first to-visit)]
        (if-let [children (:children rect)]
          (let [ox (+ ox (:x rect))
                oy (+ oy (:y rect))]
            (recur (into (next to-visit)
                         (map #(vector (conj ppath rect) (inc depth) ox oy %) children))
                   
                   (if (not= 1 depth)
                     view
                     (conj view
                         (let [rect-size (bubble-width depth)]
                           (ui/translate ox oy
                                         [(ui/translate (max 0 (- (:w rect)
                                                                  rect-size)) 0
                                                        (ui/filled-rectangle [1 1 1 0.5]
                                                                             rect-size (:h rect)))
                                          (ui/translate 0 (max 0 (- (:h rect)
                                                                    rect-size))
                                                        (ui/filled-rectangle [1 1 1 0.5]
                                                                             (:w rect) rect-size))]))))))
          (recur (next to-visit)
                 view)))
      (ui/with-style ::ui/style-stroke
        view))))

(defn pr-label
  ([x]
   (pr-label x 30))
  ([x max-length]
   (pr-label x max-length nil))
  ([x max-length font]
   (let [s (pr-str x)
         s (if max-length
             (subs s 0 (min max-length (count s)))
             s)]
     (ui/label s (or font ui/default-font)))))

(defn depth-font-size [depth]
  (max 8
       (- 20 (* depth 2))))

(defn rect-val-color [depth]
  (let [gray (min 0.5 (* depth 0.1))]
    [gray gray gray]))

(def mono-font
  #?(:clj "Menlo.ttc"
     :cljs nil))

(defn render-rect-vals [rect]
  (loop [to-visit (seq [[[] 0 0 0 rect]])
         rt (rtree/rtree)
         view []]
    (if to-visit
      (let [[ppath depth ox oy rect] (first to-visit)]
        (if-let [children (:children rect)]
          (let [ox (+ ox (:x rect))
                oy (+ oy (:y rect))]
            (recur (seq
                    (concat (next to-visit)
                            (map #(vector (conj ppath rect) (inc depth) ox oy %) children)))
                   rt
                   view
                   ))
          (let [lbl (ui/with-color (rect-val-color depth)
                      (pr-label (:obj rect) 30 (ui/font mono-font (depth-font-size depth))))
                [w h] (ui/bounds lbl)
                rx (+ (:x rect) ox)
                ry (+ (:y rect) oy)
                overlaps? (or (seq (rtree/search rt [rx ry]))
                              (seq (rtree/search rt [(+ rx w) ry]))
                              (seq (rtree/search rt [(+ rx w) (+ ry h)]))
                              (seq (rtree/search rt [rx (+ ry h)])))]
            (recur (next to-visit)
                   (if overlaps?
                     rt
                     (rtree/add! rt (make-rect rx ry w h)))
                   (if overlaps?
                     view
                     (conj view
                           (ui/translate rx ry
                                         lbl)
                           ))))))
      view)))


(defn render-keys [rect]
  (loop [to-visit (seq [[0 0 rect]])
         view []]
    (if to-visit
      (let [[ox oy rect] (first to-visit)]
        (if-let [children (:children rect)]
          (let [ox (+ ox (:x rect))
                oy (+ oy (:y rect))]
            (recur (into (map #(vector ox oy %) children)
                         (next to-visit))
                   view))
          (recur (next to-visit)
                 (conj view
                       (ui/translate ox oy
                                     (ui/label (:title rect)))))))
      view)))



(defn render-treemap-depth [rect]
  (loop [to-visit (seq [[0 0 0 rect]])
         view []]

    (if to-visit
      (let [[depth ox oy rect] (first to-visit)]
        (if-let [children (:children rect)]
          (let [ox (+ ox (:x rect))
                oy (+ oy (:y rect))]
            (recur (into (next to-visit)
                         (map #(vector (inc depth) ox oy %) children))
                   (conj view
                         (ui/with-color (coll-color (:obj rect))
                           (ui/translate ox oy
                                         (ui/with-style ::ui/style-stroke
                                           (ui/with-stroke-width (max 1 (- 10 (* 2 depth)))
                                             (ui/rectangle (max 1 (dec (:w rect)))
                                                           (max 1 (dec (:h rect))))
                                             )))))))
          (recur (next to-visit)
                 (conj view
                       (ui/translate (+ (:x rect) ox) (+ (:y rect) oy)
                                     (let [data (:obj rect)] 
                                       (on
                                        :mouse-move
                                        (fn [_]
                                          [[:select data]])
                                        [(ui/with-color (data-color data)
                                           (ui/rectangle (max 1 (dec (:w rect)))
                                                         (max 1 (dec (:h rect)))))
                                         #_(ui/scissor-view [0 0] [(:w rect)
                                                                   (:h rect)]
                                                            (ui/label data
                                                                      (ui/font nil 10)))])))))))
      (ui/with-style ::ui/style-fill
        view)))
  )



(defn default-padding-fn [depth]
  (get {0 15
        1 15
        2 10
        3 6
        4 2
        5 2
        6 2
        7 2
        8 2}
       depth
       0))



(defn treemap
  ([obj rect]
   (treemap obj rect
            treemap-options-defaults))
  ([obj {:keys [w h] :as rect} {:keys [branch?
                                       children
                                       size
                                       layout
                                       min-area
                                       padding
                                       keypath-fn
                                       recurse] :as options}]
   (let [padding (or padding
                     default-padding-fn)

         depth (get options :depth 0)
         child-options (assoc options
                              :depth (inc depth))
         pad (if (fn? padding)
               (padding depth)
               padding)

         recurse (or recurse treemap)

         include-padding (fn [n]
                           (max 1 (- n pad)))
         rect (-> rect
                  (assoc :obj obj)
                  (update :w include-padding)
                  (update :h include-padding)
                  )]
     (if (and (branch? obj)
              (> (* w h)
                 (or min-area 0)))
       (let [childs (children obj)
             child-rects (layout childs (map size childs) rect)
             keypaths (if keypath-fn
                         (keypath-fn obj)
                         (repeat nil))
             ;; we would use a promise, but `promise` doesn't exist in cljs
             parent-ref (atom nil)
             rect (assoc rect
                         :children
                         (map (fn [keypath child-rect]
                                (let [child-rect
                                      (with-meta child-rect
                                        {::treemap-parent parent-ref
                                         ::treemap-keypath keypath})]
                                  (recurse (:obj child-rect) child-rect child-options)))
                              keypaths
                              child-rects))]
         (reset! parent-ref rect)
         rect)
       rect))))


(defn keyed-treemap
  ([obj rect]
   (keyed-treemap obj rect
                  treemap-options-defaults))
  ([obj {:keys [w h] :as rect} {:keys [branch?
                                       children
                                       size
                                       layout
                                       min-area
                                       padding
                                       keypath-fn] :as options}]
   (let [depth (get options :depth 0)
         child-options (assoc options
                              :depth (inc depth)
                              :recurse keyed-treemap)
         rect (-> rect
                  (assoc :obj obj))]

     (if (map-entry? obj)
       (let [k (key obj)
               klabel (ui/label k)
               [lwidth lheight] (ui/bounds klabel)]
           (if (and (<= lwidth w)
                    (<= lheight h))
             (let [keypaths (if keypath-fn
                              (keypath-fn obj)
                              (repeat nil))
                   parent-ref (atom nil)

                   key-rect (-> (make-rect w lheight)
                                (assoc :obj k)
                                (assoc :title k))
                   key-rect (with-meta key-rect
                              {::treemap-parent parent-ref
                               ::treemap-keypath (first keypaths)})

                   val-rect (make-rect w (- h lheight))
                   val-rect (update val-rect
                                    :y + lheight)
                   val-rect (with-meta val-rect
                              {::treemap-parent parent-ref
                               ::treemap-keypath (second keypaths)})
                   val-rect (treemap (val obj) val-rect child-options)
                   rect (assoc rect
                               :children [key-rect val-rect])]

               (reset! parent-ref rect)
               rect)
             (treemap obj rect (assoc options :recurse keyed-treemap))))
      (treemap obj rect (assoc options :recurse keyed-treemap))))))

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


(defui on-mouse-out [& {:keys [mouse-out body hover?]}]
  (if hover?
    (ui/on-mouse-move-global
       (fn [[x y]]
         (let [[w h] (ui/bounds body)]
           (when (or (neg? x)
                     (> x w)
                     (neg? y)
                     (> y h))
             (into
              [[:set $hover? false]]
              (mouse-out)))))
       body)
    (ui/wrap-on
     :mouse-move
     (fn [handler [x y :as pos]]
       (into [[:set $hover? true]]
             (handler pos)))
     body))
  )


(def pprint-memo (memoize #(let [s (with-out-str (clojure.pprint/pprint %))]
                             (subs s 0 (min (count s) 500)))))

(defn parent-lines [rect]
  (loop [rect rect
         lines []]
    (if rect
      (let [{:keys [x y w h]} rect
            parent (rect-parent rect)
            px (or (:x parent) 0)
            py (or (:y parent) 0)
            local-x (- x px)
            local-y (- y py)]
        (recur parent
               [(ui/path [0 0]
                         [x y])
                (ui/translate x y
                              lines)]))
      (ui/with-stroke-width 5
        (ui/with-style ::ui/style-stroke
          lines)))))


(defn treemap-keypath [rect]
  (loop [path ()
         rect rect]
    (if-let [parent (rect-parent rect)]
      (if-let [p (rect-keypath rect)]
        (recur (conj path [rect p])
               parent)
        path)
      path)))

(defn treemap-keypath-ui [rect]
  (let [keypath (treemap-keypath rect)]
    (apply
     vertical-layout
     (->> keypath
          (remove (fn [[rect path]]
                    (#{'key 'val} path)))
          (map (fn [[rect p]]
                 (on
                  :mouse-move
                  (fn [_]
                    [[:hover-keypath rect]])
                  :mouse-down
                  (fn [_]
                    [[::click-keypath rect]])
                  (if (and (seq p)
                           (#{'find 'nth} (first p)))
                    (horizontal-layout
                     (ui/label (second p))
                     ;; show statistics of key paths
                     #_(when-let [parent (rect-parent rect)]
                         (let [parent-obj (:obj parent)]
                           (horizontal-layout
                            (ui/label (str " / " (count parent-obj) " "))
                            (when (map? parent-obj)
                              (apply
                               horizontal-layout
                               (for [k (take 5 (keys parent-obj))
                                     :let [s (pr-str k)]]
                                 (ui/label (subs s 0 (min 12 (count s)))))))))))
                    (ui/label p)))))))))

(def treemap-keypath-ui-memo (memoize treemap-keypath-ui))

(defeffect ::select-rect [$select-rect rect]
  (dispatch! :set $select-rect {:plines (ui/with-color [0 0 0]
                                          (parent-lines rect))
                                :keypath-ui (treemap-keypath-ui-memo rect)
                                :obj (:obj rect)
                                :rect rect}))

(defeffect ::update-root-rect [$root-rect new-rect]
  (dispatch! :update $root-rect
             (fn [old-root]
               ;; if root-rect has new-rect as a parent. don't do anything
               (let [has-ancestor? (loop [rect old-root]
                                     (if rect
                                       (if (= rect new-rect)
                                         true
                                         (recur (rect-parent rect)))
                                       false))]
                 (if has-ancestor?
                   old-root
                   new-rect)))))


(defeffect ::descend [$select-rect $root-rect rect]
  (when (:children rect)
    (let [root-rect (dispatch! :get $root-rect)
          ;; try to find new rect by tracing root-rect
          new-rect (loop [new-rect root-rect
                          last-rect nil]
                     (when new-rect
                       (if (= new-rect rect)
                         (when last-rect
                           last-rect)
                         (recur (rect-parent new-rect)
                                new-rect))))]
      (if new-rect
        ;; slight race condition
        (dispatch! ::select-rect $select-rect new-rect)
        (let [new-rect (first (:children rect))]
          (dispatch! ::select-rect $select-rect new-rect)
          (dispatch! :set $root-rect new-rect))))))


(defeffect ::move-right [$select-rect $hover-rect $root-rect rect]
  (let [p (rect-keypath rect)]
    (case p
      key (when-let [prect (rect-parent rect)]
            (when-let [new-rect (second (:children prect))]
              (dispatch! ::select-rect $select-rect new-rect)
              (dispatch! :set $hover-rect nil)
              (dispatch! :set $root-rect new-rect)))
      val nil
      (when (seq p)
        (case (first p)

          find (let [k (second p)
                     prect (rect-parent rect)
                     childs (:children prect)
                     right-rect (second (drop-while #(not= rect %) childs))]
                 (when right-rect
                   (dispatch! ::select-rect $select-rect right-rect)
                   (dispatch! :set $hover-rect nil)
                   (dispatch! :set $root-rect right-rect)))

          nth (let [prect (rect-parent rect)
                    childs (:children prect)
                    i (min (dec (count childs))
                           (inc (second p)))
                    right-rect (nth childs i)]
                (dispatch! ::select-rect $select-rect right-rect)
                (dispatch! :set $hover-rect nil)
                (dispatch! :set $root-rect right-rect))

          nil)))))

(defeffect ::move-left [$select-rect $hover-rect $root-rect rect]
  (let [p (rect-keypath rect)]
    (case p
      key nil
      val (when-let [prect (rect-parent rect)]
            (when-let [new-rect (first (:children prect))]
              (dispatch! ::select-rect $select-rect new-rect)
              (dispatch! :set $hover-rect nil)
              (dispatch! :set $root-rect new-rect)))
      (when (seq p)
        (case (first p)
          find (let [k (second p)
                     prect (rect-parent rect)
                     childs (:children prect)
                     left-rect (last (take-while #(not= rect %) childs))]
                 (when left-rect
                   (dispatch! ::select-rect $select-rect left-rect)
                   (dispatch! :set $hover-rect nil)
                   (dispatch! :set $root-rect left-rect)))

          nth (let [i (max 0 (dec (second p)))
                    prect (rect-parent rect)
                    left-rect (nth (:children prect) i)]
                (dispatch! ::select-rect $select-rect left-rect)
                (dispatch! :set $hover-rect nil)
                (dispatch! :set $root-rect left-rect))
          nil)))))

(defui treemap-explore [& {:keys [tm-render select-rect hover-rect root-rect keypath-hover]}]
  (let [{:keys [rect plines keypath-ui obj]} (or hover-rect
                                                 select-rect)]
    (vertical-layout
     (ui/padding
      10 10
      (horizontal-layout
       (on
        ::treemap-hover
        (fn [rect]
          [[:set $hover-rect
            {:plines (ui/with-color [1 0 0]
                       (parent-lines rect))
             :keypath-ui (treemap-keypath-ui-memo rect)
             :obj (:obj rect)
             :rect rect}]])
        ::treemap-click
        (fn [rect]
          [[:set $hover-rect nil]
           [::select-rect $select-rect rect]])
        [(on
          :key-press
          (fn [s]
            (case s
              ("w" :up) (when-let [parent-rect (rect-parent rect)]
                          [[:set $hover-rect nil]
                           [::select-rect $select-rect parent-rect]
                           [::update-root-rect $root-rect rect]])
              ("a" :left) [[::move-left $select-rect $hover-rect $root-rect rect]]
              ("s" :down) [[::descend $select-rect $root-rect rect]]
              ("d" :right) [[::move-right $select-rect $hover-rect $root-rect rect]]

              nil))
          (on-mouse-out
           ;; :hover? (get extra :treemap-hover?)
           :mouse-out (fn []
                        [[:set $hover-rect nil]])
           :body tm-render))
         plines
         keypath-hover]
        )
       (when obj
         (ui/padding 10 5
                     (let [s (pprint-memo obj)]
                       (vertical-layout
                        (on
                         :hover-keypath
                         (fn [rect]
                           (let [[ox oy]
                                 (loop [rect rect
                                        ox 0
                                        oy 0]
                                   (if rect
                                     (recur (rect-parent rect)
                                            (+ ox (:x rect))
                                            (+ oy (:y rect)))
                                     [ox oy]))]
                             [[:set $keypath-hover
                               (ui/translate ox oy
                                             [(ui/filled-rectangle [1 1 1 0.8]
                                                                   (:w rect)
                                                                   (:h rect))
                                              (ui/with-style ::ui/style-stroke
                                                (ui/rectangle (:w rect)
                                                              (:h rect)))])]]))
                         ::click-keypath
                         (fn [rect]
                           [[::select-rect $select-rect rect]])
                         (on-mouse-out
                          :mouse-out (fn []
                                       [[:set $keypath-hover nil]])
                          :body keypath-ui))
                        (ui/label s
                                  #?(:cljs (ui/font "MonoSpace" nil)
                                     :clj (ui/font "Menlo.ttc" nil))))))))))))



(comment
  (require '[membrane.example.todo :as td])

  (def todo-state (atom {:todos
                         [{:complete? false
                           :description "first"}
                          {:complete? false
                           :description "second"}
                          {:complete? true
                           :description "third"}]
                         :next-todo-text ""}))
  (skia/run (component/make-app #'td/todo-app
                                todo-state))

  (def explore-state (atom {:tm-render nil
                            :selected nil}))

  (add-watch todo-state :explore
             (fn [k ref old new]
               (future
                 (let [tm (treemap (dissoc new
                                           ::component/extra
                                           ::component/context) (make-rect 400 400)
                                   {:branch? #(and (not (string? %)) (seqable? %))
                                    :children seq
                                    :size treemap-size
                                    :layout squarified-layout
                                    :min-area 1})

                       #_ #_ rendered (render-treemap-depth tm)
                       rendered
                       [(render-linetree tm)
                        (render-rect-vals tm)
                        ;; (render-bubbles tm)
                        ]]
                   (swap! explore-state
                          update :tm-render (constantly rendered))))))

  (skia/run (component/make-app #'treemap-explore explore-state)))





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



(defn treemap-rtree [tm]
  (loop [to-visit (seq [[0 0 tm]])
         rects []]
    (if to-visit
      (let [[ox oy rect] (first to-visit)]
        (if-let [children (:children rect)]
          (let [child-ox (+ ox (:x rect))
                child-oy (+ oy (:y rect))]
            (recur (into (next to-visit)
                         (map #(vector child-ox child-oy %) children))
                   (conj rects
                         (assoc (translate rect [ox oy])
                                :obj rect))))
          (recur (next to-visit)
                 (conj rects
                       (assoc (translate rect [ox oy])
                              :obj rect)))))
      (rtree/rtree rects))))


(defn tree-depth [tm]
  (loop [tm tm
         depth 0]
    (if tm
      (recur (rect-parent tm)
             (inc depth))
      depth)))

(defn wrap-treemap-events [tm body]
  (let [rt (treemap-rtree tm)]
    (on
     :mouse-move
     (fn [pos]
       (when-let [rect (->>  (rtree/search rt pos)
                             (sort-by tree-depth)
                             last
                             :obj)]
         [[::treemap-hover rect]]))

     :mouse-down
     (fn [pos]
       (when-let [rect (->>  (rtree/search rt pos)
                             (sort-by tree-depth)
                             last
                             :obj)]
         [[::treemap-click rect]]))
     (ui/no-events body))))

#?
(:clj
 (defn lets-explore
   ([]
    (lets-explore (doall (read-source))))
   ([obj]
    (lets-explore obj [800 800]))
   ([obj [w h]]
    (let [tm (keyed-treemap obj (make-rect w h))
          tm-render (wrap-treemap-events
                     tm
                     [;;(render-treemap tm)
                      (render-depth tm)
                      (render-linetree tm)
                      (render-rect-vals tm)
                      ;; (render-bubbles tm)
                      ])
          ]
      (skia/run (component/make-app #'treemap-explore {:tm-render (skia/->Cached tm-render)}))
      ))))




(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))



