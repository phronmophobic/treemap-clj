(ns treemap-clj.view
  (:require [membrane.ui :as ui
             :refer [on]]
            [treemap-clj.core :refer [max-depth
                                      make-rect
                                      rect-parent
                                      rect-keypath
                                      translate
                                      tree-depth
                                      keyed-treemap
                                      treemap
                                      treemap-options-defaults]]
            [membrane.component :as component
             :refer [defui
                     defeffect]]
            clojure.pprint
            [membrane.basic-components :refer [on-mouse-out]]
            [treemap-clj.rtree :as rtree]))


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


(defn render-background-types
  "Draw filled rectangles of leaf rects
  with colors corresponding to the types."
  ([rect]
   (render-background-types rect {:opacity 0.2}))
  ([rect {:keys [opacity]
          :as opts}]
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
  "Draw filled rectangles of leaf rects
  with colors corresponding to the depth."
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

(defn render-hierarchy-lines [rect]
  "Draw hierarchy lines"
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

(defn render-value-labels [rect]
  "Draw value labels of leaf nodes."
  (loop [to-visit (seq [[0 0 0 rect]])
         rt (rtree/rtree)
         view []]
    (if to-visit
      (let [[depth ox oy rect] (first to-visit)]
        (if-let [children (:children rect)]
          (let [ox (+ ox (:x rect))
                oy (+ oy (:y rect))]
            (recur (seq
                    (concat (next to-visit)
                            (map #(vector (inc depth) ox oy %) children)))
                   rt
                   view
                   ))
          (let [lbl (if-let [title (:title rect)]
                      (ui/label title)
                      (ui/with-color (rect-val-color depth)
                        (pr-label (:obj rect) 30 (ui/font mono-font (depth-font-size depth)))))
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

