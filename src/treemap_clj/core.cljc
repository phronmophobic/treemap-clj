(ns treemap-clj.core
  (:require [clojure.zip :as z]
            [membrane.ui :as ui]
            #?(:clj membrane.java2d)))


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
                   ;; items in a strip get layed out horizontally
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


(defn zip-depth [loc]
  (-> loc second :pnodes count))

(defn max-depth [rect]
  (loop [zip (treezip rect)
         max-depth 0]
    (if (z/end? zip)
      max-depth
      (recur (z/next zip)
             (max max-depth (zip-depth zip))))))


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
  "Given an object and a rectangle, layout a treemap and return a treemap-clj.core/Rect

  `options` is a map that can containt the following keys:

  `branch?` branch? must be a fn of one arg that returns true if passed a node
   that can have children (but may not)

  `children` children must be a fn of one
   arg that returns a sequence of the children. Will only be called on
   nodes for which branch? returns true.

  `size` size is a function that returns the size of a node.
  If called on a branch?, the size must be the sum of its children.
  The size is used to calculate the area of the leaf nodes.

  `layout` layout is a function that takes 3 parameters
  objs: a sequence of objects
  sizes: a sequence of sizes that correspond to the objs
  rect: a Rect that specifies the bounds available
  The layout function should return a sequence of Rects that correspond the objs passed in.
  See `traditional-layout` for an example.

  `min-area` The minumum area that should try to be subdivided. `treemap` will
  stop subdividing once it breaks rectangles down into areas smaller than `min-area`.

  `padding` specifies the padding added at each level of the tree.
  Can either be a constant number or it can be function that takes 1 argument
  which is the current depth.

  `keypath-fn` Optional. if provided, should be a function that receives a
  tree branch and returns a sequence of keypaths that correspond to the
  children of the branch.

  `recurse` Optional. if provided, should be a function that
  will be called to subdivide the next level of the tree.
  "
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
  "Given an object and a rectangle, layout a treemap and return a treemap-clj.core/Rect.
  Will emphasize map keys if space is available. Rects that have enough room
  to fit the key title will have a `:title` key on the corresponding Rect.

  `options` is a map that can containt the following keys:

  `branch?` branch? must be a fn of one arg that returns true if passed a node
   that can have children (but may not)

  `children` children must be a fn of one
   arg that returns a sequence of the children. Will only be called on
   nodes for which branch? returns true.

  `size` size is a function that returns the size of a node.
  If called on a branch?, the size must be the sum of its children.
  The size is used to calculate the area of the leaf nodes.

  `layout` layout is a function that takes 3 parameters
  objs: a sequence of objects
  sizes: a sequence of sizes that correspond to the objs
  rect: a Rect that specifies the bounds available
  The layout function should return a sequence of Rects that correspond the objs passed in.
  See `traditional-layout` for an example.

  `min-area` The minumum area that should try to be subdivided. `treemap` will
  stop subdividing once it breaks rectangles down into areas smaller than `min-area`.

  `padding` specifies the padding added at each level of the tree.
  Can either be a constant number or it can be function that takes 1 argument
  which is the current depth.

  `keypath-fn` Optional. if provided, should be a function that receives a
  tree branch and returns a sequence of keypaths that correspond to the
  children of the branch.

  `recurse` Optional. if provided, should be a function that
  will be called to subdivide the next level of the tree.
  "
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




(defn tree-depth [tm]
  (loop [tm tm
         depth 0]
    (if tm
      (recur (rect-parent tm)
             (inc depth))
      depth)))



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))



