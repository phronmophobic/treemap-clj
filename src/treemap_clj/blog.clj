(ns treemap-clj.blog
  (:require [clojure.spec.gen.alpha :as gen]
            [clojure.spec.alpha :as s]
            [membrane.ui :as ui
             :refer [vertical-layout
                     horizontal-layout]]
            [clojure.data.json :as json]
            [membrane.basic-components :as basic]
            [membrane.component :as component
             :refer [defui defeffect]]
            [clojure.repl :refer [source-fn]]
            [clojure.zip :as z]
            [clojure.tools.namespace.find :as find]
            [clojure.java.classpath :as classpath]
            [membrane.skia :as skia]
            [treemap-clj.view :refer [render-hierarchy-lines
                                      type-color-legend
                                      render-background-types
                                      depth-color-legend
                                      render-depth
                                      depth-line-legend
                                      render-value-labels
                                      render-keys
                                      lets-explore]]
            [treemap-clj.core :refer [keyed-treemap
                                      treemap
                                      make-rect
                                      treemap-options-defaults
                                      treezip
                                      zip-depth]])
  
  )


(s/def ::tree (s/keys :req [::children]))



(s/def ::leaf (s/or :number? number?
                    :string? string?
                    :symbol? symbol?
                    :boolean? boolean?
                    :nil? nil?))


(s/def ::tree-map (s/map-of ::leaf 
                            ::tree-node))
(s/def ::tree-seq (s/coll-of ::tree-node 
                             :max-count 5))
(s/def ::tree-node (s/or :leaf ::leaf
                         :map ::tree-map
                         :seq ::tree-seq))


(s/def ::children (s/coll-of (s/or :tree ::tree
                                   :leaf ::leaf)
                             :max-count 5))
(def running? (atom true))
(defeffect ::gen-random [$tm-render $obj]
  (loop [[tm-render obj :as result] nil ]
    (if result
      (do
        (dispatch! :set $obj obj)
        (dispatch! :set $tm-render (skia/->Cached tm-render)))
      (when @running?
        (recur
         (try
           (let [[w h] [400 200]
                 obj (gen/generate (s/gen ::tree-node))
                 tm (keyed-treemap obj (make-rect w h)
                                   (merge treemap-options-defaults
                                          {:padding 0}))
                 tm-render [(render-hierarchy-lines tm)]
                 ]
             [tm-render obj])
           (catch Exception e
             nil))
         )))
    ))
(defonce random-gen-state (atom {}))
(defui random-generator [& {:keys [tm-render obj]}]
  (vertical-layout
   (basic/button :text "random"
                 :on-click
                 (fn []
                   [[::gen-random $tm-render $obj]]))
   tm-render))
#_(skia/run (component/make-app #'random-generator random-gen-state))

(def tree-memo
  (memoize
   (fn [obj]
     (try
       (let [[w h] [400 200]
             tm (keyed-treemap obj (make-rect w h)
                               (merge treemap-options-defaults
                                      {:padding 0}))
             tm-render [(render-hierarchy-lines tm)]
             ]
         tm-render)
       (catch Exception e
         (ui/label "error"))))))

(def test-obj (atom nil))

(def example [(range 3)
              [[(range 5)
                (range 2)
                (range 3)]
               [(range 5)
                (range 2)
                (range 3)
                (range 3)]
               1]
              [[(range 5)
                (range 2)
                (range 3)]
               [(range 5)
                (range 2)
                (range 3)
                (range 3)]
               1]
              [[(range 5)
                (range 2)
                (range 3)]
               [(range 5)
                (range 2)
                (range 3)]
               [(range 5)
                (range 2)                     
                (range 2)                     
                (range 2)
                [(range 5)
                 (range 2)
                 (range 3)]
                [(range 5)
                 (range 2)
                 (range 3)]
                [(range 5)
                 (range 2)
                 (range 3)]
                (range 2)                     
                (range 2)                     
                (range 2)                     
                (range 3)]

               [(range 5)
                (range 2)
                [(range 5)
                 (range 2)
                 (range 3)]
                [(range 5)
                 (range 2)
                 (range 3)]
                [(range 5)
                 (range 2)
                 (range 3)]
                (range 3)]
               [(range 5)
                (range 3)]
               1]
              ])

(defn render-subdivide-example
  ([rect max-depth]
   (loop [to-visit (seq [[0 0 0 rect]])
          view []]
     (if to-visit
       (let [[depth ox oy rect] (first to-visit)]
         (if (and (:children rect)
                  (< depth max-depth))
           (let [children (:children rect)
                 ox (+ ox (:x rect))
                 oy (+ oy (:y rect))]
             (recur (into (next to-visit)
                          (map #(vector (inc depth) ox oy %) children))
                    (conj view
                          (ui/translate ox oy
                                        (ui/rectangle (max 1 (dec (:w rect)))
                                                      (max 1 (dec (:h rect)))))
                          )))
           (recur (next to-visit)
                  (if (and (> (:w rect) 5)
                           (> (:h rect) 5))
                    (conj view
                          
                          (ui/translate (+ (:x rect) ox) (+ (:y rect) oy)
                                        (ui/rectangle (max 1 (dec (:w rect)))
                                                      (max 1 (dec (:h rect))))))
                    view
                    )
                  )
           ))
       (ui/with-style ::ui/style-stroke
         view)))))

#_(skia/run #(render-subdivide-example (treemap example (make-rect 300 300)
                                    (merge treemap-options-defaults
                                           {:padding 0}))
                           7
                 ))

#_(doseq [i (range 6)]
  (skia/draw-to-image! (str "resources/public/images/basic-subdivide-" i ".png")
                       (render-subdivide-example (treemap example (make-rect 400 200)
                                                (merge treemap-options-defaults
                                                       {:padding 0}))
                                       i
                                       )))
(defn prune-depth [tree]
  (loop [zip (treezip tree)]
    (if (z/end? zip)
      (z/root zip)
      (if (> (zip-depth zip) 3)
        (recur (-> zip
                   (z/edit #(gen/generate (s/gen ::leaf)))
                   z/next))
        (recur (-> zip z/next))))))


#_(def type-example (prune-depth
                     (gen/generate (s/gen ::tree) )))
(def type-example
  '#:treemap-clj.core{:children
                   [nil
                    nil
                    #:treemap-clj.core{:children [nil true]}
                    #:treemap-clj.core{:children
                                       [-185
                                        #:treemap-clj.core{:children
                                                           [#:treemap-clj.core{:children
                                                                               [true
                                                                                #:treemap-clj.core{:children
                                                                                                   [4463601
                                                                                                    nil]}
                                                                                #:treemap-clj.core{:children
                                                                                                   [nil]}
                                                                                #:treemap-clj.core{:children
                                                                                                   [foo
                                                                                                    -263.53125]}
                                                                                #:treemap-clj.core{:children
                                                                                                   []}]}
                                                            #:treemap-clj.core{:children
                                                                               [-4979043
                                                                                #:treemap-clj.core{:children
                                                                                                   ["8N78NMyaQ2Ocs4PI52XL01x0D"]}]}]}
                                        #:treemap-clj.core{:children
                                                           []}
                                        #:treemap-clj.core{:children
                                                           [#:treemap-clj.core{:children
                                                                               []}]}]}
                    #:treemap-clj.core{:children
                                       [#:treemap-clj.core{:children
                                                           [#:treemap-clj.core{:children
                                                                               [#:treemap-clj.core{:children
                                                                                                   [true
                                                                                                    nil]}
                                                                                #:treemap-clj.core{:children
                                                                                                   []}
                                                                                #:treemap-clj.core{:children
                                                                                                   [false]}
                                                                                #:treemap-clj.core{:children
                                                                                                   []}
                                                                                -61.40625]}
                                                            nil
                                                            #:treemap-clj.core{:children
                                                                               [nil
                                                                                nil
                                                                                -378]}
                                                            0.233978271484375]}]}]}
  )
#_(def type-example (prune-depth
                     (gen/generate (s/gen ::tree) )))

(defn render-type-example []
  (skia/->Cached
   (horizontal-layout
    (type-color-legend)
    (ui/spacer 50 0)
    (render-background-types
     (treemap type-example (make-rect 400 200)
              (merge treemap-options-defaults
                     {:padding 0}))
                    1)))
  )
#_(skia/run render-type-example)

#_(skia/draw-to-image! (str "resources/public/images/type-background.png")
                     (render-type-example))

(defn render-depth-example []
  (skia/->Cached
            (horizontal-layout
             (depth-color-legend)
             (ui/spacer 50 0)
             (render-depth (treemap type-example (make-rect 400 200)
                                    (merge treemap-options-defaults
                                           {:padding 0}))
                           1))))


#_(skia/draw-to-image! (str "resources/public/images/depth-background.png")
                     (render-depth-example))


(defn render-depth-padding-example []
  (skia/->Cached
   (horizontal-layout
    (depth-color-legend)
    (ui/spacer 50 0)
    (render-depth (treemap type-example (make-rect 400 200)
                           (merge treemap-options-defaults
                                  {:padding (fn [depth]
                                              (get {0 4
                                                    1 10
                                                    2 4
                                                    3 4
                                                    4 4
                                                    5 4}
                                                   depth
                                                   0))}))
                  1))))

#_(skia/run render-depth-padding-example)
#_(skia/draw-to-image! (str "resources/public/images/depth-padding.png")
                     (render-depth-padding-example))

;; randomly generated
(def line-example-data
  '[[[{nil D*+7Ad76,
       -0.373779296875 "Xo4trn6Wqjl43jfZ0mVm8g",
       !05-24+- 0.056640625,
       AGs*6ARp -3,
       "9Sb3C3VQMLoqxkfheX8yX82Z" nil,
       "BX14NY72eht0g3l260" "2PZ94Pjc6Kfw2",
       "4cP3g" "L4iSE78",
       false nil,
       "rlv9" n34OAHz.}
      {"42x0w8vmqD" "aE26143jpjVhhQEhAd4vxjDH",
       341 false,
       -0.008112155366688967 false,
       false nil,
       rmY5-.V3 ?T+.9oaH,
       nil true,
       "KVS6C2z773" "sr51xIb3gKwF3YEqUgo9IdD",
       -203 -1}
      {nil "4N",
       0 "8R2855fZWJUlQcAQ",
       "" -3,
       14.375 true,
       53.2451171875 I3LNY7?r,
       true false,
       "V4v8ln9w82EoJOtLp52kD" false,
       U-_8z!.8 l*45cp.!,
       false II--_+4*,
       mW7w9!A4 -0.0257912278175354,
       "w50Bh63qk5x5rIV2qd9WqBl0AfV" "1r0HwUiSqr7"}]
     {28585171
      {true N-4h0-,
       2.139129400253296 true,
       91.15625 true,
       T8w1V nil,
       "1TA" true}}
     {true [o8w5+09_],
      117.71711921691895 "7d4g7Z4sxigQJb7v6iV3Q1x83eo",
      false {"2iaA3" nil, nil "wVRbx6kUQQ07MfC17P4"},
      +?*cV!2B
      {nil true,
       -8.0 false,
       306.25 "2XK7DXF5WXcL93I9",
       0.0044574737548828125 ?__w*.X5,
       -887222 717,
       e!m false,
       h4.H4 nil,
       true nil,
       "M3Nn8Uy7Y2ug6kj2tI0OHzmuVKY" *+A7pK.8,
       "1TTIx7R2irV0UES6bsMoo60r" "waRPMj2a4X0d4fSM1H",
       NO_aSDPJ nil,
       false "wVYoSoBQZEhv2wz",
       -41.04867696762085 -47.24246463179588},
      nil
      {-151155 "nDKQuF5sxDZ04FIecF",
       nil -804434,
       p?OHkR+8 nil,
       true 362368192,
       -2383952 nil},
      -2.3046875 ["87X7znC8Lt301lt4Kk" / true false "2RPZ69O0pgKd"],
      ?6-+b*g! [!-+V?.?3 -22 "86aWmu8i1ZB7F6T4r4" .H_+pfB1]}
     [{nil _FY3!4.P,
       "2f1A2Iai2kG7Z9z9Wi355Xpx" false,
       -14.128662109375 GX_1Q*+!,
       11254529 true,
       !9_t62*t Ds7O7!T+,
       -231135 ##NaN,
       !KKL7!DT 4,
       true .V?R0W?g,
       89354882 -159887,
       false nil,
       "3y9l3azvtbLQ9lltkWkU" nil,
       +bcE?Wbu false,
       .7?8G8Y- S-+U-4J.}
      253
      {nil 86.0,
       e!?Yw+4h _.h?-0,
       9.57666015625 true,
       -6243880 "2jcn6Qbm",
       292126 -290,
       l+5t800y -20085,
       "5P3hZu1fGKfhK36M91waaV4Z" true,
       true !-.xaT_g,
       "8K" -0.12412834167480469,
       "3a1zi7X" nil,
       "pEi6FH49JEiW5IDTi48949fC" "356ENTTtyrvD4lLP0CY4f9c9s",
       false "26EI4iN4",
       h--hs.w- Lq.?!!r,
       "995oDTNg0R" "88"}]
     [{"Th6XP2E3yIqbclAB" false,
       true "9rwle6J1Q9",
       "49784V65c4FL077LbkVVIoCl9s02" Rgy!,
       -4 nil,
       "3Tawwg" cV!lz_7*,
       141373 Y.Jh_z?-,
       nil "ngtl"}
      nil
      {true _-T,
       "3LqcZhJfb0YnmZX1VdwRFpC4u" "a4VJi7jP0N0rG12",
       nil "",
       "don337Zghgof4R58ASVP" -122.0390625}
      {nil "1D8R9v7YAo3yVwmM",
       -2.98870849609375 ZA8R4e+8,
       -70.96875 nil,
       "uvm7ApYOj9p5S145dN" nil,
       *?x.p-?5 nil,
       u-!+4S*i "76BJhgK2X2rd",
       "RV6hYT80CMsJ" "a",
       "USf8i42z9NT20yX2sNMvE0" nil,
       "xQI6rVtZjP31cZ5CE" tRBg-J,
       _sX6?+D4 nil,
       T+Q+lxM2 nil,
       -5962 false}]]])

(defn clj-zip [obj]
  (z/zipper seqable?
            identity
            (fn [node children]
              (if (map-entry? node)
                (vec children)
                (into (empty node) children)))
            obj))

(defn shorten-names [tree]
  (let [max-length 8]
    (loop [zip (clj-zip tree)]
      (if (z/end? zip)
        (z/root zip)
        (let [node (z/node zip)
              zip (cond
                    (symbol? node)
                    (z/edit zip (fn [sym]
                                  (let [s (name sym)]
                                    (symbol (subs s 0 (min 8 (count s)))))))

                    
                    :else
                    zip)]
          (recur (z/next zip))
          )))))

(defn render-line-example []
  (skia/->Cached
   (let [tm (treemap line-example-data (make-rect 400 200)
                     (merge treemap-options-defaults
                            {:padding (fn [depth]
                                        (get {0 4
                                              1 10
                                              2 4
                                              3 4
                                              4 4
                                              5 4}
                                             depth
                                             0))}))]
     (horizontal-layout
      (depth-line-legend)
      (ui/spacer 50 0)
      [(render-hierarchy-lines tm)]))))
#_(skia/run render-line-example
  )
#_(skia/draw-to-image! (str "resources/public/images/line-bare-demo.png")
                     (render-line-example))

#_(lets-explore line-example-data [400 200])


(def label-example-data
  {:a (range 5)
   :b (range 5)
   :c (range 5)})


(defn render-label-example []
  (skia/->Cached
   (let [tm (treemap label-example-data (make-rect 400 200)
                     (merge treemap-options-defaults
                            {:padding (fn [depth]
                                        (get {0 4
                                              1 10
                                              2 4
                                              3 4
                                              4 4
                                              5 4}
                                             depth
                                             0))}))]
     [(render-background-types tm)
      (render-hierarchy-lines tm)
      (render-value-labels tm)
      ])))


#_(skia/run render-label-example
  )
#_(skia/draw-to-image! (str "resources/public/images/simple-label-example.png")
                     (render-label-example))


(defn namespace-map []
  (let [nss (->> (find/find-namespaces(classpath/classpath))
                 (filter #(.startsWith (name %) "clojure.")))]
    (doseq [ns nss]
      (try
        (require ns)
        (catch Exception e
          (println e))))
    (let [publics (loop [publics {}
                         nss (seq nss)]
                    (if nss
                      (let [ns (find-ns (first nss))]
                        (if ns
                          (let [key-path (conj (clojure.string/split (name (ns-name ns)) #"\.")
                                               "public-interns")
                                ]
                            (recur (assoc-in publics
                                             key-path
                                             (keys (ns-publics ns))
                                             #_(into {}
                                                     (for [[k v] (ns-publics ns)]
                                                       [k
                                                        (try
                                                          (source-fn (symbol (name (ns-name ns)) (name k)))
                                                          (catch Exception e
                                                            nil))]))
                                             )
                                   (next nss)))
                          (recur publics
                                 (next nss))))
                      publics))]
      publics)))



(def nm (delay (namespace-map)))
(defn render-keyed-example []
  (skia/->Cached
   (let [tm (keyed-treemap @nm (make-rect 400 400))]
     [(render-depth tm 0.4)
      (render-keys tm)
      ])))





(comment
  #_(lets-explore @nm [400 400])
  #_(skia/run render-keyed-example)
  #_(skia/draw-to-image! (str "resources/public/images/keyed-example.png")
                         (render-keyed-example))
  (lets-explore
   (json/read-str (slurp
                   "/var/tmp/test-savegame.json"))[400 200]))


;; Sizes
(def example-sizes
  [[100 100]
   [100 200]
   [200 100]
   [200 200]
   [200 450]
   [450 200]
   [450 450]])

(defn export-size-example []
  (let [[max-w max-h] [(reduce max (map first example-sizes))
                       (reduce max (map second example-sizes))]
        example-data (json/read-str (slurp
                                     "/var/tmp/test-savegame.json"))
        ]
    (doseq [[w h] example-sizes
            :let [tm (keyed-treemap example-data (make-rect w h))
                  view [(ui/spacer max-w max-h)
                        (render-depth tm (if (< (* w h) (* 200 200) )
                                           1
                                           0.4))
                        (render-keys tm)
                        (ui/translate 0.5 0.5
                                      (ui/with-style ::ui/style-stroke
                                        (ui/rectangle (dec w) (dec h))))]]]
      (skia/draw-to-image! (str "resources/public/images/sizes-example-" w "x" h ".png")
                           view))))

