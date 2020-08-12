# treemap-clj

An alternative to pprint for generically visualizing heterogeneous, hierarchical data

## Installation

Leiningen dependency:

```
[com.phronemophobic/treemap-clj "0.2.0"]
```

deps.edn dependency:

```
{:deps
 {
  com.phronemophobic/treemap-clj {:mvn/version "0.2.0"}
  }
}
```


![API](/doc/api.png?raw=true)



### Save a treemap as an image

```
(def obj {:a (range 5)
          :b (range 5)
          :c (range 5)})

;; layout the treemap rectangles
(require '[treemap-clj.core :as treemap])

(def tm (treemap/treemap obj
                         (treemap/make-rect 300 300)))
;; or

(def tm (treemap/keyed-treemap obj
                               (treemap/make-rect 300 300)))


;; render the treemap
(require '[treemap-clj.view :as tview])

;; render your desired layers
(def tm-rendered [(tview/render-depth tm)
                  ;; (tview/render-background-types tm)
                  ;; (tview/render-keys tm)
                  (tview/render-hierarchy-lines tm)
                  (tview/render-value-labels tm)
                  ])

;; save rendered treemap to image
(require 'membrane.java2d)

(membrane.java2d/save-to-image! "treemap.png" tm-rendered)

;; or draw to a buffered image
;; (membrane.java2d/draw-to-image tm-rendered)
```

### Run the built in treemap explorer

Currently only available on Mac OSX and Ubuntu

From the command line: `lein run -m treemap-clj.treem <edn or json file`

Note: you must have `data.json` (eg. `[org.clojure/data.json "1.0.0"]`) as a dependency to read json files.

From the repl:

```
(require '[treemap-clj.treem :as treem])

(def my-obj {:a 1})
(treem/app my-obj)
```

### Rendering your own treemap layers

The layout and rendering steps are intentionally broken up to make it so treemaps can be utilized in many contexts. 

Treemaps are layout out using `treemap-clj.core/treemap` or treemap-clj.core/keyed-treemap`. Both functions return a single `treemap-clj.core/Rect`.



```
> (treemap-clj.core/treemap {:a 1} (treemap-clj.core/make-rect 100 100))
 {:x 0,
  :y 0,
  :w 85,
  :h 85,
  :obj {:a 1},
  :children
  ({:x 0,
    :y 0,
    :w 70N,
    :h 70N,
    :obj [:a 1],
    :children
    ({:x 0, :y 0, :w 60N, :h 25N, :obj :a, :children nil}
     {:x 0, :y 35N, :w 60N, :h 25N, :obj 1, :children nil})})}
```

Each rectangle has enough info to draw a rectangle (keys `x`, `y`, `w`, `h`) as well as the object it represents `:obj`.

The `:children` are offset by all of their parents' `x` and `y` coodinates.

Rendering a layer is as simple as treewalking. 

#### Using simple loop/recur

```
(def black [0 0 0])
(defn render-dots [rect]
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
                       (ui/translate (+ ox (:x rect)) (+ oy (:y rect))
                                     (ui/filled-rectangle black 2 2 )
                                      )))))
      view)))
```

#### Using clojure.zip

`clojure.zip` makes treewalking very straightforward. This produces the same result as above.

```
(require '[clojure.zip :as z])
(def black [0 0 0])
(defn render-dots-zip [tm]
  (let [zip (z/zipper :children :children
                      (fn [rect children]
                        (ui/translate (:x rect)
                                      (:y rect)
                                      (vec children)))
                      tm)
        rect->dot (fn [rect]
                    (ui/translate
                     (:x rect) (:y rect)
                     (ui/filled-rectangle black 2 2 )))]
    (loop [zip zip]
      (if (z/end? zip)
        (z/root zip)
        (let [zip (if (z/branch? zip)
                    zip
                    (z/edit zip rect->dot))]
          (recur (z/next zip)))))
    )) 
```


## Fixing the mac osx dock icon appearing

On macosx, `keyed-treemap` may cause an annoying java dock icon to appear. There are 2 ways to prevent this:

1. add `-Dapple.awt.UIElement=true` to your java options
2. Use the following snippet before calling `keyed-treemap`
```
(let [ui-element (System/getProperty "apple.awt.UIElement")]
  (when (nil? ui-element)
    (System/setProperty "apple.awt.UIElement" "true")))
```



## License

Copyright © 2020 Adrian Smith

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
