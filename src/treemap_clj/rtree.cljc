(ns treemap-clj.rtree
  #?(:cljs
     (:require [rtree]))

  #?(:clj (:import com.github.davidmoten.rtree.RTree
                   com.github.davidmoten.rtree.geometry.Geometries
                   ))
  )

#?
(:clj
 (defn jtree [rects]
   (reduce (fn [rt {:keys [x y w h] :as rect}]
             ;; tree = tree.add(item, Geometries.point(10,20));
             (let [geom (Geometries/rectangle
                         (double x)
                         (double y)
                         (double (+ x w))
                         (double (+ y h)))]
               (.add ^RTree rt rect geom)))
           (RTree/create)
           rects)))
#?
(:clj
 (defn jsearch [rt [x y]]
   (-> (.search ^RTree rt (Geometries/point (double x) (double y)))
       (.toBlocking)
       (.toIterable)
       (->> (map (fn [entry]
                   (.value ^com.github.davidmoten.rtree.Entry entry)))))))

#?
(:cljs
 (defn jstree [rects]
   (reduce (fn [rt {:keys [x y w h] :as rect}]
             (doto rt
               (.insert (js-obj
                         "x" x
                         "y" y
                         "w" w
                         "h" h)
                        rect)))
           (rtree)
           rects)))

#?
(:cljs
 (defn jssearch [rt [x y]]
   ;; myRTree.search({x:10, y:10, w:10, h:10});
   (vec (.search rt (js-obj "x" x
                            "y" y
                            "w" 1
                            "h" 1)))))

(defn rtree [rects]
  #?(:clj (jtree rects)
     :cljs (jstree rects)))

(defn search [rt [x y :as pt]]
  #?(:clj (jsearch rt pt)
     :cljs (jssearch rt pt)))






;; (Geometries/circle 1.0 2.0 3.0)

;; com.github.davidmoten.rtree.geometry.Geometries.*;

;; RTree<String, Point> tree = RTree.create();
