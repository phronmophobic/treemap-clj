(ns treemap-clj.rtree
  #?(:cljs
     (:require [rtree :as rt]))

  #?(:clj (:import com.github.davidmoten.rtree.RTree
                   com.github.davidmoten.rtree.Entries
                   com.github.davidmoten.rtree.geometry.Geometries
                   ))
  )

#?
(:clj
 (defn jtree [rects]
   (RTree/create (map (fn [{:keys [x y w h] :as rect}]
                         (let [geom (Geometries/rectangle
                                    (double x)
                                    (double y)
                                    (double (+ x w))
                                    (double (+ y h)))]
                           (Entries/entry rect geom)))
                      rects))
   #_(reduce (fn [rt {:keys [x y w h] :as rect}]
               ;; tree = tree.add(item, Geometries.point(10,20));
               (let [geom (Geometries/rectangle
                           (double x)
                           (double y)
                           (double (+ x w))
                           (double (+ y h)))]
                 (.add ^RTree rt rect geom)))
             (-> (RTree/star)
                 (.maxChildren 4)
                 (.create)
                 )
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
(:clj
 (defn jadd [rt {:keys [x y w h] :as rect}]
   (let [geom (Geometries/rectangle
                                    (double x)
                                    (double y)
                                    (double (+ x w))
                                    (double (+ y h)))]
     (.add ^RTree rt rect geom))))

#?
(:cljs
 (defn jsadd! [rt {:keys [x y w h] :as rect}]
   (doto rt
     (.insert (js-obj
               "x" x
               "y" y
               "w" w
               "h" h)
              rect))))

#?
(:cljs
 (defn jstree [rects]
   (reduce (fn [rt rect]
             (jsadd! rt rect))
           (rt)
           rects)))

#?
(:cljs
 (defn jssearch [rt [x y]]
   ;; myRTree.search({x:10, y:10, w:10, h:10});
   (vec (.search rt (js-obj "x" x
                            "y" y
                            "w" 1
                            "h" 1)))))



(defn rtree
  ([]
   #?(:cljs (rt)
      :clj (-> (RTree/star)
               (.maxChildren 4)
               (.create) )))
  ([rects]
   #?(:clj (jtree rects)
      :cljs (jstree rects))))

(defn search [rt [x y :as pt]]
  #?(:clj (jsearch rt pt)
     :cljs (jssearch rt pt)))



(defn add! [rt rect]
  #?(:cljs (jsadd! rt rect)
     :clj (jadd rt rect)))






;; (Geometries/circle 1.0 2.0 3.0)

;; com.github.davidmoten.rtree.geometry.Geometries.*;

;; RTree<String, Point> tree = RTree.create();
