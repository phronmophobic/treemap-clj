(ns treemap-clj.rtree


  #?(:clj (:import com.github.davidmoten.rtree.RTree
                   com.github.davidmoten.rtree.geometry.Geometries
                   ))
  )

(set!  *warn-on-reflection* true)
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

(defn rtree [rects]
 #?(:clj (jtree rects)))

(defn search [rt [x y :as pt]]
  #?(:clj (jsearch rt pt)))









;; (Geometries/circle 1.0 2.0 3.0)

;; com.github.davidmoten.rtree.geometry.Geometries.*;

;; RTree<String, Point> tree = RTree.create();
