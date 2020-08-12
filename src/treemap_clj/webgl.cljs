(ns treemap-clj.webgl
  (:require-macros [membrane.component :as component
                    :refer [defui
                            defeffect]])
  (:require [goog.net.XhrIo :as xhr]
            [membrane.webgl :as webgl]
            [membrane.ui :as ui]
            cljs.reader
            [treemap-clj.view
             :refer [render-depth
                     render-value-labels
                     render-background-types
                     render-hierarchy-lines
                     render-keys
                     treemap-explore
                     wrap-treemap-events]]
            [treemap-clj.core
             :refer [treemap
                     keyed-treemap
                     make-rect]]
            [membrane.component :as component]))

(defn $ [id]
  (js/document.getElementById id))
(def canvas ($ "canvas"))
(def blob-area ($ "blobarea"))
(def update-btn ($ "update-btn"))
(def url-input ($ "url-input"))
(def fetch-example-select ($ "fetch-example-select"))
(def fetch-btn ($ "fetch-btn"))
(def fetch-example-btn ($ "fetch-example-btn"))

;; checkboxes
(def background-types ($ "background-types"))
(def background-depth ($ "background-depth"))
(def lines ($ "lines"))
(def value-labels ($ "value-labels"))
(def keyed-cb ($ "keyed"))

(def canvas-sizes (js/document.getElementsByName "canvas-size"))

(defonce app (atom nil))
(defonce app-state (atom {}))
(defn update-treemap [obj]
  (let [
        keyed? (.-checked keyed-cb)

        selected-size-cb (some #(when (.-checked %)
                                  %)
                               canvas-sizes)
        size-str (when selected-size-cb
                   (.-value selected-size-cb))

        [w h] (if size-str
                (->> (clojure.string/split size-str "x" )
                     (map #(js/parseInt % 10)))
                [800 800])


        treemap-rect (make-rect w h)

        tm (if keyed?
             (keyed-treemap obj treemap-rect)
             (treemap obj treemap-rect))

        background-opacity (if (<= (* w h) (* 350 350))
                             0.5
                             0.2)
        tm-render (wrap-treemap-events
                   tm
                   [(cond
                      (.-checked background-depth)
                      (render-depth tm background-opacity)


                      (.-checked background-types)
                      (render-background-types tm background-opacity))
                    

                    (when (.-checked lines)
                      (render-hierarchy-lines tm))

                    (cond
                      (.-checked value-labels)
                      (render-value-labels tm)

                      keyed?
                      (render-keys tm))


                    
                    ;; (render-bubbles tm)
                    ])]

    (reset! app-state
            {:tm-render (webgl/->Cached tm-render)
             :tm tm})
    (membrane.webgl/redraw @app)))

(defonce checkbox-listens (doseq [cb [background-types
                                      background-depth
                                      lines
                                      value-labels
                                      keyed-cb]]
                            (.addEventListener cb
                                               "click"
                                               (fn [e]
                                                 (update-treemap (:obj (:tm @app-state)))
                                                 ))))

(defonce radio-canvas-size-listens (doseq [cb canvas-sizes]
                                     (.addEventListener cb
                                                        "click"
                                                        (fn [e]
                                                          (update-treemap (:obj (:tm @app-state)))
                                                          ))))

(defonce button-listen (.addEventListener
                        update-btn
                        "click"
                        (fn []
                          (let [blob (.-value blob-area)
                                obj (js->clj (js/JSON.parse blob))]
                            (update-treemap obj)))))

(defn parse-edn-or-json [s]
  (try
    (cljs.reader/read-string s)
    (catch js/Object e
      (js->clj (js/JSON.parse s)))))

(defonce fetch-listen (.addEventListener
                       fetch-btn
                       "click"
                       (fn []
                         (let [url (.-value url-input)]
                           (xhr/send url
                                     (fn [e]
                                       (let [x (.-target e)
                                             obj (parse-edn-or-json (.getResponseText x))]
                                         (update-treemap obj))))))))

(defonce fetch-example-listen (.addEventListener
                               fetch-example-btn
                               "click"
                               (fn []
                                 (let [url (.-value fetch-example-select)]
                                   (xhr/send url
                                             (fn [e]
                                               (let [x (.-target e)
                                                     obj (parse-edn-or-json (.getResponseText x))]
                                                 (update-treemap obj))))))))

(defui web-wrapper [& {:keys [tm-render loading?]}]
  (if tm-render
    (treemap-explore :tm-render tm-render)
    (if loading?
      (ui/label "loading...")
      (ui/label "No data. Try loading or fetching some!"))))


(defn js-main [& args]
  (reset! app (membrane.webgl/run (component/make-app #'web-wrapper app-state) {:canvas canvas})))
