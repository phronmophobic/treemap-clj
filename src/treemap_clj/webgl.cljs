(ns treemap-clj.webgl
  (:require [goog.net.XhrIo :as xhr]
            [membrane.webgl :as webgl]
            [membrane.ui :as ui]
            [treemap-clj.core
             :refer [treemap
                     treemap-explore
                     render-depth
                     render-rect-vals
                     render-treemap
                     render-linetree
                     wrap-treemap-events
                     make-rect]]
            [membrane.component :as component]))

(defn $ [id]
  (js/document.getElementById id))
(def canvas ($ "canvas"))
(def blob-area ($ "blobarea"))
(def update-btn ($ "update-btn"))
(def url-input ($ "url-input"))
(def fetch-btn ($ "fetch-btn"))

;; checkboxes
(def background-types ($ "background-types"))
(def background-depth ($ "background-depth"))
(def lines ($ "lines"))
(def value-labels ($ "value-labels"))

(defonce app-state (atom {}))
(defn update-treemap [obj]
  (let [
        tm (treemap obj (make-rect 800
                                   800))
        tm-render (wrap-treemap-events
                   tm
                   [(cond
                      (.-checked background-depth)
                      (render-depth tm)

                      (.-checked background-types)
                      (render-treemap tm))
                    
                    (when (.-checked lines)
                      (render-linetree tm))
                    (when (.-checked value-labels)
                      (render-rect-vals tm))
                    
                    ;; (render-bubbles tm)
                    ])]
    (prn "updating?")
    (reset! app-state
            {:tm-render (webgl/->Cached tm-render)
             :tm tm})))

(defonce checkbox-listens (doseq [cb [background-types
                                      background-depth
                                      lines
                                      value-labels]]
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

(defonce fetch-listen (.addEventListener
                         fetch-btn
                         "click"
                         (fn []
                           (let [url (.-value url-input)]
                             (xhr/send url
                                       (fn [e]
                                         (let [x (.-target e)
                                               obj (js->clj (.getResponseJson x))]
                                           (update-treemap obj))))))))

(defn js-main [& args]
  (defonce start-app (membrane.webgl/run (component/make-app #'treemap-explore app-state) {:canvas canvas})))
