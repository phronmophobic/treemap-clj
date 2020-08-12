(ns treemap-clj.mdown
  (:require [clojure.zip :as z]
            [hiccup.core :refer [html]])
  (:import com.vladsch.flexmark.util.ast.Node
           com.vladsch.flexmark.html.HtmlRenderer
           com.vladsch.flexmark.parser.Parser
           com.vladsch.flexmark.ext.attributes.AttributesExtension
           com.vladsch.flexmark.ext.xwiki.macros.MacroExtension
           com.vladsch.flexmark.util.data.MutableDataSet))


;; //options.set(Parser.EXTENSIONS, Arrays.asList(TablesExtension.create(), StrikethroughExtension.create()));


(defn parse [s]
  (let [options (doto (MutableDataSet.)
                  (.set Parser/EXTENSIONS [(AttributesExtension/create)
                                           (MacroExtension/create)]))
        parser (-> (Parser/builder options)
                   (.build))
        doc (.parse parser s)]
    doc))

(defn doc->tree-seq [doc]
  (tree-seq #(.hasChildren %)
            #(seq (.getChildren %))
            doc))

(defn doc->zip [doc]
  (z/zipper #(.hasChildren %)
            #(seq (.getChildren %))
            identity
            doc))

(defn children [doc]
  (seq (.getChildren doc)))

(defprotocol IBlogHtml
  (blog-html [this]))

(extend-type com.vladsch.flexmark.util.ast.Document
  IBlogHtml
  (blog-html [this]
    [:div {}
     (map blog-html (children this))]))

(extend-type com.vladsch.flexmark.ast.Paragraph
  IBlogHtml
  (blog-html [this]
    [:p {}
     (map blog-html (children this))
     ]))

(extend-type com.vladsch.flexmark.ast.Heading
  IBlogHtml
  (blog-html [this]
    (let [tag (keyword (str "h" (.getLevel this)))]
      [tag {}
       (map blog-html (children this))])))

(extend-type com.vladsch.flexmark.ast.StrongEmphasis
  IBlogHtml
  (blog-html [this]
    [:strong
     (map blog-html (children this))]))


(extend-type com.vladsch.flexmark.ast.BlockQuote
  IBlogHtml
  (blog-html [this]
    [:blockquote.blockquote
     (map blog-html (children this))]))

(extend-type com.vladsch.flexmark.ast.FencedCodeBlock
  IBlogHtml
  (blog-html [this]
    [:pre
     [:code
      (map blog-html (children this))]]))


(extend-type com.vladsch.flexmark.ast.Code
  IBlogHtml
  (blog-html [this]
    [:code (map blog-html (children this))]))

(extend-type com.vladsch.flexmark.ast.Text
  IBlogHtml
  (blog-html [this]
    (str (.getChars this))))


(extend-type com.vladsch.flexmark.ast.Link
  IBlogHtml
  (blog-html [this]
    [:a {:href (-> this .getUrl str)}
     (-> this (.getText) str)]))

(extend-type com.vladsch.flexmark.ast.Image
  IBlogHtml
  (blog-html [this]
    [:img {:src (-> this .getUrl str)
           :alt (-> this (.getText) str)}]))

(extend-type com.vladsch.flexmark.ast.SoftLineBreak
  IBlogHtml
  (blog-html [this]
    [:br]))


(defmulti markdown-macro (fn [macro]
                           (.getName macro)))


(defmethod markdown-macro "blockquote-footer" [macro]
  [:footer.blockquote-footer
   (map blog-html (drop-last (children macro)))])

(extend-type com.vladsch.flexmark.ext.xwiki.macros.Macro
  IBlogHtml
  (blog-html [this]
    (markdown-macro this)))








(defn parse-blog []
  (-> (slurp "markdown/treemaps-are-awesome.md")
      parse
      blog-html))

(defn blog-page [body]
  [:html {:lang "en"}
   [:head

    [:meta {:charset "utf-8"}]
    [:meta {:name "viewport" :content "width=device-width, initial-scale=1, shrink-to-fit=no"}] 
    ;; [:meta {:name "description" :content ""}]
    [:meta {:name "author" :content "Adrian smith"}]

    ;; <link rel="icon" href="../../favicon.ico">
    [:title "Treemaps are awesome!"]

    [:link {:href "bootstrap.min.css"
            :rel "stylesheet"}]
    [:link {:href "blog.css"
            :rel "stylesheet"}]]

   [:body

    [:div {:class "blog-masthead"}
     [:div {:class "container"}
      [:nav.nav.blog-nav
       [:a.nav-link.active {:href "#"}
        "Treemaps are awesome!"]
       [:a.nav-link {:href "treemap-demo.html"}
        "Treemap Demo"]]]]
    [:div.blog-header
       [:div.container
        [:h1.blog-title "Treemaps are awesome!"]
        [:p.lead.blog-description "An alternative to pprint for generically visualizing heterogeneous, hierarchical data"]]]


    [:div.container
     [:div.row
      [:div.col-sm-8.blog-main
       [:div.blog-post
        body]]]]


    ]]
  )

(defn render-blog []

  (let [post-html (parse-blog)
        page-html (blog-page post-html)
        html-str (html page-html)]
    (spit "resources/public/treemaps-are-awesome.html"
          html-str))

  )

(defonce running? (atom false))
(defn watch-blog []
  (let [path "markdown/treemaps-are-awesome.md"
        f (clojure.java.io/file "markdown/treemaps-are-awesome.md")
        get-val #(.lastModified f)]
    (when (not @running?)
      (reset! running? true)
      @(future
        (loop [last-val nil]
          (let [current-val (get-val)]
            (when @running?
              (when (not= current-val last-val)

                (print "rendering blog...")
                (flush)
                (render-blog)
                (print " done.\n")
                (flush))

              (Thread/sleep 500 )
              (recur current-val))))))))

(defn -main [ & args]
  (watch-blog))

;; for fenced code blog
;; (.getinfo adsf) to find lang
(comment(-> (parse "```foo

(+ 1 2)
```
")
            doc->zip
            z/next
            z/node
            (.getInfo)
            ))
