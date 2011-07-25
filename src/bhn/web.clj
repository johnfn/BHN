(ns bhn.web
  (:use clojure.test)
  (:use clojure.contrib.java-utils)
  ; (:use clojure.contrib.duck-streams)
  (:use ring.adapter.jetty)
  (:use ring.util.response)
  (:use ring.middleware.reload)
  (:use ring.middleware.params)
  (:use ring.middleware.stacktrace))

(def posts (atom [{:url "some-url" :title "This is a title" :id (next-id)}]))
(def comments (atom [{:body "this is the body" :parent 1 :author "some dude"}]))

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defn hash-map? [x] 
  (= (type x) (type {}))) ;How is this not a builtin?

;;;;;;;;;;;;;;
; TEMPLATING ;
;;;;;;;;;;;;;;

; Given an attr hash-map, turn it into a string of key='value' pairs.
(defn attr [attrs]
  (apply str 
    (if (empty? attrs) 
      '("") 
      (flatten (list " " (map (fn [attr] (list (as-str (first attr)) "='" (second attr) "'")) attrs))))))

(defn surround [name x] (str "<" name ">" x "</" name ">"))

; Imagine if you could, whilst evaluating a symbol, also eval the tests.
; Now that would be MIND BLOWING. and FAST.

; Build arbitrary html symbols.

(defn make-wrapper [tag]
  (fn wrapper 
    ([] (str "<" tag ">" "" "</" tag ">"))
    ([& content] 
      (if (hash-map? (first content)) ; attrs list
        (apply str (concat (list "<" tag (attr (first content)) ">") (rest content) (list "</" tag ">")))
        (apply str (concat `("<" ~tag ">") content `("</" ~tag ">")))))))

(defmacro make-wrappers [& wrappers]
  (cons 'do (for [tag wrappers] `(def ~tag (make-wrapper '~tag)))))

(make-wrappers textarea html body title head h1 h2 p div a form input submit)

;
;
;

(defn get-post-by-id [id]
  (first (filter #(= (:id %) id) @posts)))

(let [id (atom 0)]
  (defn next-id []
    (swap! id inc)))

(defn post-link [params & args]
  (let [url (params "url")
        title (params "title")
        new-post {:url url :title title :id (next-id)}]
      (swap! posts conj new-post)
      "/")) ;issue redirect to /

(defn post-comment [params & args]
  (let [body (params "body")
        id (Integer/parseInt (second args))
        new-comment {:body body :author "you" :parent id}]
      (swap! comments conj new-comment)
      (first args)
    ))

; If URL matches URL-REGEX, call FUNC (assumed to be a function that renders 
; a template) with the groups matched in the regex as arguments. 
(defn handle 
  ([url url-regex func]
    (let [regex (re-pattern (str "^" url-regex "$"))
          pieces (re-find regex url)]
      (if pieces
        (apply func pieces))))
  ([url url-regex func params]
    (let [regex (re-pattern (str "^" url-regex "$"))
          pieces (re-find regex url)]
      (if pieces
        (apply func params pieces)))))

(defn handler [req]
  (let [uri (get req :uri)
        params (:form-params req)]
    (when-not (= uri "/favicon.ico")
        (if (not (empty? params))
          (redirect (or 
            (handle uri #"/addlink" post-link params)
            (handle uri #"/([\d]+)/comments\w*" post-comment params)
          ))
          (response (or 
            (handle uri #"/" index)
            (handle uri #"/addlink" addlink)
            (handle uri #"/([\d]+)/comments\w*" show-comments)
            "404"
          ))
          ))))

(def app
  (-> #'handler
    ; (wrap-reload '(bhn.web)) ; Totally useless with in-memory db because it
    ;                          ; gets annihilated every page refresh.
    (wrap-params)
    (wrap-stacktrace)))

(defn b-run-test[]
  (use 'bhn.web :reload)
  (run-tests 'bhn.web))

;tests

;(deftest test-templating
;  (is (= (render-template '(html) {}) "<html></html>"))
;  (is (= (render-template 
;         '(html (head (title "sup"))
;                (body (h1 "yup"))) {}) 
;         "<html><head><title>sup</title></head><body><h1>yup</h1></body></html>"))
;  (is (= (render-template
;           '(html (apply str (for [x (range 3)] (div x)))) {})
;           "<html><div>0</div><div>1</div><div>2</div></html>"))
;  (is (= (render-template
;           '(html (head) (body (apply str (for [x (range 3)] (div x))))) {})
;           "<html><head></head><body><div>0</div><div>1</div><div>2</div></body></html>"))
;  (is (= (render-template
;           '(html (head) (body (apply str (for [x (range 3)] (div x))))) {})
;           "<html><head></head><body><div>0</div><div>1</div><div>2</div></body></html>"))
;)


(defn -main []
  (defonce server (run-jetty app {:port 8989 :join? false}))
  (.start server))


;;;;;;;;;
; VIEWS ;
;;;;;;;;;

(defn index [& args]
  (html
    (head
      (title "Sup?"))
    (body
      (h1 "Better Hacker News")
      (a {:href "addlink"} "Add a link")
      (apply str 
        (for [post @posts] 
          (div 
            (a {:href (post :url)} (post :title)) " "
            (a {:href (str (post :id) "/comments")} "comments")
          )))
      (div "What's up?"))))

(defn addlink [& args]
  (html
    (head (title "new link"))
    (body
      (h1 "Add a link.")
      (form {:action "" :method "post"}
        (div "title" (input {:name "title"}))
        (div "url" (input {:name "url"}))
        (input {:type "submit"}))
    )))

(defn show-comments [& args]
  (let [url (first args)
        comment-id (Integer/parseInt (second args))
        post (get-post-by-id comment-id)]
    (html
      (head (title "This is the comments page"))
      (body
        (h1 (:title post))
        (apply str (for [comment @comments :when (= (:parent comment) comment-id)]
          (str
            (div (:body comment))
            (div (:author comment)))))
        (p)
        (form {:action "" :method "post"}
          (div "Leave a comment")
          (div "body" (textarea {:name "body"}))
          (input {:type "submit"}))
        ))))

