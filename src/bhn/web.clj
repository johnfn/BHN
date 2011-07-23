(ns bhn.web
  (:use clojure.test)
  (:use clojure.contrib.java-utils)
  (:use ring.adapter.jetty)
  (:use ring.util.response)
  (:use ring.middleware.reload)
  (:use ring.middleware.stacktrace))

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

(make-wrappers html body title head h1 h2 div a)

;;;;;;;
; WEB ;
;;;;;;;

(defn index[]
  (html
    (head
      (title "Sup?"))
    (body
      (h1 "Better Hacker News")
      (a {:href "addlink"} "Add a link")
      (apply str (for [x (range 10)] (div (str "Post number " x))))
      (div "What's up?"))))

(defn addlink[]
  (html))
  
(defn handler [req]
  (case (get req :uri)
    "/" (response (index))
    "/test" (response "herp")
))

(def app
  (-> #'handler
    (wrap-reload '(bhn.web))
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
  (run-tests)
  (let [port 8989]
    (run-jetty app {:port port})))

