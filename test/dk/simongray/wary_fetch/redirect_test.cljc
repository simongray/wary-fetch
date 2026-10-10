(ns dk.simongray.wary-fetch.redirect-test
  "The request that follows a redirect: its method, its body, and the
  credentials that stay behind."
  (:require [clojure.test :refer [deftest is testing]]
            [dk.simongray.wary-fetch.redirect :as redirect]))

(deftest the-request-after-a-redirect
  (let [post  {:method  :post
               :url     "https://a.example/x"
               :headers {"Authorization" "Basic x" "Cookie" "c=1" "Accept" "*/*"}
               :body    "{}"}
        there "https://a.example/y"]
    (testing "a 303, and a 301 or 302 to a POST, turn into a GET without a body"
      (is (= {:method :get :url there :headers (:headers post) :body nil} (redirect/next-request post 303 there)))
      (is (= [:get nil] ((juxt :method :body) (redirect/next-request post 301 there))))
      (is (= [:get nil] ((juxt :method :body) (redirect/next-request post 302 there))))
      (is (= [:put "{}"] ((juxt :method :body) (redirect/next-request (assoc post :method :put) 302 there)))))
    (testing "a 307 and a 308 keep the method and the body"
      (is (= [:post "{}"] ((juxt :method :body) (redirect/next-request post 307 there))))
      (is (= [:post "{}"] ((juxt :method :body) (redirect/next-request post 308 there)))))
    (is (= :head (:method (redirect/next-request (assoc post :method :head) 303 there))) "a HEAD stays a HEAD")
    (testing "credentials stay behind when the redirect leaves the origin"
      (is (= {"Accept" "*/*"} (:headers (redirect/next-request post 307 "https://b.example/x"))))
      (is (= {"Accept" "*/*"} (:headers (redirect/next-request post 307 "https://a.example:8443/x"))) "another port")
      (is (= {"Accept" "*/*"} (:headers (redirect/next-request post 307 "http://a.example/x"))) "another scheme")
      (is (= (:headers post) (:headers (redirect/next-request post 307 "https://A.example:443/x")))
          "the same origin, written another way"))
    (is (= 5 (:timeout-seconds (redirect/next-request (assoc post :timeout-seconds 5) 307 there)))
        "the rest of the request stays")))

(deftest where-a-redirect-leads
  (is (= "https://a.example/y" (redirect/target "https://a.example/x" "/y")))
  (is (nil? (redirect/target "https://a.example/x" "http://a.example/y")) "never from https to http")
  (is (nil? (redirect/target "https://a.example/x" "ftp://a.example/y")) "only to http or https")
  (is (nil? (redirect/target "https://a.example/x" nil))))

(deftest where-redirects-moved-a-request-for-good
  (is (= "https://c/" (redirect/permanent-url [[301 "https://a/"] [308 "https://b/"]] "https://c/")))
  (is (= "https://b/" (redirect/permanent-url [[301 "https://a/"] [302 "https://b/"]] "https://c/"))
      "the permanent ones at the start")
  (is (nil? (redirect/permanent-url [[302 "https://a/"] [301 "https://b/"]] "https://c/"))))
