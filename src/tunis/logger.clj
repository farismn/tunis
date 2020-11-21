(ns tunis.logger
  (:require
   [tunis.logger.redaction :refer [redact]]))

;; Essentially the interceptor form of
;; https://github.com/nberger/ring-logger

(def default-request-keys
  [:request-method :uri :server-name])

(def default-redact-key?
  #{:password
    :hashed-password
    :secret
    :token
    :secret-token
    :authorization
    :authentication
    :auth})

(defn- make-log-fn
  [log-fn transform-fn]
  (fn [message]
    (when-let [transformed (transform-fn message)]
      (log-fn transformed))))

(defn log-request-params-interceptor
  ([]
   (log-request-params-interceptor {}))
  ([{:keys [log-fn
            transform-fn
            log-level
            request-keys
            redact-key?
            redact-value-f]
     :or   {log-fn         println
            transform-fn   identity
            log-level      :debug
            request-keys   default-request-keys
            redact-key?    default-redact-key?
            redact-value-f (constantly "[REDACTED]")}}]
   {:name  ::request-params
    :enter (fn [{:keys [request] :as ctx}]
             (let [log    (make-log-fn log-fn transform-fn)
                   params (redact (:params request)
                                  {:redact-key?    redact-key?
                                   :redact-value-f redact-value-f})]
               (log {:level   log-level
                     :message (-> request
                                  (select-keys request-keys)
                                  (assoc ::type ::request-params
                                         :params params))})
               ctx))}))

(defn log-request-start-interceptor
  [{:keys [log-fn transform-fn log-level request-keys]
    :or   {log-fn       println
           transform-fn identity
           log-level    :info
           request-keys default-request-keys}}]
  {:name  ::request-start
   :enter (fn [{:keys [request] :as ctx}]
            (let [log      (make-log-fn log-fn transform-fn)
                  start-ms (System/currentTimeMillis)]
              (log {:level   log-level
                    :message (-> request
                                 (select-keys request-keys)
                                 (assoc ::type ::request-start))})
              (assoc ctx ::start-ms start-ms)))})

(defn log-response-interceptor
  [{:keys [log-fn transform-fn request-keys log-exception?]
    :or   {log-fn         println
           transform-fn   identity
           request-keys   default-request-keys
           log-exception? true}}]
  {:name  ::response
   :enter (fn [{::keys [start-ms] :as ctx}]
            (cond-> ctx
              (nil? start-ms)
              (assoc ::start-ms (System/currentTimeMillis))))
   :leave (fn [{:keys  [request response]
               ::keys [start-ms]
               :as    ctx}]
            (let [log        (make-log-fn log-fn transform-fn)
                  elapsed-ms (- (System/currentTimeMillis) start-ms)
                  status     (:status response)
                  log-level  (if (and (number? status) (<= 500 status))
                               :error
                               :info)]
              (log {:level   log-level
                    :message (-> request
                                 (select-keys request-keys)
                                 (assoc ::type ::response
                                        :status status
                                        :elapsed elapsed-ms))})
              ctx))
   :error (fn [{:keys  [error request]
               ::keys [start-ms]
               :as    ctx}]
            (if log-exception?
              (let [elapsed-ms (- (System/currentTimeMillis) start-ms)]
                (log-fn {:level     :error
                         :throwable error
                         :message   (-> request
                                        (select-keys request-keys)
                                        (assoc ::type ::response
                                               :elapsed elapsed-ms))})
                ctx)
              ctx))})

(comment

  (require '[sieppari.core :as siep])

  (let [handler (fn [{:keys [params] :as req}]
                  (if (:throw? params)
                    (throw (ex-info "foobar" {}))
                    (assoc req :status 200)))]
    (siep/execute
      [(log-request-start-interceptor {})
       (log-request-params-interceptor {})
       (log-response-interceptor {})
       handler]
      {:request-method :get
       :uri            "/"
       :params         {:foo      "bar"
                        :password "bazbarquux"
                        :throw?   false}}))

  )
