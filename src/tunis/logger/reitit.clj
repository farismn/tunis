(ns tunis.logger.reitit
  (:require
   [tunis.logger :as tns.log]))

(defn log-request-params-interceptor
  ([]
   (log-request-params-interceptor nil))
  ([options]
   {:name    ::request-params
    :compile (fn [{:keys [tunis]} _]
               (when-let [opts (or tunis options)]
                 (tns.log/log-request-params-interceptor opts)))}))

(defn log-request-start-interceptor
  ([]
   (log-request-start-interceptor nil))
  ([options]
   {:name    ::request-start
    :compile (fn [{:keys [tunis]} _]
               (when-let [opts (or tunis options)]
                 (tns.log/log-request-start-interceptor opts)))}))

(defn log-response-interceptor
  ([]
   (log-response-interceptor nil))
  ([options]
   {:name    ::response
    :compile (fn [{:keys [tunis]} _]
               (when-let [opts (or tunis options)]
                 (tns.log/log-response-interceptor opts)))}))
