(ns tunis.logger.redaction
  (:require
   [clojure.walk :as walk]))

(defn redact
  [m {:keys [redact-key? redact-value-f]}]
  (walk/postwalk (fn [v]
                   (if (map? v)
                     (into {}
                           (map (fn [[k v']]
                                  (if (redact-key? k)
                                    [k (redact-value-f v')]
                                    [k v'])))
                           v)
                     v))
                 m))
