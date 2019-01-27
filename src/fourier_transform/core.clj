(ns fourier-transform.core
  (:gen-class))
(defn dft [v]
  (let [n (count v)]
    (for [i (range n)]
      (Math/hypot
       (apply + (map * v (map (comp #(Math/cos %) (partial * 2 i Math/PI (/ n))) (range n))))
       (apply + (map * v (map (comp #(Math/sin %) (partial * 2 i Math/PI (/ n))) (range n))))))))
(defn -main
  [& args]
  (println "Hello, World!"))
