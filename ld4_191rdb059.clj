

(require '[clojure.string :as s])

(defn shifrs
  [kopigs, pozicija, zinasPozicija]
  (def kompens (- (* kopigs 2) 2))
  (def otrakompens (* (- (- kopigs 1) pozicija) 2))
  (def neparakompens (* pozicija 2))
  (cond (= zinasPozicija 0 ) pozicija
    (or (= pozicija 0) (= pozicija (- kopigs 1))) (+ (shifrs kopigs pozicija (- zinasPozicija 1)) kompens)
    (= (mod (+ zinasPozicija 1) 2) 0) (+ (shifrs kopigs pozicija (- zinasPozicija 1)) otrakompens)
    :else (+ (shifrs kopigs pozicija (- zinasPozicija 1)) neparakompens)))

(defn at [s i] (subs s i (+ i 1)))
(defn nomainit [zina] (s/replace zina #" " "_"))

(defn decrypt  [el txt]
  (def teksts (nomainit txt))
  (def elementi (kodetelementus el (count teksts)))
  (def e (range (count teksts)))
  (map (fn [f] (at teksts (second f))) (sort-by first (map vector elementi e))))

(defn kodetelementus [el garums]
  (filter  (fn [x] (< x garums)) (for [x (range el) y (range garums)] (shifrs el x y))))

(defn encrypt [el txt]
  (def teksts (nomainit txt))
  (map (fn [x] (at teksts x)) (kodetelementus el (count teksts))))

(def pirmais "we are decrypted")
(println (str "pirmais: " pirmais))
(def encrypted (apply str (encrypt 3 pirmais)))
(println (str "encrypt: " encrypted))
(println (str "decrypt: " (apply str (decrypt 3 encrypted))))
