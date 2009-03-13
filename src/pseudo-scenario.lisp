(defworld my-table (table))

(defsim alice (person my-table)
  :name "Alice")


(defsim bob (person my-table)
  :name "Bob")


(defsim malcom (person my-table)
  :name "Bob")


(defsims 3 (chair my-table))


(defaction alice 0
  (let ((ch (random-pick (chairs-of my-table))))
    (sit-on ch)))


(defreaction alice (TODO)
  (greet bob))


(defaction bob 10
  (let ((ch (random-pick (chairs-of my-table))))
    (sit-on ch)))
   