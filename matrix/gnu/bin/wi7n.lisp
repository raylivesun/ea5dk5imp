#|
wi7n:
- Author: admin
- Date: 2024-09-22
|#

(define (my-if test true-action false-action)
(if test true-action false-action))

(my-if (> 3 2) (println "yes it is" ) (exit))