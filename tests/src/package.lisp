(defpackage kilns-tests
  (:use #:cl #:kell-calculus #:kilns #:fiveam)
  (:shadowing-import-from #:kell-calculus #:substitute #:match)
  (:shadowing-import-from #:kilns #:load))

(in-package kilns-tests)

(def-suite kilns
    :description "The suite containing all of the sub-suites.")
