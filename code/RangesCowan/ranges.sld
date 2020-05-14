(define-library (ranges)
  (import (scheme base)
          (scheme case-lambda)
          (srfi 1)
          (srfi 128)
          (srfi 145))
  (export range numeric-range
          range? range-contains? range-includes? range-empty?
          range-element-comparator range-length range-indexer range-ref
          range-start range-end
          range-split-at range-take range-take-right range-drop range-drop-right
          range-count range-map->list range-for-each range-fold range-fold-right
          range-any range-every
          range-filter->list range-remove->list range-reverse
          range-index range-index-right range-take-while range-drop-while
          range-take-while-right range-drop-while-right
          range->list range->generator)

  (include "ranges.scm"))
