(rail-ref r n)

(rail-ref-set! r n obj)

(rail-tail r n)

(rail-tail-set! r n r2)

(rail . objs)

(make-rail n [fill])

(rail-tabulate n proc)

(rail-copy r)

(rail->list r)

(list->rail list)

(rail->list! r)

(rail? obj)

(empty-rail? r)

(rail= r1 r2)

(rail-take r n)

(rail-drop r n)

(rail-take-right r n)

(rail-drop-right r n)

(rail-length r)

(rail-append . rs)

(rail-reverse r)

(rail-fold kons knil . rs)

(rail-fold-right kons knil . rs)

(rail-tail-fold kons knil . rs)

(rail-tail-fold-right kons knil . rs)

rail-unfold, rail-reduce, rail-reduce-right


Filtering & Partitioning


rail-map(!), rail-for-each, rail-append-map, rail-tail-for-each
