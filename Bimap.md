A bimap is a pair of [SRFI 225](https://srfi.schemers.org/srfi-225/srfi-225.html),
one mapping keys to unique values,
the other mapping the values back to their keys.

## Issues

## Constructor

`(make-bimap `*forward-dto forward-dict reverse-dto reverse-dict`)

Returns a bimap based on two empty dictionaries and their associated
dictionary type objects (DTOs).'

## Predicate

`(bimap? `*obj*`)`

Return `#t` if *obj* is a bimap and `#f` otherwise.

## Initializers

`(bimap-set-from-alist `*bimap alist*`)`

The associations of *alist* are added to the bimap.
If a key to be added already exists, the new value prevails.

`(bimap-adjoin-from-alist `*bimap alist*`)`

The associations of *alist* are added to the bimap.
If a key to be added already exists, the old value prevails.

## Dictionary procedures

These procedures are performed on the forward dictionary.

`(bimap-empty?` *bimap*`)`  
`(bimap-contains?` *bimap key*`)`  
`(bimap=?` *= bimap1 bimap2*`)`  
`(bimap-pure?` *bimap*`)`  
`(bimap-ref` *bimap key [failure [success] ]*`)`  
`(bimap-ref/default` *bimap key default*`)`  
`(bimap-comparator` *bimap*`)`  
`(bimap-map` *proc bimap*`)`  
`(bimap-filter` *dpred bimap*`)`  
`(bimap-remove` *pred bimap*`)`  
`(bimap-size` *bimap*`)`  
`(bimap-count` *pred dict*`)`  
`(bimap-any` *pred bimap*`)`  
`(bimap-every` *pred bimap*`)`  
`(bimap-keys` *bimap*`)`  
`(bimap-values` *bimap*`)`  
`(bimap-entries` *bimap*`)`  
`(bimap-fold` *proc knil bimap*`)`  
`(bimap-map->list` *proc bimap*`)`  
`(bimap->alist` *bimap*`)`  
`(bimap-for-each` *proc bimap [ start [ end ] ] *`)`  
`(bimap->generator` *bimap [ start [ end ] ] *`)`  
`(bimap-set-accumulator` *bimap*`)`  
`(bimap-adjoin-accumulator` *bimap*`)`

These procedures are performed on the reverse dictionary.

`(bimap-reverse-contains?` *bimap key*`)`  
`(bimap-reverse-ref` *bimap key [failure [success] ]*`)`  
`(bimap-reverse-ref/default` *bimap key default*`)`  
`(bimap-reverse->alist` *bimap*`)`  

These procedures mutate both the forward and the reverse dictionaries.

`(bimap-set` *bimap obj …*`)`  
`(bimap-adjoin` *bimap obj ...*`)`  
`(bimap-delete` *bimap key …*`)`  
`(bimap-delete-all` *bimap keylist*`)`  
`(bimap-replace` *bimap key value*`)`  
`(bimap-intern` *bimap key failure*`)`  
`(bimap-update` *bimap key updater [failure [success] ]*`)`  
`(bimap-update/default` *bimap key updater default*`)`  
`(bimap-pop` *bimap*`)`  
`(bimap-find-update` *bimap key failure success*`)`  
