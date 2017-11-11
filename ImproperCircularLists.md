Here are the valid uses of improper lists in R7RS-small in contexts where lists (as opposed to arbitrary objects) are expected:

* As a `lambda`-list
* As the last argument of `append`
* As the argument to `list-copy`, which then returns an improper list that is `equal?` to the argument
>
And of circular lists:

* As the list argument to `list-ref`
* As the list arguments to `map` and `for-each` (though it is an error if all arguments are circular)
