## makeCacheMatrix constructs a cache matrix object
## the object is a wrapper around a matrix with support for caching
## some value associated with the matrix
##
## cacheSolve returns the inverse of a cache matrix object
## it caches the inverse, using it in subsequent calls as long as the
## cache matrix is not changed

## This function constructs a cache matrix object
makeCacheMatrix <- function(x = matrix()) {
	
	# cache object
	cache <- NULL
	
	# update the matrix
	# clear the cache object
	set <- function(y) {
		x <<- y
		cache <<- NULL
	}
	
	# return the matrix
	get <- function() {
		x
	}
	
	# update the cache object
	set_cache <- function(c) {
		cache <<- c
	}
	
	# return the cache object
	get_cache <- function() {
		cache
	}
	
	# construct the cache matrix object
	list(set = set, get = get, set_cache = set_cache, get_cache = get_cache)
}


## Returns a matrix that is the inverse of the provided cache matrix object
cacheSolve <- function(x, ...) {
    # check if we have the inverse matrix cached
	inverse <- x$get_cache()
	
	# use cached inverse matrix if it exists
	if (!is.null(inverse)) {
		message("using cached data")
		return(inverse)
	}
	
	# otherwise compute the inverse matrix
	inverse <- solve(x$get())
	
	# update the cache with the computed value
	x$set_cache(inverse)
	
	# return the inverse
	inverse
}