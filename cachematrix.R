## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. The functions below allow you to create an object that looks
## like a matrix but caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(mat = matrix()) {
    set <- function(x) mat <<- x; sol <<- NULL
    get <- function() mat
    set.inverse <- function(inverse) sol <<- inverse
    get.inverse <- function() sol
    list(set = set,
         get = get,
         set.inverse = set.inverse,
         get.inverse = get.inverse)
}

## This function computes the inverse of the special "matrix" returned by
## `makeCacheMatrix` above. If the inverse has already been calculated (and the
## matrix has not changed), then `cacheSolve` should retrieve the inverse from
## the cache.
cacheSolve <- function(x, ...) {
    sol <- x$get.inverse()
    if(!is.null(sol)) message('getting cached data')
    else sol <- solve(x$get()); x$set.inverse(sol)
    sol
}