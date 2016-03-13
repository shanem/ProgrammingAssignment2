## Utilies for caching matrix inverses

## Argument: a matrix
## Returns: a "cacheMatrix"
##      cacheMatrix:
##          A wrapper for a matrix and its cached inverse
##          Auto-invalidates cache when matrix is set

makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    set <- function(y) {
        x <<- y
        cachedInverse <<- NULL
    }
    get <- function() x
    setCachedInverse <- function(inverse) cachedInverse <<- inverse
    getCachedInverse <- function() cachedInverse
    list(set = set, get = get,
         setCachedInverse = setCachedInverse,
         getCachedInverse = getCachedInverse)
}


## Takes a cacheMatrix, calls "solve" (matrix inverse)
## Uses and updates the input cacheMatrix's cache

cacheSolve <- function(x, ...) {
    cachedInverse <- x$getCachedInverse()
    if(!is.null(cachedInverse)) {
        message("getting cached data")
        return(cachedInverse)
    }
    rawMatrix <- x$get()
    inverse <- solve(rawMatrix, ...)
    x$setCachedInverse(inverse)
    inverse
}
