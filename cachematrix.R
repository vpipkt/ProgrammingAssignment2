## Defines a structure for storing a  matrix and (if applicable) caching
## its inverse.


## makeCacheMatrix defines the cachematrix structure. 
## Call getters and setters to access matrix or its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # The matrix itself is stored as x
    # The inverse is cached in inverseCache
    
    inverseCache <- NULL
    
    #defines the matrix, re-initialize the inverse to NULL
    set <- function(y) {
        x <<- y
        inverseCache <<- NULL
    }
    
    # return the matrix
    get <- function() x
    
    # set the inverse
    setInverse <- function(inv) inverseCache <<- inv
    
    # return the cached inverse
    getInverse <- function() inverseCache
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute or return the inverse of the CacheMatrix structure.
##  Checks the cached value of the matrix inverse first. If the value is null
##  computes the value and stores it in the cacheMatrix structure.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("Getting cached inverse.")
        return(inv)
    }
    #matrix inverse was not cached, cache and return it
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    return(inv)
        
}
