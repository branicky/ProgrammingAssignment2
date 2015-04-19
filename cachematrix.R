## Coursera: R Programming
## Programming Assignment 2: Lexical Scoping
## Caching the Inverse of a Matrix
## Juraj Branický 2015

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL ## i is cache for inverse
    
    ## set the value of the wrapped matrix
    set <- function(y) {
        x <<- y
        i <<- NULL ## matrix has changed so we have to unset cached inverse
    }
    
    ## get the value of the wrapped matrix
    get <- function() x
    
    ## set the value of the cache
    setInverse <- function(inverse) i <<- inverse
    
    ## get the value of the cache
    getInverse <- function() i
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache
cacheSolve <- function(x, ...) {
    i <- x$getInverse() ## firstly we look for cached inverse ...
    if (!is.null(i)) {  ## ... and if it exists, we returned it
        message("getting cachced data")
        return(i)
    }
    data <- x$get()     ## otherwise we have to calculate the inverse ...
    i <- solve(data, ...)
    x$setInverse(i)     ## ... we store computed value in the cache ...
    i                   ## ... and we return it
}
