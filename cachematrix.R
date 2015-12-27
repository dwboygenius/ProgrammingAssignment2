## The following functions work to optimize the calculation of the inverse
## of a matrix. If the calculation has not yet been performed, it is done
## on the fly, the value is returned, and saved/cached for later. If a cached
## value exists it is returned instead of performing the calculation again.

## This function creates a list of functions that can be
## performed on the matrix that is passed
makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    ## set function saves the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ## get function returns the matrix
    get <- function() x
    ## setI function saves the inverse matrix
    setI <- function(inv) i <<- (inv)
    ## getI function returns the inverse matrix
    getI <- function() i
    ## return the list of functions
    list(set = set, get = get,
         setI = setI,
         getI = getI)
}

## This function calculates the inverse of a matrix and returns it.
## If it was already calculated and cached, the cached value is returned.
cacheSolve <- function(x, ...) {
    ## grab cached inverse from matrix
    i <- x$getI()
    ## if cached inverse exists, return with message
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## else calculate the inverse, save/cache, and return it
    data <- x$get()
    i <- solve(data, ...)
    x$setI(i)
    i
}
