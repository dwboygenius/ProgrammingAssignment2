## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    setI <- function(inv) i <<- (inv)
    getI <- function() i
    list(set = set, get = get,
         setI = setI,
         getI = getI)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    i <- x$getI()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setI(i)
    i
    
    ## Return a matrix that is the inverse of 'x'
}
