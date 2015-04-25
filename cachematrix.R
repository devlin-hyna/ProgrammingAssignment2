## cachematrix.R
##
## This file contains two functions
##     makeCacheMatrix:   creates a matrix that can also store its inverse.
##     cacheSolve:        returns the inverse of a cacheMatrix.

## makeCacheMatrix takes a matrix as its parameter and then creates
##     a version of the matrix that can also store the inverse of
##     the matrix.

makeCacheMatrix <- function(x = matrix()) {
    m.inv <- NULL
    
    set <- function(y) {
        x <<- y
        m.inv <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setinv <- function(inv) {
        m.inv <<- inv
    }
    
    getinv <- function() {
        m.inv
    }
    
    list(set = set, 
         get = get, 
         setinv = setinv, 
         getinv = getinv)
}


## cacheSolve checks for an existing inverse of a matrix
##    if there is a cached inverse, return it
##    otherwise invert, set for future use, and return

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m.inv <- x$getinv()
    if(!is.null(m.inv)) {
        message("getting cached inverse.")
        return(m.inv)
    }
    m.data <- x$get()
    m.inv <- solve(m.data)
    x$setinv(m.inv)
    m.inv
}
