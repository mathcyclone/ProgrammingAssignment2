## These functions construct and interact with an object that caches the inverse of
## a matrix to save time during computation.

## The makeCacheMatrix() function stores its invertible matrix argument as a list of
## four functions to set and return the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {x}
    setinv <- function(inv_val) {inv <<- inv_val}
    getinv <- function() {inv}
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## The cacheSolve() function returns the inverse of a matrix that is stored using the
## makeCacheMatrix() function. By checking if the value of the inverse has been cached
## before calling the solve() function, the cacheSolve() function saves time.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
