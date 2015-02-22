## These functions calculate the inverse of a matrix, and store that inverse to shorten future calculations

## This function creates a list of four functions that cache inverse matrices

makeCacheMatrix <- function(x = matrix()) {
    inver_mat <- NULL
    set <- function(y) {
        x <<- y
        inver_mat <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inver_mat <<- solve
    getinverse <- function() inver_mat
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following returns the inverse of a matrix, either pulling it from the cache, or calculating it itself

cacheSolve <- function(x, ...) {
    inver_mat <- x$getinverse()
    if(!is.null(inver_mat)) {
        message("getting cached data")
        return(inver_mat)
    }
    data <- x$get()
    inver_mat <- solve(data, ...)    ## Return a matrix that is the inverse of 'x'
    x$setinverse(inver_mat)
    inver_mat
}
