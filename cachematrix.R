## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function return a list of four functions

makeCacheMatrix <- function(x = matrix()) {
    value <- NULL
    
    set <- function(m) {
        x <<- m
        value <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(solve) value <<- solve
    
    getinverse <- function() value
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()

    # If the inverse is already calculated, return it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    # The inverse is not yet calculated, so we calculate it
    data <- x$get()
    inv <- solve(data, ...)

    # Cache the inverse
    x$setinv(inv)

    # Return it
    inv
}
