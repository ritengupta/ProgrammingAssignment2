## Functions to create a matrix with cached inverse

# Creates the matrix "object", returns 4 functions to set data, get data,
# set the inverse, and get the inverse. Stores inverse in variable 'inv'.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
    
}


# Takes a matrix "object". Computes inverse if the 'inv' member is NULL
# If inv is not null, returns inv.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i

}
