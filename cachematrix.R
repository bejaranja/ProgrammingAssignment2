makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(newinv) inv <<- newinv
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

cacheSolve <- function(x, ...) {
    myinv <- x$getinv()
    if(!is.null(myinv)) {
        message("getting cached matrix")
        return(myinv)
    }
    data <- x$get()
    myinv <- solve(data, ...)
    x$setinv(myinv)
    return(myinv)
}
