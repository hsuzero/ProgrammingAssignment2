## This function is able to cache potentially time-consuming computations.
## 

## make a cache of input matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
         inv <<- NULL
     }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## calculate or return cache

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)){
        message("getting cache data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
