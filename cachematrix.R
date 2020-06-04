## The first function 'makeCacheMatrix' creates a special matrix which can cache
## it's inverse. While the 'cacheSolve' function computes the inverse 
## of the special matrix created by the 'makeCacheMatrix' function or it 
## retrieve the cached data if it's available.


## The 'makeCacheMatrix' funtion creates a list of four functions.
## The 'get' function is used to get the required matrix into the 'cacheSolve' function.
## The 'getinv' function gets the cached inverse of the matrix.
## 'set' function allows to the set or change the value of the matrix.
## 'setinv' funtion allows to cache the inverse of a given matrix.

makeCacheMatrix <- function(x = matrix()){
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv 
    list(set = set, get = get, getinv = getinv, setinv = setinv)
}


## 'CacheSolve' is the funtion which returns the inverse of the matrix.
##  It either computes the inverse using the 'solve' funtion or retrieve the
##  data if it's already computed.

cacheSolve <- function(x,...) {
    inv <- x$getinv() 
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinv(inv)
    inv
    ## The function return a matrix that is the inverse of 'x'
}
