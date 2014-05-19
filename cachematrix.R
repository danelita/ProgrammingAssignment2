## Assignment 2 - Caching the Inverse of a Matrix

## Initialize main functions; getinverse, setinverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns inverse matrix of 'x'
cacheSolve <- function(x, ...) {
    ## Get location of matrix inverse
    m <- x$getinverse()
    ## If there's valid value, then we have it in cache
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ##If not found in cache, then apply solve and 
    ##save value in cache for future use
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

