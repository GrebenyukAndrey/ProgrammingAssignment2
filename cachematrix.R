## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    get <- function() x
    
    setSolve <- function(solved) s <<- solved
    
    getSolve <- function() s
    
    list(set = set,
         get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getSolve()
    if( !is.null(s) ) {
        message("getting cached solve")
        return(s)
    }
    
    tmp <- x$get()
    s <- solve(tmp, ...)
    x$setSolve(s)
    s
}


tst <- makeCacheMatrix( matrix( sample(1:10, 25, replace=TRUE), 5, 5) )
cacheSolve(tst)
cacheSolve(tst)
cacheSolve(tst)
