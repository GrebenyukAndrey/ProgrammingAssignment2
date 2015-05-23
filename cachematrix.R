## This tiny module gives you a matrix, that can cache it's solve 
## — so it won't be caclulated every time you need it
## 
## This "cached" matrix is wrapped into special object — you can
## construct an instance with "makeCacheMatrix" function
##
## Use methods get/set to access matrix inside the object
##
## Use method cacheSolve to obtain matrix solve. Calcutation is cached
##
## Here is complete example:
##    cached <- makeCacheMatrix(yourMatrix) ## Create wrapped matrix
##    tmp <- cached.get() ## Read matrix
##    cached.set(anotherMatrix) ## Change matrix
##    s <- cacheSolve(cached) ## Obtain matrix solve with caching
##


## Create cached matrix
##
## Input: your matrix
## Output: cached matrix
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



## Obtain matrix solve
##
## Input: cached matrix. You can create it with "makeCachedMatrix"
## Output: matrix solve
##
## Metod uses cache. So solve won't be caclulated on every call
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



## Test section
## 
## Message "getting cached solve" should appear on second call
##
##tst <- makeCacheMatrix( matrix( sample(1:10, 25, replace=TRUE), 5, 5) )
##cacheSolve(tst)
##cacheSolve(tst)
