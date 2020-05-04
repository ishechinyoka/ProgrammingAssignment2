## Put comments here that give an overall description of what your
## functions do

## Based on the template provided on the Instructions page, this file contains a pair of functions to compute the cache of an inverse matrix
## Write a short comment describing this function

## 1: Create a matrix object that caches its inverse:
makeCacheMatrix <- function(x = matrix()) {
invMatrix <- NULL
    set <- function(x) {
        spcMatrix <<- x;
        invMatrix <<- NULL;
    }
    get <- function() return(spcMatrix);
    setinv <- function(inv) invMatrix <<- inv;
    getinv <- function() return(invMatrix);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## Write a short comment describing this function
## 2: Then the following function calculates 
## the inverse returned by the function "cacheMatrix" in step 1
## (however, if the calculation had already been done, the function will simply get it from the cache):
cacheSolve <- function(x, ...) {
    invMatrix <- spcMatrix$getinv()
    if(!is.null(invMatrix)) return(invMatrix)
    data <- spcMatrix$get()
    invMatrix <- solve(data, ...)
    spcMatrix$setinv(invMatrix)
    ## Return a matrix that is the inverse of 'x'
    return(invMatrix)
}
