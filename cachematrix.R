###############################################################################
## 
##  Script-File: Functions to cache the inverse of a matrix
##  Holzi1
##  2015-07-22
##
###############################################################################
##
##  This script file includes two functions: makeCacheMatrix and cacheSolve
##  
##  makeCacheMatrix:
##  The input of the main function makeCacheMatrix is a square invertible matrix 
##  X and the output is a list of 4 "sub"-functions which can be called: set to 
##  change the input-matrix X, get returns the input-matrix X, setinv stores the
##  inverse matrix I in the main function and getinv returns the inverse Matrix 
##  I.
##
###############################################################################

makeCacheMatrix <- function(X = matrix()) {
    I <- NULL
    set <- function(Y) {
        X <<- Y
        I <<- NULL
    }
    get <- function() {
        return(X)
    }
    setinv <- function(inv) {
        I <<- inv
    }
    getinv <- function() {
        return(I)
    }
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

###############################################################################
## 
##  cacheSolve:
##  The input of this function is the object where the previous function 
##  makeCacheMatrix is stored and the output is the inverse Matrix of X. The 
##  function calls the value I, before stored with the function makeCacheMatrix,
##  with the "sub"-function getinv and tests if the value exists and is not 
##  null. If it exists it returns the inverse matrix I of X. Otherwise the 
##  function calculates the inverse Matrix I, stores it in object assigned to
##  the function makeCacheMatrix and returns the inverse Matrix I.
##
###############################################################################

cacheSolve <- function(x, ...) {
    I <- x$getinv()
    if(!is.null(I)) {
        message("getting cached inverse matrix")
        return(I)
    }
    else {
        data <- x$get()
        I <- solve(data, ...)
        x$setinv(I)
        return(I)
    }
}