############################################
## This R script file contains two functions: makeCacheMatrix and cacheSolve
## 
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
##
## cacheSolve: This function returns the inverse of the special "matrix". 
## If the inverse has been calculated and cached, return the cached matrix; 
## otherwise, use solve to get the inverse. 
############################################

## Description of makeCacheMatrix
## inv: an enviromental variable which stores the inverse matrix 
## set: function for resetting the matrix x
## get: function for getting the matrix x
## setinv: put the inverse matrix into inv (cached)
## getinv: get the inverse matrix from inv, the memory
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(im) inv <<- im
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Description of cacheSolve
## First, check if the inverse matrix is in the environmental variable inv.
## If yes, return the cahced data; if not, use solve function to calculate 
## the inverse matrix and put it into inv
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
