## These functions are used to create an environment in which to
## cache a square matrix and its inverse.  They contain the functions
## necessary to retrieve and write the matrix as well as those to
## calculate the inverse and return it. Note: There is no error
## checking to ensure that the matrix is invertible!

## Example:
    ## Create an invertible square matrix
    ##
    ## x <- matrix(runif(3*3),3,3) ## make a random square matrix
    ## x <- t(x)*x ## make x positive definite (and thus invertible)
    ## cache <- makeCacheMatrix(x)
    ## inverse <- cacheSolve(cache)

## The function makeCacheMatrix takes a matrix as an input argument
## which is then placed in a cache. The return value is the lsit of
## functions for this matrix:
## set(y) will cache the matrix y
## get() will return the cached matrix
## setinv(inv) will cache the inverse of the matrix
## getinv() will return the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL ## New cache, so initialize storage for inverse
    ## set(y) will allow us to reset the matrix in the cache to y, but
    ## this means we likely have a different inverse, so reset to null
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    ## get() will simply return the cached matrix
    get <- function(){
        x  
    }
    ## setinv(inv) will allow us to set the cached inverse matrix
    setinv <- function(inv) {
        inverse <<- inv
    }
    ## getinv() will return the cached inverse matrix
    getinv <- function() inverse
    
    ## return value for makeCacheMatrix is a list containing the
    ## functions which can access the cache
    list(set = set, get = get,setinv = setinv,getinv = getinv)
}


## cacheSolve is used to find the inverse of the cached matrix
## as produced by makeCacheMatrix. Additionally it will cache the 
## inverse and return it as the output for cacheSolve(x).

cacheSolve <- function(x) {
    ## Retrieve the inverse matrix from the cache
    inverse <- x$getinv()
    
    ## If we get a non-null matrix then we are fine and return it
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    ## If x$getinv() returned a null value we need to set the inverse
    data <- x$get() ## First retrieve the matrix from the cache
    inverse <- solve(data) ## Calculate its inverse
    x$setinv(inverse) ## Set the inverse in the cache
    inverse ## return the inverse from the function cacheSolve(x)
}
