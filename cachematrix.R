## Put comments here that give an overall description of what your
## functions do

## The following functions enable reuse of the computed matrix inverse -
## instead of re-computing the inverse which could be inefficient


## Write a short comment describing this function

## The makeCacheMatrix function takes a matrix as input argument
## and returns a list of functions of get, set, getinverse, setinverse
## The returned list of functions get and set the two values - input matrix 
## and its inverse
 
makeCacheMatrix <- function(x = matrix()) {
    ## x is input matrix
    ## return a list of functions - set, get, setinverse, getinverse
  
    cachedInv <- NULL

    ## set - caches the pair of input matrix and its inverse
    set <- function(y) {
        x <<- y
        cachedInv <<- NULL
    }

    ## get function returns the cached matrix
    get <- function() x

    ## cache the inverse of the matrix
    setinverse <- function(newCache) cachedInv <<- newCache

    ## getinverse returns the cached inverse of the matrix - it could be null
    getinverse <- function() cachedInv

    ## return list of functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

## The cacheSolve takes the argument as the list of functions defined in the makeCacheMatrix.
## It gets the cached inverse. If the inverse has not been computed previously
## then it computes it and caches the input matrix and its inverse to be reused.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    cachedInv <- x$getinverse()

    ## if the cached value is not null then simply return the same
    if(!is.null(cachedInv)) {
        message("Getting cached data")
        return(cachedInv)
    }

    ## if cached value is null then compute the inverse
    ## first get the matrix
    data <- x$get()

    ## Assume the matrix has a valid inverse 
    ## get the inverse of the matrix
    cachedInv <- solve(data, ...)

    ## cache the inverse of the matrix
    x$setinverse(cachedInv)
    return (cachedInv)
}
