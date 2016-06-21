## Set of two functions that allow a matrix to be computed and cached, to prevent 
## recalculation if the inverse is needed in the future


## Creates an object that holds get/set functions that allow a matrix to have
## its inverse cached, the functions are:
##  get -> "gets" the matrix
##  set -> "resets" the matrix to the matrix passed in as a parameter
##  setSolve -> creates the inverse matrix
##  getSolve -> returns the inverse matrix
## return of function is a list of these 4 functions

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## This function returns either the cached inverse matrix if it has been computed
## otherwise computes and return the inverse matrix
## parameter is an object of type makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getSolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}
