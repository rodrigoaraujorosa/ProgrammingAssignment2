## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix do:
##   a) Set the value of the matrix
##   b) Get the value of the matrix
##   c) Set the value of the solve
##   d) Get the value of the solve
##
## cacheSolve do:
##   a) Checks to see if the solve has already been calculated
##   b) If so, it get S the solve from the cache and skips the computation
##   c) Otherwise, it calculates the solve of the data and sets the value
##      of the solve in the cache via the setsolve function

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)){
        message("Getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
