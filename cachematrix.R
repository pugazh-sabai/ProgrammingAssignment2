## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates an object containing a
## matrix and its inverse. It initializes the
## the inverse to NULL. It also has get, set functions to work
## with the matrix and it's inverse

## This function returns a list of the following four functions
## set()        : Sets the matrix to user given values and
##                initialize the inverse to NULL
## setinv()     : Sets the inverse to the user given value
## get()        : Return the matrix
## getinv()     : Return the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function (y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function () x
        getinv <- function () inv
        setinv <- function (inverse) inv <<- inverse
        list (set = set, get = get, 
              getinv = getinv, setinv = setinv)
        
}

## cacheSolve function returns the inverse of a matrix
## created using makeCacheMatrix function

## This function returns the inverse from cache if it
## was already done and stored in the inv variable. If
## the inverse was not computed already, this function
## computes it using the solve command

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)) {
                message ('Getting cached data')
                return (inv)
        }
        inv <- solve(x$get(), ...)
        x$setinv (inv)
        inv
}
