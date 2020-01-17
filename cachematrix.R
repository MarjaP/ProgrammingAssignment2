## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function:
## makeCacheMatrix function creates a special "matrix" object. It can cache its inverse.

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## Write a short comment describing this function:
## cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated & the matrix has not changed,
## the cachesolve function retrieves the inverse of 'X' from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
## if the inverse has already been calculated
## get it from the cache and no computation,
## otherwise, calculates the inverse.
        inv = x$getinv()
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat.data = x$get()
        inv = solve(mat.data, ...)
## sets the value of the inverse in the cache - setinv.
        x$setinv(inv)
        return(inv)
        }
