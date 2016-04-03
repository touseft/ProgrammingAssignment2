## Put comments here that give an overall description of what your
## functions do

## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly each time.


## This function creates a special "matrix" object that can cache its inverse
##
makeCacheMatrix <- function(x = matrix()) {


        ## Initialize to NULL to note that the invert needs to be computed
        invm <- NULL

        set <- function(y) {
                x <<- y
                ## Re-initialize to NULL to note that the invert needs to be re-computed
                invm <<- NULL
        }

        get <- function() x
        setinverse <- function(inverse) invm <<- inverse
        getinverse <- function() invm

        ## Return list of functions to set, get, setinverse, getinverse of a matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.
##
## Assume that the matrix is square and always invertable.
##
cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        invm <- x$getinverse()

	## if the inverse is in cache and it is not dirty, just get it from cache. 
        if(!is.null(invm)) {
                message("getting cached data")
                return(invm)
        }

	## cache is dirty, must calculate the inverse here and return it.
        data <- x$get()
        invm <- solve(data, ...)
        x$setinverse(invm)

        invm
}


