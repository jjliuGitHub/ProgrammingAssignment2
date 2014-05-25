## Put comments here that give an overall description of what your
## functions do

## This function creats a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) i <<- solve
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix function above. If the inverse has already been calculated
## and the matrix has not changed, then the cacheSolve should just retrieve
## the inverse from the cache without calculating it again
## For this assignment we assume the matrix is always invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i


}
