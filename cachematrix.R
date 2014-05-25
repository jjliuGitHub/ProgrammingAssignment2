## Very similar to the example given in the instructions, the first function
## makeCacheMatrix creates a special "matrix", which is really a list containing
## a function to: set/get the value of the matrix; set/get the inverse of
## the matrix.
## The second function calculates the inverse of the matrix created in the 
## first function -- after it checks whether the inverse has been already
## calcuated. If yes, then it'll just return the cached value of the inverse.

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
