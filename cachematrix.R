## The pair of functions that cache the inverse of a matrix.
## Usage:
## > x <- matrix(1:4, 2, 2)
## > y <- makeCacheMatrix(x)
## > result <- cacheSolve(y)

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        cachedInverse <- NULL 
        # Setting the matrix
        set <- function(y) {
                x <<- y
                cachedInverse <<- NULL
        }
        # Getting the original matrix
        get <- function() x
        # Setting the cached inversed matrix
        setInversed <- function(inversed) cachedInverse <<- inversed
        # Getting the cached inversed matrix
        getInversed <- function() cachedInverse
        # Returning a list of functions
        list(set = set, get = get,
             setInversed = setInversed,
             getInversed = getInversed)
}


## cacheSolve computes the inverse of the matrix or returns a cached value

cacheSolve <- function(x, ...) {
        # Trying to get the cached value
        inversed <- x$getInversed()
        if(!is.null(inversed)) {
                message("getting cached data")
                return(inversed)
        }
        # Calculating a new inversed matrix
        inversed <- solve(x$get(), ...)
        # Caching the data
        x$setInversed(inversed)
        inversed
}
