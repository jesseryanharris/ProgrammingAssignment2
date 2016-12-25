## Two functions: makeCacheMatrix and cacheSolve
## Together they calculate and cache the inverse of a matrix.

## Function 1: makeCacheMatrix
## Accepts as input an invertible matrix. Stores the matrix and
## outputs a list of functions that can be called by the cacheSolve
## function.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Function 2: cacheSolve
## Calculates the inverse of a matrix and caches the result.
## If inverse has already been calculated, it just returns the
## cached data.
##
## Example:
##   my_matrix <- matrix(rexp(9), nrow = 3, ncol = 3)
##   m <- makeCacheMatrix(my_matrix)
##   cacheSolve(m)

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
