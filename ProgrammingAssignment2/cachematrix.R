## Written by Brian True for Coursera class R Programming 01.24.15

## Function(s): 
     # makeCacheMatrix(x = matrix())
          # Argument(s): 'x' an (invertable) matrix
     # cacheSolve(x, ...)
          # argument(s): 'x' an (invertable) matrix, '...' additional optional args

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setinv <- function(inv) i <<- inv
     getinv <- function() i
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)

}

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) { 
     i <- x$getinv()
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     ## Return 'i' a matrix that is the inverse of 'x'
     data <- x$get()
     i <- solve(data, ...)
     x$setinv(i)
     i
     
}
