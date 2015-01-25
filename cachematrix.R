## Written by Brian True for Coursera class R Programming 01.24.15

## Function(s): 
     # makeCacheMatrix
          # Argument(s): x an (invertable) matrix
     # cacheSolve
          # argument(s): x an in
## Write a short comment describing this function
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

## Write a short comment describing this function
cacheSolve <- function(x, ...) { 
     i <- x$getinv()
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     ## Return a matrix that is the inverse of 'x'
     data <- x$get()
     i <- solve(data, ...)
     x$setinv(i)
     i
     
}
