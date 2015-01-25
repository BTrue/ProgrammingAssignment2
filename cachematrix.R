## Put comments here that give an overall description of what your
## functions do

## Insert Comments Here when you unsderstand the function
makeVector <- function(x = numeric()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setmean <- function(mean) m <<- mean
     getmean <- function() m
     list(set = set, get = get,
          setmean = setmean,
          getmean = getmean)
}

## Insert Comments ...
cachemean <- function(x, ...) {
     m <- x$getmean()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- mean(data, ...)
     x$setmean(m)
     m
}

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setinv <- function(mean) i <<- inv
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
     i <- inv(data, ...)
     x$setinv(i)
     i
     
}
