## These functions cache the inverse of a square, invertible matrix so that it
## may be easily recalled for future calculations. This saves time and computing
## power, as the inverse need only be computed once.

## The 'makeCacheMatrix' function creates a special "matrix" object that will be
## used to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## The 'cacheSolve' function calculates the inverse of the "matrix" object from
## the 'makeCacheMatrix' function, retrieving the inverse from the cache if it
## has already been computed.

cacheSolve <- function(x, ...) {
      m <- x$getsolve()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}
