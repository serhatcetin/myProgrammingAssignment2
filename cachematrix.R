## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
## Below a pair of functions that stores a  matrix and caches the inverse of this matrix


## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function computes the inverse of the special "matrix" created by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)){
  message("return cached data")
    return(inv)
  }
  ser <- x$get()
  inv <- solve(ser, ...)
  x$setInverse(inv)
  inv
}
