## Put comments here that give an overall description of what your
## functions do

## this function will create a 'matrix' object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  matrixinverse <- NULL
  set <- function(y) {
    x <<- y
    matrixinverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) matrixinverse <<- solve
  getinverse <- function() matrixinverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## function computes the inverse of the special "matrix" returned by makeCacheMatrix
## if nothing has changed or no mean, it retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## using solve() function
  matrixinverse <- x$getinverse()
  if(!is.null(matrixinverse)) {
    message("getting cached data")
    return(matrixinverse)
  }
  data <- x$get()
  matrixinverse <- solve(data, ...)
  x$setinverse(matrixinverse)
  matrixinverse
}
