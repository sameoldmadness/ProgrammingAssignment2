## Put comments here that give an overall description of what your
## functions do

## Creates container for matrix and its inverse
## with respective getters and setters.
## Inverse gets cleared when new matrix is set.

makeCacheMatrix <- function(matrix = matrix()) {
  inverse <- NULL
  set <- function(newMatrix) {
    matrix <<- newMatrix
    inverse <<- NULL
  }
  get <- function() matrix
  setinverse <- function(newInverse) inverse <<- newInverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Gets inverse from cacheMatrix if exists
## or evaluates and saves it otherwise.
## @see makeCacheMatrix

cacheSolve <- function(cacheMatrix, ...) {
  inverse <- cacheMatrix$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  matrix <- cacheMatrix$get()
  inverse <- solve(matrix, ...)
  cacheMatrix$setinverse(inverse)
  inverse
}
