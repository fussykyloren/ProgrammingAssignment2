## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix that caches its own inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <- -NULL
  }
  
  get <- function(){ x }
  
  setInverse <- function(inverseToSet){ inverse <<- inverseToSet }
  getInverse <- function(){ inverse }
  list( set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## This function computes the the inverse of the matrix created by
## the makeCacheMatrix function. It should, if the matrix is already
## created, retrive the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  
  if(!is.null(inverse)) {
    message("Retreiving cached data")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setInverse(inverse)
  inverse
}
