## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #Store the cached inverse
  set <- function(y) {
    x <<- y #Update matrix
    inv <<- NULL #Reset cached inverse
  }
  get <- function() x #Get matrix
  setInverse <- function(inverse) inv <<- inverse #Cache the inverse
  getInverse <- function() inv #Get cached inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if(!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
