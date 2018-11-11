## The functions below are used in tandem to either calculate the inverse of a matrix and store
## the calculated inverse in cache, or if the inverse is already stored in cache then just retrieve
## the inverse.

## The function below, makeCacheMatrix, creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function () x
  setInverse <- function(Inverse) Inv <<- Inverse
  getInverse <- function () Inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The function below, cacheSolve, computes the inverse of the matrix returned by makeCacheMatrix.  
## If the inverse has already been calculated, then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  Inv <- x$getInverse()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data)
  x$setInverse(Inv)
  Inv
}
