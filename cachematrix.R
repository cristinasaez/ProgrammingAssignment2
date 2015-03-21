
## These functions are used to create a special object that stores a matrix and cache's its inverse.
## They will enable us to reduce the computational cost. 

## makeCacheMatrix creates the special "matrix"
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inverse <<- solve
  getsolve <- function() inverse
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve calculates the inverse of the special "matrix" created with the above function.
## If the inverse has already been calculated, it gets it.
## Otherwise, it is calculated and is set the value in the cache
cacheSolve <- function(x, ...) {
  inverse <- x$getsolve()
  if(!is.null(inverse)) {
    message("Getting cached data - Inverse of the matrix")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setsolve(inverse)
  inverse
}
