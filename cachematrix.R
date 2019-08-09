## R Programming Course
## Programming Assignment 2: Lexical Scoping

## This function creates a special matrix object that can cache its
## inverse

makeCacheMatrix <- function(x = matrix()) {
  x_inverse <- NULL
  set <- function(y) {
    x <<- y
    x_inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(compute_inverse) x_inverse <<- compute_inverse
  get_inverse <- function() x_inverse
  list(set = set, get = get, 
       set_inverse = set_inverse, 
       get_inverse = get_inverse)
}


## This function computes the inverse of the speciay matric
## returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  x_inverse <- x$get_inverse()
  if (!is.null(x_inverse)) {
    message("getting cached data")
    return(x_inverse)
  }
  data <- x$get()
  x_inverse <- solve(data, ...)
  x$set_inverse(x_inverse)
  x_inverse
}
