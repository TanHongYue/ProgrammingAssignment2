## Creating two functions here, makeMatrix and cacheSolve. makeMatrix helps to calculate the inverse of a matrix,
## which needs to be inputed as an argument. cacheSolve checks if the inverse of a matrix has already been calculated
## in makeMatrix - if the inverse of a matrix has already been calculated, cacheSolve will cache the value, otherwise,
## it will compute the inverse of the matrix.

## Within makeMatrix, there are 4 functions set(), get(), setInverse(), getInverse().
makeMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
