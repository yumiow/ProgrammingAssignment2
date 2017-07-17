## My functions creates inverted matrix using R scoping
## rules and with caching functionality. It helps saving intermediates
## as inputs for the cached values.

## Starts with cache matrix(function)

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inverseMatrix) inverse <<- inverseMatrix
  getInverseMatrix <- function() inverse
  list(set = set,
       get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## Then the solve() function gets inverse of the matrix
## using the makeCacheMatrix() function I created above.
## Basically this is executed by if there's inverse be calculated already
## and matrix is not changed, then retrieve inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverseMatrix() # from cache
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get() # get
  inverse <- solve(data, ...) # solve
  x$setInverseMatrix(inverse)
  inverse # return
}
