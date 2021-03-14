## Function makeCacheMatrix() produces list of functions for setting and getting
## back to the environment values of matrix and inverse matrix. Function makeCacheMatrix()
## should be called on your initial matrix before cacheSolve() to set initial values!

## Function cacheSolve() checks if inverse matrix is already solved in the environment and  
## returns inverse matrix. Otherwise performs matrix inverse solving, save inverse matrix 
## and returns result. There is no checking for square matrix and det()!=0 
## because for this assignment we assume that matrix is always invertible
 

## Function for setting matrix list parameters and defining Set&Get functions
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solved) inv <<- solved
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Function for matrix solving
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrixdata <- x$get()
  inv <- solve(matrixdata, ...)
  x$setinv(inv)
  inv
}
