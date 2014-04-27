## the functions in this file are designed to aid in the sometimes computational and memory intensive
## process of inverting a matrix in the R language
## two functions are in this file:
## makeCacheMatrix()
## cacheSolve()
## descriptions of each of these functions may be found in comments above the function itself


## makeCacheMatrix is an custom function designed to create a special matrix object that can cache 
## its inverse
## parameter(s):  a matrix
## returns:  a customized list object based on the matrix passed as the input parameter
makeCacheMatrix <- function(x = matrix()) {
  inverseOfMatrix <- NULL
  
  set <- function(newMatrixValue){
    x <<- newMatrixValue
    # set inverseOfMatrix to NULL at higher scope because if set new matrix then any old inverse calculation
    # is old and could be incorrect
    inverseOfMatrix <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(newInverseMatrix){
    inverseOfMatrix <<- newInverseMatrix
  }
  
  getInverse <- function(){
    inverseOfMatrix
  }
  
  list(set = set, get = get, setInverse= setInverse, getInverse = getInverse)
}


## cacheSolve actually caculates the inverse matrix and caches it so it does not have to be recalculated
## requirement for function users:   the matrix supplied must be invertible.  
## no error checking is done to prevent a non-invertible matrix from being used in this function
## parameters:  x must be of "makeCacheMatrix" type
cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverse()
  
  # if the inverse of the matrix contained in the speical x object has already been calculated
  # then just returned that cached inverse (returns the inverse and exits the function here)
  if (!is.null(inverseMatrix)){
    message("returning cached inverse matrix")
    return(inverseMatrix)
  }
  
  # if the inverse has not been calculated then calculate the inverse, set it, and return inverse of x
  x$setInverse(solve(x$get())) # sets inverse in the special x object
  ## returns inverse of x as a matrix directly
  x$getInverse()
  
}
