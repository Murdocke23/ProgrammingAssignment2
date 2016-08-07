##for computing and caching the Inverse of a square matrix

## caching function to store original matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  
  setMatrix <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  getMatrix <- function() {
    x
  }
  
  setInverse <- function(inverseToApply) {
    inverseMatrix <<- inverseToApply
  }
  
  getInverse <- function() {
    inverseMatrix
  }
  
  list(
    setMatrix = setMatrix, 
    getMatrix = getMatrix,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## solves for inverse of a square matrix
## checks first for cached inverse, but if does not exist,
## then computes and caches the inverse
## assumes inverse matrix exists
cacheSolve <- function(x, ...) {
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)){
      message("retrieving cached inverse matrix")
      return(invMatrix)
  }
  matrixData <- x$getMatrix()
  invMatrix <- solve(matrixData, ...)
  x$setInverse(invMatrix)
  invMatrix
}
