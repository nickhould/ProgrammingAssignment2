## Matrix inversion is usually a costly computation.
## The two functions below are a simple system to cache the results
## of a matrix inversion. 

## The `makeCacheMatrix` function creates a 
## special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inversedMatrix <- NULL
  set <- function(newMatrix) {
    x <<- newMatrix
    inversedMatrix <<- NULL
  }
  get <- function() x
  setInversedMatrix <- function(im) inversedMatrix <<- im
  getInversedMatrix <- function() inversedMatrix
  list(set=set,
       get=get,
       setInversedMatrix=setInversedMatrix,
       getInversedMatrix=getInversedMatrix
       )
}


## The `cacheSolve` function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  inversedMatrix <- x$getInversedMatrix()
  if(!is.null(inversedMatrix)) {
    message("getting cached data")
    return(inversedMatrix)
  }
  
  data <- x$get()
  inversedMatrix <- solve(data, ...)
  x$setInversedMatrix(inversedMatrix)
  inversedMatrix
  
}

