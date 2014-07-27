## These two functions allow the inverse of a matrix to be stored, or
## cached so it won't have to be recalculated unless the matrix changes. 

## This function creates a 'matrix' object that caches the inverse
## of the input matrix. It is assumed that the input matrix is invertible.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function (y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse,
         getInverse = getInverse)
}


## This function checks to see if the inverse has already been calculated
## and cached. If so, it retrieves the cached inverse matrix. If not, it 
## calculates the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
