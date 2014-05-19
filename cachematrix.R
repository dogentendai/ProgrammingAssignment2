## The following functions permit to:
##  - create a special "matrix" that can cache its inverse
##  - compute the inverse of it

## The following makeCacheMatrix function creates a container
## to chache a matrix and its inverse
## Input parameter: x a matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The cacheSolve function perform the inverse of a given matrix (makeCacheMatrix) or
## gets it from a cache if already present.
## Input parameter: x a makeCacheMatrix object
## Return value: an inverse matrix
cacheSolve <- function(x, ...) {
  print("solve .....")
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
