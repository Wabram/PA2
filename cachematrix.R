## Functions caching the inverse of a matrix.
## Makeing a special matrix object that can cache its inverse:
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the matrix returned by "makeCacheMatrix" function.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the "cachesolve" function should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        ## Return the inverse
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        ## Get the matrix from our object
        data <- x$get()
        ## Calculate the inverse
        m <- solve(data, ...)
        ## Set the inverse to the object
        x$setInverse(m)
        ## Return the matrix
        m
}
