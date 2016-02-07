## Functions makeCacaheMatrix and cacheSolve implement the solution to 
## the second programming assignment and create a cached version of a 
## matrix inversion.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolved <- function(solved) s <<- solved
  getsolved <- function() s
  list(set = set, get = get,
       setsolved = setsolved,
       getsolved = getsolved)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolved()

  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolved(s)
  s
}
