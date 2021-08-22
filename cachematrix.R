## Overall Description: Getting the inverse of a matrix. Computing it if 
## the inverse haven't been generated and cached, otherwise retrived it
## from cache.

## makeCacheMatrix: Generating a matrix that is able to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL                      
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  setinv <- function(inverse_matrix) inv_matrix <<- inverse_matrix
  getinv <- function() inv_matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve: Calculating the inverse of the matrix returned by 
## makeCacheMatrix. If the inverse has already been computed and and the matrix 
## has not been modified, cachesolve will directly retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_matrix <- x$getinv()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  data <- x$get()
  inv_matrix <- solve(data, ...) # solve() Obtaining the inverse matrix 
  x$setinv(inv_matrix)
  inv_matrix
}
