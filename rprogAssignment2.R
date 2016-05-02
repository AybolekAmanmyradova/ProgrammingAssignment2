#Caching the inverse of a Matrix
#The following function creates a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
#The following function gives us inverse of an above special matrix. If we alredy found it inverse it does not calculate it again, just retrieves the inverse from cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matr <- x$get()
  inv <- solve(matr, ...)
  x$setinverse(inv)
  inv
}
