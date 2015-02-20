## The following function calculates the inverse of a matrix.
## It first checks if the inverse is already calculated and cached,
## and if not it calculates the inverse and caches the result
## for future use

## The function "makeCacheMatrix" sets and gets a matrix and its inverse 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse,
    getinverse = getinverse)
}


## The function cacheSolve calculates the inverse of a matrix
## after ensuring the inverse has not been cached

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
