## Put comments here that give an overall description of what your
## functions do

## Create the function called makeCacheMatrix to make cache matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL

  set <- function(y) {
    x <<- y
    inv <<- NULL
  }

  get <- function() x

  setinverse <- function(inverse) {
    inv <<- inverse
  }

  getinverse <- function() inv

  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Create the function called cacheSolve to cache the inverse of the matrix

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()

  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
