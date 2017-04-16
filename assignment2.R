## The function "makeMatrix" is able to cache the inverse of a matrix.
## This function creates a martix object that can cache its inverse.

makeMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inver <<- inverse
  getInverse <- function() inver
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inver <- x$getInverse()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setInverse(inver)
  inver
}

