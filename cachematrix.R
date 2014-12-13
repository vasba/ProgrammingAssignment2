## Create an object that can cache its matrix and its inverse.
## Create a function that uses the cached matris object
## to retrieve the inverse

## Returns an object containing a matrix and it's cached inverse
## The inverse of the matrix must be set after the object is created.

makeCacheMatrix <- function(x = matrix()) {
  minverse <- NULL
  set <- function(y) {
    x <<- y
    minverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) minverse <<- inverse
  getinverse <- function() minverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Gets the inverse for the cached matrix in 
## a makeCacheMatrix object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minverse <- x$getinverse()
  if(!is.null(minverse)) {
    message("getting inverse matrix")
    return(minverse)
  }
  data <- x$get()
  minverse <- solve(data, ...)
  x$setinverse(minverse)
  minverse
}
