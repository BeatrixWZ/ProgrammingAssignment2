## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix sets the inverse to NULL, then calls the function y 
## and sets the new inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL
  set <- function(y) {
    x <<- y
    iv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) iv <<- inverse
  getinverse <- function() iv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns a matrix that is the inverse of x
## it tests if the inverse is NULL, if thats the case the function 
## calculates the inverse and stores it

cacheSolve <- function(x, ...) {
  iv <- x$getinverse()
  if (!is.null(iv)) {
    message("getting cached data")
    return(iv)
  }
  data <- x$get()
  iv <- solve(data, ...)
  x$setinverse(iv)
  iv
}
