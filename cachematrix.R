## makeCacheMatrix initializes x as a empty matrix, sets the inverse to NULL
## the set() function assigns y to the x object of the parent environment
## and clears any value of iv cached by cacheSolve earlier
## get() defines the getter for the matrix x
## setinverse() defines the setter for the inverse iv
## getinverse() defines the getter of thhe inverse
## each of these functions are then assigned as an element within a list
## in order to return it to the parent environment
## so it gives the name 'set' to the set() function defined above etc.
## Naming the list elements allows to use the $ sign to access the functions by name

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
## it tests if the inverse is NULL, if thats is false it 
## calculates the inverse (using the solve() function) and stores it by setinverse()

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
