## These functions create a matrix inverse that can be stored in the parent environement

## This function defines a list of of 4 functions. Set() defines the input variable and an empty inverse variable in the parent environment.
## Get() retrives the defined input variable from the parent environment.
##setinv() Calculates the inverse of the input and saves it to the parent environment.
## getinv(0 Retrives the saved inverse from the parent environment.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #matrix(,nrow = dim(x)[1], ncol = dim(x)[2])
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This finction takes names lists from makeCacheMatrix() adn tests if an inverse already
## exists in the parent environment. If the inverse exists it print the existing inverse, 
## if the inverse does not exist and inverse is calcuated and printed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
