## CACHING THE INVERSE OF A MATRIX
##This assignment is about writing a pair of functions that cache the inverse of a matrix.


##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##1. set the value of the matrix  2. get the value of the matrix
##3. set the value of the matrix inversion  4. get the value of the matrix inversion

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ##set the value of the matrix
  set <- function(y){
    x<<-y
    m<<-NULL
  }
  ##get the value of the matrix
  get<-function() x
  ##set the value of inverse matrix
  setsolve <- function(solve) m<<-solve
  ##get the value of inverse matrix
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)){
    ##If the inversion of the matrix "x" has already been calculated,
    ##it gets the matrix inversion from the cache and skips the computation
    message("getting cached data")
    return(m)
  }
  ##If the inverision hasn't been calculated yet, it calculates the inverse matrix
  ##and sets the value of the inversion in the cache via the setsolve function.
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
