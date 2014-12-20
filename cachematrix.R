## Below you can find implementations of two functions that enable caching the result of matrix inversion
## First of them constructs the object which enables the caching and the second is the interface to use the object

## This function creates a 4 element object that enables storing and viewing a matrix and its inversion 
## (this function doesn't the second object to be an inversion, however we will use it for this purpose)

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <- function(y) {
    x<<-y
    inv<<-NULL
  }
  get <- function() x
  setinv <- function(inverse) inv<<-inverse
  getinv <- function() inv
  return(list(set=set, get=get, setinv=setinv, getinv=getinv ))
}


## This function checks whether matrix inversion is already calculated for the object.
## If yes, it returns the existing result, else it calculates the inversion, stores and returns it.

cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(!is.null(inv)) {
    message("Data found in cache, returning")
    return(inv)
  }
  inv<-solve(x$get(),...)
  x$setinv(inv)
  return(inv)
}
