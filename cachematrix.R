## This program contains a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
  inv <<- NULL
  }
  
  get <- function() x
  setInv <- function(solve) inv <<- solve   
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
    
}

## This function does the actual inversing of matrix x.  It first checks if the in-
## verse matrix has been found; if yes, returns the result and quits. If not, the 
## inverse of x is calculated, saved to cached, and returned.


cacheSolve <- function(x=matrix(), ...) {

  inv<- x$getInv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get() # obtains matrix from object x
    inv <- solve(data, ...)  # finds inverse matrix
    x$setInv(inv) # assigns resulting inverse matrix to object x
    inv
  
  
}
