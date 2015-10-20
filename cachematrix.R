## makeCacheMatrix creates and returns a list of functions:
## setdata: Changes the data of the CacheMatrix x and resets the inverse.
## getdata: Returns the data contained in x.
## setinvert: Calculates the invert of the matrix x.
## getinvert: Returns the invert of the matrix x --> interted_mat.


makeCacheMatrix <- function(x = matrix()) {
inverted_mat <- NULL
setdata <- function(y) {
  x <<- y
  inverted_mat <<- NULL;
}
getdata <- function() as.matrix(x)
setinvert <- function(invert) inverted_mat<<- invert
getinvert <- function() inverted_mat
list(setdata = setdata, getdata = getdata, setinvert = setinvert, getinvert = getinvert)
}



## Returns the inverse of the matrix x.
##In case the inverse is cached, no operation is made and the cached value is returned.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  
  
  inv <- x$getinvert()              ## Gets the inverted value of matrix x
  if(!is.null(inv)) {               ## null returned if it is not calculated before
    message("getting cached data")  ## if inv!=NULL, then it returns the cached value.
    return(inv)
  }
  data <- x$getdata()               ## inverse not calculated, gets the matrix x.
  inv <- solve(as.matrix(data))     ## Calculate the inverse
  x$setinvert(inv)                  ## sets the invert (caches).
  inv                               ## returns the value
  
}
