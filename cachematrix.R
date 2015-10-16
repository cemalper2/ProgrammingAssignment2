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



## Returns the invert of the matrix x.
##In case the invert is cached, no operation is made and the cached value is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinvert()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$getdata()
  inv <- solve(as.matrix(data))
  x$setinvert(inv)
  inv
  
}
