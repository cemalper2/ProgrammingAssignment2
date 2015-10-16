## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function



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


## Write a short comment describing this function

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
