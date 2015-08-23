## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#The given twofunctions are used to cache the inverse of a matrix having a special object

#This function creates a matrix with a special object .
makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invrs <<- inverse
  getinverse <- function() invrs
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. 
#if the inverse is already present then it shows the result  and gets inverse from cache.
#if not then it sets the inverse value to the cache.


cacheSolve <- function(x, ...) {
  invrs <- x$getinverse()
  if(!is.null(invrs)) {
    message("getting data from cache.")
    return(invrs)
  }
  data <- x$get()
  invrs <- solve(data)
  x$setinverse(invrs)
  invrs
}

