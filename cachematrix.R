## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#The given twofunctions are used to cache the inverse of a matrix having a special object

#This function creates a matrix with a special object .
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. 
#if the inverse is already present then it shows the result  and gets inverse from cache.
#if not then it sets the inverse value to the cache.


cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
