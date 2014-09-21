## The below two functions create and reuse a list containing a matrix and its inverse. In a computationally intensive looping,
## the below functions provide a reuse feature
## Using the functions: initialize/create the list using a command line interface 
## e.g., Step1: mat<-matrix(c(10,21,33,45),2,2) ; Step2: matrix_list<-makeCacheMatrix(mat);
## Once the list is initialized, the cacheSolve function can use this list to either update the inverse of the matrix Or
## reuse a previously computed inverse

## makeCacheMatrix creates the list and should be initially from outside the cacheSolve e.g., command line... 
## ...Only post-creation of the CacheMatrix list, should cacheSolve invoke or attempt to reset the inverse of another matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## CacheSolve takes in a list and returns the inverse of the matrix
## The function attempts at retrieving the inverse from the Cache. In case the inverse is not present in the cache, the
## function creates an inverse (using Solve()) and  updates the list (created initially using the makeCacheMatrix command)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
