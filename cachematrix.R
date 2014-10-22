## Programming Assignment #2 of...
##    Coursera DataScience course on R Programming
##    (https://class.coursera.org/rprog-008/human_grading/view/courses/972581/assessments/3/submissions)
## raju@intellisoft.ch 2014-10-22

## This file contains a pair of functions that cache the inverse of a matrix

## makeCacheMatrix creates a special 'matrix' which is actually
## a list containing the following:
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the value of the inverse
##   4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special 'matrix' created
## by the above function. It first checks to see if the inverse has already
## been calculated and, if it has, it returns that skipping the calculation.
## Otherwise, it calculates the inverse and sets the inverse in the 
## cache via the setinverse function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## ----- Test ------
## To test the above functions at the R prompt, source the file and then:
## > m <- matrix(c(2,2,3,2), nrow=2, ncol=2)
## > a <- makeCacheMatrix(m)
## > cacheSolve(a) # no caching the first time around
## > cacheSolve(a) # this time it will show that the cached data is used

