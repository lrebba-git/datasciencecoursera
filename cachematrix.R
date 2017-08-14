## The below both functions makeCacheMatrix and cacheSolve are used to create the inverse of a matrix
## and cache it

## makeCacheMatrix creates and returns the list of functions used by cacheSolve to
## get and set the value of inverted matrix in cache. It first creates the matrix in working environment, get its value,
## inverts the matrix and stores in cache and get the inverted matrix from cache

makeCacheMatrix <- function(x = matrix()) {
  
  cacheinverse <- NULL
  setValue <- function(y) {
    x <<- y
    cacheinverse <<- NULL
  }
  getValue <- function() x
  setInverseValue <- function(inverseValue) cacheinverse <<- inverseValue
  getInverseValue <- function() cacheinverse
  list(setValue=setValue, getValue=getValue, setInverseValue=setInverseValue, getInverseValue=getInverseValue)
}

## cacheSolve calcluates the inverse of the matrix created in the above function - makeCacheMatrix
## It checks for the inverted matrix, if it does not exist in cache,
## it it created in the working environment and the inverted value of it is stored in cache

cacheSolve <- function(M, ...) {
  inverse <- M$getInverseValue()
  if(!is.null(inverse)) {
    message("Inverse was already calculated. Here you go:")
    return(inverse)
  }
  value <- M$getValue()
  inverse <- solve(value)
  M$setInverseValue(inverse)
  inverse
}

## Test Case:

## Matrix <- matrix(c(4,7,9,11),2,2)
## Cache <- makeCacheMatrix(Matrix)

## cacheSolve(Cache)

## First run:
##            [,1]       [,2]
## [1,] -0.5789474  0.4736842
## [2,]  0.3684211 -0.2105263

## cacheSolve(Cache)

## Second run: gets the value from cache

## Inverse was already calculated. Here you go:
##            [,1]       [,2]
## [1,] -0.5789474  0.4736842
## [2,]  0.3684211 -0.2105263
