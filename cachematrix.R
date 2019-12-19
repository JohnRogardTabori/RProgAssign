## Title:  makeCacheMatrix and cachSolve Functions with Test Data
## Author: John Rogard Tabori
## Date:  12/20/2019
## Acknowlegement:  Basic structures and ideas for the functions
## taken from DPeng.  All errors are the sole province of the author.

## The makeCacheMatrix function consists of two small functions that 
## prepare for and store a retrievable inverse matrix in cache memory.

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) invrs <<- solve
  getinverse <- function() invrs
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve function consists of two parts, the first which
## checks to see if an inverted matrix of the specified form and
## data exists.  If such a matrix exists, it pulls it in and 
## prints it to the console.  If no inverted, cached matrix 
## exits, cacheSolve uses the matrix data from makeCacheMatrix, 
## inverts it using the "solve" function in R, stores it in cache
## memory for future use, and prints it to the console for
## inspection.

cacheSolve <- function(x, ...) {
  invrs <- x$getinverse()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  data <- x$get()
  invrs <- solve(data, ...)
  x$setinverse(invrs)
  invrs
}

##  Test Data Using a 2 X 2 Matrix 

x <- makeCacheMatrix(matrix(1:4, 2, 2))
cacheSolve(x)
cacheSolve(x)

##  Test Data Using a 3 X 3 Matrix

x <- makeCacheMatrix(matrix(c(1,4,7,2,9,11,0,5,9),3,3))
cacheSolve(x)
cacheSolve(x)
