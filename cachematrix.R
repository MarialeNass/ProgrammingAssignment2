## This is a function that creates a matrix
## it then caches the matrix itself by storing its value
## in memory for use at a later time
## I copied from the example given for this assignment

## The first function, makeVector creates a special "vector" (in ## this case it will be a MATRIX), which is really a list 
## containing a function to:

## set the value of the matrix
## get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrixinv <- function(solve) m <<- solve
  getmatrixinv <- function() m
  list(set = set, get = get,
       setmatrixinv = setmatrixinv,
       getmatrixinv = getmatrixinv)
}



## As before I started by using the code from the vector example
## Return a matrix that is the inverse of 'x'

## the user should try this code by doing the following:
## defining a matrix, an example is given here (which by the way ## is explained here ==> https://www.coursera.org/learn/r-programming/discussions/weeks/3/threads/ePlO1eMdEeahzg7_4P4Vvg

## please try ( from the link above):
## m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
## myMatrix_object <- makeCacheMatrix(m1)
## cacheSolve(myMatrix_object)

cacheSolve <- function(x, ...) {
  m <- x$getmatrixinv()
  if(!is.null(m)) {
    message("getting cached data from makeCacheMatrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrixinv(m)
  m
  
}

