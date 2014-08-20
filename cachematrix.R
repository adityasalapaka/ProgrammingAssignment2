## These functions cache the inverse of a matrix to eliminate the need 
## for repeated computation of inverse of the same matrix. The functions
## create a special object to store a matrix and cache its inverse.

## makeCacheMatrix creates an special "matrix" which is a list containing
## functions to set the value of the matrix, get the value of the matrix, 
## set the inverse of the matrix and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y)  {
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setinverse <- function(solve) m<<-solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## cacheSolve calculates the inverse of the special "matrix" created by
## makeCacheMatrix. It firsts checks to see if the inverse has been caluclated
## and if so, it gets the inverse from the cache and skips computation. Else
## it calculates the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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