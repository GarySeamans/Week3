## This function is to make a matrix that
## caches its inverse. 

## Modified from the "makeVector" example
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  SetInvMat <- function(InvMat) m <<- InvMat
  GetInvMat <- function() m
  list(set = set, get = get,
       SetInvMat = SetInvMat,
       GetInvMat = GetInvMat)
}


## This function computes and returns
## the inverse of the matrix from
## the "makeCacheMatrix" function

## Modified from the "cachemean" example
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$GetInvMat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$SetInvMat(m)
  m
  
}
