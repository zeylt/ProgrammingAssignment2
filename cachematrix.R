## The following functions 'makeCacheMatrix' and 'cacheSolve' allow one to
## store and reuse the inverse of a matrix so that it doesn't have to be
## continually recomputed (which is computationaly expensive). The matrix
## provided to makeCacheMatrix is assumed to be square and invertible.
## Example usage:
## A <- makeCacheMatrix(rbind(c(1,2),c(2,1)))
## B <- cacheSolve(A)


## `makeCacheMatrix` creates a special "matrix", which is
## really a list containing a function to
##
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  M <- NULL
  set <- function(y) {
    x <<- y
    M <<- NULL
  }
  get <- function() x
  setinv <- function(solve) M <<- solve
  getinv <- function() M
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## 'cacheSolve' calculates the inverse of the special "matrix"
## created by 'makeCacheMatrix'. However, it first checks to see if the
## inverse has already been calculated and stored. If so it simply returns
## the cached version of the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  M <- x$getinv()
  if(!is.null(M)) {
    message("getting cached inverse")
    return(M)
  }
  data <- x$get()
  M <- solve(data, ...)
  x$setinv(M)
  M
}
