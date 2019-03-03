## I have two functions, the first takes a matrix and provides in-built functions/arguements for caching it.
## The second computes the inverse of the matrix inputed in the first and returns the inverse if 
## it has already been calculated; and then retrieves the inverse from the cache.

## This function takes as input a matrix, creates an empty object, `m` and sets a 
## prerequisite functions which allow us to obtain the inverse of the given matrix. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function takes a function of the type makeCacheMatrix() and returns the inverse of the matrix it defines. It can also 
## return a cached value for the inverse of a matrix if this is already available (has been previously called)

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if (!is.null(m)) {
    message("loading cached inverse")
    return(m)  
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
