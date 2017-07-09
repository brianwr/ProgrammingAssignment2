## Put comments here that give an overall description of what your
## functions do

#This function creates a special "vector" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setcachematrix <- function(solve) i <<- solve
  getcachematrix <- function() i
  list(set = set, get = get,
       setcachematrix = setcachematrix,
       getcachematrix = getcachematrix)
}

#This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If inverse has not already been calculated (and 
#the matrix has changed), then the cachesolve will store the 
#inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  i <- x$getcachematrix()
  if(!is.null(i)) {
    message("getting cached matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setcachematrix(i)
  i
}
