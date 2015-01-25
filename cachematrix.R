## Functions in this file allow to create and solve (invert) a chacheable matrix
## At the end of the file there is a test that tests that the functions work.

## This functions takes a matrix as argument, sets its inverse to null
## and creates helper functions to get and set the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This functions received a matrix built with makeCacheMatrix(),
## checks if the inverse has been caches and returns it if so, otherwise
## calculates and caches the inverse

cacheSolve <- function(x) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("Getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}

## Simple test to ake sure the functions work.

make_cache_matrix_test <- function() {
  ## create a invertible 3x3 matrix
  data <- matrix(data = c(1, 3, 0, 2, -2, 1, -4, 1, -1), nrow=3, ncol=3)
  my_matrix <- makeCacheMatrix(data)
  
  ## use our cache solve function
  inv_cached <- cacheSolve(my_matrix)
  ## inverse the matrix to get a reference result
  inv <- solve(data)
  
  print(all(inv_cached == inv))
  
  ## use our function again, the cached value should be used
  inv_cached <- cacheSolve(my_matrix)
  
  print(all(inv_cached == inv))
}
