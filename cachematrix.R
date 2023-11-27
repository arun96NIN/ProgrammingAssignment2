## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mat = matrix()) {
  inverse <- NULL
  
  set <- function(matrix) {
    mat <<- matrix
    inverse <<- NULL
  }
  
  get <- function() mat
  
  setinverse <- function(inv) {
    inverse <<- inv
  }
  
  getinverse <- function() inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(cacheMatrix, ...) {
        ## Return a matrix that is the inverse of 'x'
  cached_inverse <- cacheMatrix$getinverse()
  
  if (!is.null(cached_inverse)) {
    message("Getting cached inverse")
    return(cached_inverse)
  }
  
  matrix_to_invert <- cacheMatrix$get()
  inv <- solve(matrix_to_invert, ...)
  
  cacheMatrix$setinverse(inv)
  
  inv
  
}

#inversed matrix as output

mat <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2))
cacheSolve(mat)