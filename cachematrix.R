## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## set vector and make inv variable null
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## get function returns the matrix
  get <- function() x
  ## sttinverse function sets the inverse of matrix
  setinverse <- function(inverse) inv <<- inverse
  ##getinverse returns the inverse of matrix
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## first check if inverse exists in cache. if yes, return it
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached value of matrix inverse")
    return(inv)
  }
  
  ## If not, compute it and return it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
