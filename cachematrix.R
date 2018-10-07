## Put comments here that give an overall description of what your
## functions do

## creates matrix object that can cache its inverse for the input

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv) 
  
}


## Computes the inverse of "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, then it should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

## ---------------Check the program------------------------
m <- matrix(rnorm(16),2,2)
m1 <- makeCacheMatrix(m)
cacheSolve(m1)

