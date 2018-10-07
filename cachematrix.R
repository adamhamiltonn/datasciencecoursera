## Put comments here that give an overall description of what your
## functions do

## creates matrix object that can cache its inverse for the input

makeCacheMatrix <- function(x = matrix()) {

  invrse <- NULL
  set <- function(y) {
    x <<- y
    invrse <<- NULL
  }
  get <- function() x
  setinvrse <- function(inverse) invrse <<- inverse
  getinvrse <- function() invrse
  list(set = set, get = get, setinvrse = setinvrse, getinvrse = getinvrse) 
  
}


## Computes the inverse of "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, then it should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  invrse <- x$getinvrse()
  if(!is.null(invrse)) {
    message("getting cached result")
    return(invrse)
  }
  data <- x$get()
  invrse <- solve(data, ...)
  x$setinvrse(invrse)
  invrse
}

## ---------------Check the program------------------------
m <- matrix(rnorm(16),2,2)
m1 <- makeCacheMatrix(m)
cacheSolve(m1)

