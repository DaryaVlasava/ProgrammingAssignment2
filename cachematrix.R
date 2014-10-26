## makeCacheMatrix function creates a "matrix" object
## cacheSolve returns inverse of a matrix, if it has already been calculated, or 
## computes and cache the inverse

## makeCacheMatrix function creates a "matrix" object, which is a list of functions:
## set the matrix, get the matrix, set the inverse of the matrix and get the inverse 
## of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
} 


## cacheSolve function return the inverse of the matrix, if it has been already computed, 
## or, if not, calculates the inverse and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x%setInv(inv)
  inv
}
