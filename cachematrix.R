## This programme calculates the inverse of a matrix in an 
## efficient way. When the inverse is calculated, it is cached.
## When the inverse is needed again, it get retrieve it from
## the cache. It also checks whether the matrix did not change
## in the mean time, in which case it will be calculated again.



## 'makeCacheMatrix' creates a special matrix object
## of which the inverse can be cached.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## Set the value of the matrix
  set<- function(y) {
    x <<- y 
    inv <<- NULL
  }
  
  ## Get the value of the matrix
  get <- function() x
  
  ## Set the inverse of the matrix
  setInv <- function(inverse) inv <<- inverse
  
  ## Get the inverse of the matrix
  getInv <- function() inv
  
  ## Return a list with the 4 functions
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)    
}



## 'cacheSolve' computes the inverse of a special matrix object 
## (as created by 'makeCacheMatrix'), but only does so when
## the inverse is not yet calculated for the curren matrix. 
## Otherwise it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  ## If the inverse is already calculated, return inv
  if(!is.null(inv)) {
    message("Retrieving the cached data...")
    return(inv)
  }
  
  ## Otherwise, calculate the inverse and save it
  ## in the special matrix object
  matrix <- x$get()
  inv <- solve(matrix)
  x$setInv(inv)
  inv
}