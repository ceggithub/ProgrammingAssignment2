## This functions to assign a matrix to an environment(Cache) and recovers.
## Does the same with the inverse of the matrix.

## This function records and retrieves the matrix and the inverse matrix cache.
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL ##Initializes the inverse matrix for the first time.
  
  ## Assign the value of the matrix when demanded.
  set <- function(y) {
    x <<- y
    inv <<- NULL ## Matrix has changed then the inverse matrix is cleared.
  }
  
  ## Only return the matrix.
  get <- function() x
  
  ## Put the inverse matrix in the cache
  setinv <- function(inverso) {
    inv <<- inverso
  }
  
  ## Only return the inverse matrix
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## This function does the calculation of the inverse matrix.
cacheSolve <- function(x, ...) {
  
  ## Recover a inverse matrix cached. If exist then return it.
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting Cached Data")
    return(inv)
  }
  
  ## If it does not exist then get and calculate
  data <- x$get()
  inv <- solve(data)
  
  ## Writes the result of the calculation in the cache
  x$setinv(inv)
  
  ## And return
  inv
}


