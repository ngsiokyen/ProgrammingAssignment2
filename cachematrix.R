## This assignment is to write a pair of functions that cache the inverse of a matrix

## The first function, makeCacheMatrix, is a list containing functions to
## 1. set the matrix
## 2. get the matirx
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv.mat <- NULL
  set <- function(y) {
    x <<- y
    inv.mat <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv.mat <<- inverse
  getInverse <- function() inv.mat
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The second function, cacheSolve, computes the inverse of the matrix created with the first function.
## It will first checks to see if the inverse has already been computed.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, the inverse will be computed and the value will be set in the cache 
## via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv.mat <- x$getInverse()
  ## If the inverse has already been computed
  if (!is.null(inv.mat)) {
    ## Get it from the cache and skips the computation
    message("getting cached data")
    return(inv.mat)
  }
  ## Otherwise, computes the inverse of the matrix
  matdata <- x$get() ## run the get function to get the value of the input matrix
  inv.mat <- solve(matdata, ...)
  ## Sets the value of the inverse in the cache via the setInverse function.
  x$setInverse(inv.mat)
  inv.mat
}
