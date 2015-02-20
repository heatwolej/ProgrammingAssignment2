## Together, these functions will cache the inverse of a matrix so that the inverse can be retrieved
## in lieu of multiple repetitions of the expensive set of operations required to calculate an inverse

## This function creates a special matrix object that can cache its inverse.
## The function takes one argument (x) which is the input matrix

makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL

  ## This setter function takes one parameter (y) and sets the basic matrix to that value
  set <- function(y) {
    ## Note that the <<- implies that we are setting the values of invMat and x in the
    ## makeCacheMatrix environment
    x <<- y
    invMat <<- NULL
  }
  
  ## getter for x (the basic input matrix)
  get <- function() x
  
  ## setter for Inverse function
  setInv <- function(inv) {
    invMat <<- inv
  } 
  
  ## getter for inverse matrix
  getInv <- function() invMat
  
  ## create a list of functions to call
  list(set = set, 
       get = get,
       setInv = setInv,
       getInv = getInv)
  
}

## This function computes the inverse of the special matrix returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then
## cacheSolve should retrieve the inverse from the cache.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  invMat <- x$getInv()
  if(!is.null(invMat)) {
    ## already cached, no need to call "solve"
    message("getting cached data")
    return(invMat)
  } else {
    ## not yet cached
    message("calculating inverse")
    data <- x$get()
    ## "solve" is an R function that calculates the inverse of a square matrix
    invMat <- solve(data, ...)
    x$setInv(invMat)
    invMat
  }
  
}
