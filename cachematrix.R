## These R functions cache time-consuming computations for Matrix inversions.
## These pair of functions cache the inverse of a matrix

## makeCacheMatrix: This function creates a special "matrix" object that can
## cache its inverse.

## makeCacheMatrix contains a list containing functions to
##    1. set the matrix
##    2. get the matrix
##    3. set the inverse
##    4. get the inverse
##    this list is used as input to the function cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
      mInv = NULL
      set = function(y) {
            x <<- y
            mInv <<- NULL
      }
      get = function() x
      setInv = function(inverse) mInv <<- inverse 
      getInv = function() mInv
      list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then cacheSolve should
## retrieve the inverse from the cache.

## Return a matrix that is the inverse of 'x'
## return: inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
      mInv = x$getInv()
      
      # check if the inverse has already been calculated
      if (!is.null(mInv)){
            # get data from the cache and skip computation. 
            message("getting the cached data")
            return(mInv)
      }
      
      # inverse not yet calculated, so calculate...
      matrixData = x$get()
      mInv = solve(matrixData, ...)
      
      # set value of the inverse in the cache using the setInv function.
      x$setInv(mInv)
      
      return(mInv)
}
