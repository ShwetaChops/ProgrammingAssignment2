# This set of functions provide the ability to cache the inverse of a matrix 
# in order to avoid repeat computation. Thus, it provides a more effective way
# of conducting matrix inversion.

## The function makeCacheMatrix() creates a special "matrix" object that can 
## cache its inverse. This consists of a list of functions to:
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
## This special "matrix" object is then used as an input in the next function 
## - cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  ## x is a square invertable matrix.
  ## Initialize the value of the inverse to NULL.  
  inv <- NULL
  ## Create another function set where the matrix and value of the inverse 
  ## will be cached, and changes made to the matrix will give effect.
  set <- function(){
    x <<- y
    inv <<- NULL
  }
  ## Returns the matrix
  get <- function() x
  ## setinverse function sets the inverse for a non-singular matrix
  setinverse <- function(inverse) inv <<- inverse
  ## getinverse function gets the inverse
  getinverse <- function() inv
  ## naming the elements of the list of functions returned
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the function will retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  ## x is the output of makeCacheMatrix()
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  ## if the inverse has already been computed and cached
  if (!is.null(inv)){
    ## skip computation and pick it up from the cache
    print("Getting Cached Data")
    return(inv)
  }
  ## else, the inverse is computed
  data <- x$get()
  inv <- solve(data,...)
  ## the now computed inverse, is cached for future use
  x$setinverse(inv)
  ## the inverse is returned
  return(inv)
}

