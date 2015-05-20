## 1.makeCacheMatrix: This function creates a special "matrix" object 
# that can cache its inverse
## 2.cacheSolve: This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already been 
# calculated (and the matrix has not changed), then the cachesolve should 
# retrieve the inverse from the cache.

## The first function, makeCacheMatrix creates a special "matrix", which is 
# really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse of the matrix
# get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  # set the value of the inverse of the matrix
  setinverse <- function(inverse) i <<- inverse
  # get the value of the inverse of the matrix
  getinverse <- function() i
  #return list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function gets the inverse of the special "matrix" created 
# with the above function. 
# It first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix 
# and sets the inverse of the matrix in the cache via the solve function.

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
  # It first checks to see if the inverse has already been calculated. 
  i <- x$getinverse()
  if(!is.null(i)) {
    #it gets the inverse from the cache and skips the computation.
    message("getting cached inversed matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...) #it calculates the inverse of the matrix using "solve"
  x$setinverse(i)
  i
}
