## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
## Your assignment is to write a pair of functions that cache the inverse of a matrix.

## Write the following functions:

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
  
  #init to NULL
  inver <- NULL
  
  #working env.
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  
  #get the matrix
  get <- function() {
    x
  }
  
  setinverse <- function(inverse) {
    inver <<- inverse
  }
  
  getinverse <- function() {
    inver
  }
  
  # functions available in the working env.
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Reverse the invertible matrix x

cacheSolve <- function(x, ...) {
   
  #get the inverse of the matrix from the cache
  
  inver <- x$getinverse()
  
  if(!is.null(inver)) {
    
    message("The following matrix is retrieved from the cache:")
    
    return(inver)
  }
  
  # build matrix
  mat <- x$get()
  # solve the matrix and stores in inver
  inver <- solve(mat)
  
  #set in the cache
  x$setinverse(inver)
  
  return(inver)
}
