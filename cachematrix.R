## Put comments here that give an overall description of what your
## functions do

##Main Aim of this R draft is to discuss programme of two functions, namely,
##"makeCacheMatrix" and "cacheSolve".These functions togeather will cache the inverse of a matrix
##Being Mathematician I know how hard it can be to calculate inverse of matrix repeatedly,
##So we are programming for the R draft which can not only store a matrix but caches it's inverse , also.


## Write a short comment describing this function
##1. makeCacheMatrix function will create a special "matrix" object that can
##cache its inverse for the input matrix. clearly,here input matrix is a non singular matrix

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
