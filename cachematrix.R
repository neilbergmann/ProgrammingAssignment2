##cacheSolve.R
## Neil Bergmann, 11 August 2014

## It can be expensive to calculate the inverse of a matrix, so
## if the inverse is calculated repeatedly it can be useful
## to cache the inverse to avoid unnecessary calculations.

## makeCacheMatrix() creates a "special matrix" with a cached inverse,
## cacheSolve calculates the inverse and stores it in the cache, or 
## else retrives the cached value.

##======================================================================
## makeCacheMatrix () takes a square matrix x as input
## and returns a special matrix which can caches its inverse.
## Methods $get() and $set(x) can be used to read and write the
## matrix while correctly updating the cached mean


makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL  ## Initially no inverse is cached
  
  set <- function(y) { ## sets a new value of the matrix
    x <<- y
    inv <<- NULL      ## invalidate any cached inverse
  }
  
  get <- function() x  ## returns the matrix contents
  
  ##setinv, getinv accesses the inv field in the special matrix
  setinv <- function(inverse) inv <<- inverse               
  getinv <- function() inv  
 
  ## return the methods used to access data in the special matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##===========================================================================
## cacheSolve() is called in order to access the inverse of a special matrix
## if there is no cached inverse, then one is calculated, stored and returned
## if a cached inverse is available, the cached value is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of special matrix 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {  ## cached inverse available so return it
    message("getting cached data")

  } else {              ## no cached inverse, so..
  data <- x$get()       ## retieve matrix data
  inv <- solve(data, ...)  ## calculate inverse
  x$setinv(inv)         ## store back in the special matrix
  }
  inv  ## return the inverse
}

## Test Routines for cacheSolve and makeCacheMatrix
## Call cacheTest() to run a test script

cacheTest <- function () {
  
  testmatrix <- rnorm(100)
  dim(testmatrix) <- c(10,10)
  print(c("Here is the initial matrix ", testmatrix))
  
  t1 <- solve(testmatrix)
  print(c("Here is its inverse " , t1 ))
  
  testcache <- makeCacheMatrix(testmatrix)
  print(c("Here is the matrix stored as a special matrix", testcache$get()))
  
  t2 <- cacheSolve(testcache)
  if (identical(t1,t2)) {
    print("Inverse correctly Cached ")
  } else {
    print("Inverse INCORRECT ")
  }
  t3 <- cacheSolve(testcache)
  if (identical(t1,t3)) {
    print("Inverse correctly retrieved from cache ")
  } else {
    print("Inverse INCORRECTLY retrieved from cache ")
  }
}