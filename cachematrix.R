## As mentioned in Programming Assignment 2 of The Coursera R Programming
## course, matrix inversion is (especially for larger matrices) a costly
## operation which may benefit from results caching. Therefore, cachematrix.R
## provides two functions that can be used for cached matrix inversion.


## makeCacheMatrix creates a special "matrix" object containing a list with
## four funtions to
## 1. setMatrix: set the value of the matrix
## 2. getMatrix: get the value of the matrix
## 3. setInverse: set the value of the inverse
## 4. getInverse: get the value of the inverse
makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  
  setMatrix <- function(m) {
    mat <<- m
    inv <<- NULL
  }
  
  getMatrix <- function() {
    mat
  }
  
  setInverse <- function(i) {
    inv <<- i
  }
  
  getInverse <- function() {
    inv
  }
  
  list(
    setMatrix=setMatrix
    , getMatrix=getMatrix
    , setInverse=setInverse
    , getInverse=getInverse
  )
}


## This function returns the inverse of a cached matrix getated using
## makeCacheMatrix(). If the inverse has not been computed (and stored) yet,
## solve() is used to compute the inverse, the result cached, and returned as
## result. If a computed result exists, it is directly returned without
## computation.
## The function assumes that inversion is always possible,
cacheSolve <- function(m, ...) {
  inv <- m$getInverse()
  if (is.null(inv)) {
    ## not stored, compute and save
    message("computing inverse")
    inv <- solve(m$getMatrix())
    m$setInverse(inv)
    return(inv)
  } else {
    ## stored, no computation needed
    message("getting cached data")
    return(inv)
  }
}


## Simple test case. Note that invertibility of matrix is not enforced.
## Yet, a random matrix is invertible with probability 1.
test <- matrix(runif(1000000), nrow=1000, ncol=1000)
system.time(solve(test))
#User      System verstrichen 
#1.74        0.02        1.78
ctest <- makeCacheMatrix(test)
system.time(cacheSolve(ctest))
#computing inverse
#User      System verstrichen 
#1.81        0.03        1.92
system.time(cacheSolve(ctest))
#getting cached data
#User      System verstrichen 
#0           0           0 