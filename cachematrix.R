##
## A pair of functions that cache the inverse of a matrix.
## 
## The function makeCacheMatrix creates a special "matrix" object that can cache
## its inverse. 
##
## This function is really a list containing functions to:
##
##  1. set the value of the matrix.
##  2. get the value of the matrix.
##  3. set the cached value of the inverse of the matrix.
##  4. get the cached value of the inverse of the matrix.
##
makeCacheMatrix <- function(m_x = matrix()) {
  ##
  ## Initialize the cached value for the inverse.
  ##
  m_inv <- NULL
  ##
  ## Define the 4 functions.
  ##
  ##   1. Set the value of the matrix, m_x; reinitialize the cached value of the 
  ##   inverse, m_inv.
  ##
  f_set <- function(p_x) {
    m_x <<- p_x ## Set the values of the matrix, m_x.
    m_inv <<- NULL ## Invalidate the cached value of the inverse, m_inv.
  }
  ##
  ##   2. Get the value of the matrix, m_x.
  ##
  f_get <- function() {m_x}
  ##
  ##   3. Set the value of the cached inverse, m_inv.
  ##
  f_set_inverse <- function(p_inv) {m_inv <<- p_inv}
  ##
  ##   4. Get the value of the cached inverse, m_inv.
  ##
  f_get_inverse <- function() {m_inv}
  ##
  ## Package the above four functions to return as a list.
  ##
  list(
    "set" = f_set, 
    "get" = f_get,
    "set_inverse" = f_set_inverse,
    "get_inverse" = f_get_inverse
    )
}
##
## The following cacheSolve function calculates the inverse of the special 
## matrix "object" created with the makeCacheMatrix function above. However, it 
## first checks to see if the inverse has already been calculated. If so, it 
## gets the inverse from the cache and skips the computation. Otherwise, it 
## calculates the inverse of the data and sets the value of the inverse in the 
## cache via the set_inverse function.
##
cacheSolve <- function(x, ...) {
  ##
  ## Return a matrix that is the inverse of 'x'.
  ## Note: x is expected to be a special "vector" created by the function 
  ## makeCacheMatrix.
  ##
  m_inverse <- x$get_inverse()
  if(!is.null(m_inverse)) {
    message("getting cached data")
    return(m_inverse)
  }
  data <- x$get()
  m_inverse <- solve(data, ...) ## Find the inverse of the matrix.
  x$set_inverse(m_inverse)
  m_inverse
}
##
## Demonstration of ussage.
##
m1 <- matrix(c(1,2,3,4), c(2,2)) ## Create a 2x2 matrix.
c1 <- makeCacheMatrix(m1)        ## Create the spacial "vector."
print(c1$get_inverse())          ## Initially the cached inverse is unset.
cacheSolve(c1)                   ## Calculate and set the cached inverse.
print(c1$get_inverse())          ## Fetch the cached inverse which is now set.