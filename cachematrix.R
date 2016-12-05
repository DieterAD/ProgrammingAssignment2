## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix and cacheSolve enable to calculate the inverse of a matrix
# the result is saved in the environment of the function "makeCacheMatrix" and 
# it can be accessed by the function cacheSolve. when chacheSolve is first called on 
# a new function, it checks if the inverse_ has already been calculated, if yes, it 
# prints this cached calculation, if not it calls the setinv function from makeCacheMatrix
# to calculate the inverse of the matrix and displays it. 
# if a new matrix is created using makeCacheMatrix, the variable inverse_ is set to zero in the environemnt
# of the function. 

makeCacheMatrix <- function(x = matrix()) {

  #set inv to NULL
  inverse_ <- NULL
  # set the value of the vector
  #get x into the function, reset inv to NULL
  set <- function(y) {
    x <<- y
    inverse_ <<- NULL
  }
  # get the value of the vector
  get <-function() x
  # set the value of the inverse
  setinv <- function(solve) inverse_ <<- solve
  # get the value of the inverse
  getinv <- function() inverse_
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)
}




## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  #calculate inverse using the function getinv from above
  inverse_ <- x$getinv()
  #if its already there (notzero), then get cached value
  if(!is.null(inverse_)) {
    message("getting cached data")
    return(inverse_)
  }
  data <- x$get()
  inverse_ <- solve(data, ...)
  x$setinv(inverse_)
  inverse_
}
