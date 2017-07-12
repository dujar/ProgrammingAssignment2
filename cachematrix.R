## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This functions aims to make it possible to cache a matrix of any dimesion

makeCacheMatrix <- function(x = matrix()) {
    Inverse <- NULL
    set <- function(y) {
      x <<- y #assign y to x and save the the variable x for future calls
      Inverse <<- NULL
    }
    get <- function() x
    setinv <- function(inv) Inverse <<- inv
    getinv <- function() Inverse
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }




## Write a short comment describing this function
## This function supposedly fetches the inverse if it's cached, otherwsie it computes it with "solve" function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'makeCacheMatrix <- function(x, ...) {
  Inverse <- x$getinv() 
  if(!is.null(Inverse)) {
    message("getting cached data")
    return(Inverse)
  }
  mat <- x$get() #gets the matrix from the 'x'makeCacheMatrix
  Inverse <- solve(mat)
  x$setinv(Inverse)
  Inverse
}



