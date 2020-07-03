## makeCacheMatrix function creates a special matrix
## setmat sets value of special matrix to the argument passed and inv to null
## getmat gives the matrix
## setinv sets value of inv to argument passed
## getinv gives the inverse of marix

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  setmat<-function(y)
  {
    x<<-y
    inv<<-NULL
  }
  getmat<-function() x
  setinv<-function(inverse) inv<<-inverse
  getinv<-function() inv
  list(setmat = setmat, getmat = getmat,
       setinv = setinv,
       getinv = getinv)
  
}


## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if(!is.null(inv))
  {
    message("Getting cached data")
    return(inv)
  }
  mat<-x$getmat()
  inv<-solve(mat,...)  ## computes inverse of matrix
  x$setinv(inv)
  inv
  
}
