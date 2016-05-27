## makeCacheMatrix set a matrix, the matrix can also be set with the fonction set
## The invert matrix can be set with the call setinvers (ex: mat$setinvers())
## The invert matrix can ge access (if it already exist) by the call $getinvers()
## The function return a list of function  so we can apply a function to an object using'$'
## Example : a_matrix$getinvers

makeCacheMatrix <- function(x=matrix()) {
  invert <- NULL
  set <- function(y){
    x <<- y
    invert <<- NULL
  }
  get <- function() x
  setinvers <- function() invert <<- solve(x)
  getinvers <- function() invert
  list(set=set,get=get,setinvers=setinvers,getinvers=getinvers)
}

# Passing a matrix as a parameter of the cacheSolve function will return the invert
# matrix if it already exist. If not, the invert matrix is calculate and return

cacheSolve <- function(x, ...) {
  invert <- x$getinvers()
  # Check if invert matrix already exist
  if(!is.null(invert)) {
    return(invert)
  }
  # Calculate the invert matrix and return it
  data <- x$get()
  invert <- solve(data)
  x$setinvers(invert)
  invert
}
