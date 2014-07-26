## These functions provide a mechanism for returning the inverse of a given matrix.  If that inverse has
## already been calculated, the cached calculated inverse of the matrix will be returned.

## This function just contains a list of sub-functions to get and set the matrix and get and set the inverse 
## of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  x_inverse <- NULL
  
  set <- function (y) {
    x <<- y
    x_inverse <- NULL
  }
  
  get <- function() x
  
  set_inverse <- function (new_inverse) x_inverse <<- new_inverse
  
  get_inverse <- function () x_inverse
  
## This is the return value: essentially a list of the functions.  
  
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)

}


## This function returns a matrix that is the inverse of x using the cached copy of that inverse if it has been set.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  x_inverse <- x$get_inverse()
  if (!is.null(x_inverse)) {
    message("Getting cached data")
    return(x_inverse)
  }
  data <- x$get()
  x_inverse <- solve(x$get())
  x$set_inverse(x_inverse)
  x_inverse
}
