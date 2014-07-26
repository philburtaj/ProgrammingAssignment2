## These functions provide a mechanism for returning the inverse of a given matrix.  If that inverse has
## already been calculated, the cached calculated inverse of the matrix will be returned.

## This function just returns a list of functions to get and set the input matrix and get and set the inverse 
## of the input matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  # First we set x_inverse to NULL.
  x_inverse <- NULL
  
  # The set function sets the x variable in the parent frame to the new matrix, and the x_inverse
  # variable in the parent frame to NULL.  Within this function, x and x_inverse are free variables.
  set <- function (y) {
    x <<- y
    x_inverse <<- NULL
  }
  
  # The get function just returns the currently assigned matrix.
  get <- function() x
  
  # The set_inverse function sets the x_inverse variable in the parent frame to the new_inverse matrix that
  # is passed to the function.  Within this function, x_inverse is a free variable.
  set_inverse <- function (new_inverse) x_inverse <<- new_inverse
  
  # The get_inverse function returns the currently set x_inverse matrix.  This will be NULL until someone
  # calls the cacheSolve function to set calculate and set the value.
  get_inverse <- function () x_inverse
  
  # This is the return value for the makeCacheMatrix function: the return value is essentially 
  # a list of the functions.
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)

}


## This function returns a matrix that is the inverse of x using the cached copy of that inverse if it has been set.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # First we set the local variable x_inverse to the currently set inverse in the list passed as an argument to
  # this fucntion.
  x_inverse <- x$get_inverse()
  
  # Then we check whether the currently set x_inverse is NULL, or if it has already been calculated.  If already
  # calculated, then we just pass it back as the return value.
  if (!is.null(x_inverse)) {
    message("Getting cached data")
    return(x_inverse)
  }
  
  # If the inverse has not been calculated, then get the current input matrix, solve for the inverse,
  # then call the set function in the input list and return the inverse matrix.
  data <- x$get()
  x_inverse <- solve(x$get())
  x$set_inverse(x_inverse)
  x_inverse

}
