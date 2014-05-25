## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  # inv will store the cached inverse matrix
   s <- NULL  
  
  # Setter for the matrix
  set <- function(y) { 
    x <<- y
    s <<- NULL
  }
  
  # Getter for the matrix
  get <- function() x
  
  # Setter for the inverse
  setsolve <- function(solve) s <<- solve
  
  # Getter for the inverse  
  getsolve <- function() s
  
  # Return the matrix with our newly defined functions  
  list(set = set, get = get,setsolve = setsolve,getsolve = getsolve)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'

  s <- x$getsolve()
  
  # If the inverse is already calculated, return it
  if(!is.null(s)) {
    message("getting cached data...")
    return(s)
  }
  
  # The inverse is not yet calculated, so we calculate it
   data <- x$get()
   s <- solve(data, ...)
  
  # Cache the inverse
  x$setsolve(s)
  
  # Return it
  s

}
