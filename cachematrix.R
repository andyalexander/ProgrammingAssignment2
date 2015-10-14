## Methods to hold a cached matrix, and a function to solve this pseudo matrix object to determine
## its inverse

## Create a matrix object holding a cached inverse, and exposing getter and setter methods

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  # setter method - uses the <<- operator to find the value of x in the parent environment
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # getter method simply returns x, given the lexical scoping rules, finds x where it was defined, which was
  # in the object constructor
  get <- function() x
  
  # functions to get and set the inverse
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  # Return a list of function pointers 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## Compute the inverse of the above pseudo-matrix object if and only if it doesn't exist already
## if it exists, return from cache
cacheSolve <- function(x) {
  # get the inverse from the matrix object and return it if the inverse already exists
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # get the 'input' matrix from the object and compute the inverse, and subsequently return to calling function
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
