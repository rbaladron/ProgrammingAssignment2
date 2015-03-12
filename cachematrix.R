#makeCacheMatrix creates a special "matrix", which is really a list containing a function to

#set the value of the matrix
#get the value of the matrix
#setInversa the value of the inverse matrix
#getInversa the value of the inverse matrix 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  xInversa <- NULL
  set <- function(y) {
    x <<- y
    xInversa <<- NULL
  }
  get <- function() x
  setInversa <- function(inversa) xInversa <<- inversa
  getInversa <- function() xInversa
  list (set = set, get = get,
        setInversa = setInversa,
        getInversa = getInversa)

}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  m <- x$getInversa()
  if(!is.null(m)) { # if the inversion result is there
    message("getting cached data")
    return(m)
  }
  data <- x$get() 
  m <- solve(data)
  x$setInversa(m) 
  m 
}
