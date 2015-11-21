## makeCacheMatrix and cacheSolve will be used to prevent tedious computation
## of getting a matrix's inverse.

## makeCacheMatrix will set and get the value of a "matrix" and set and get the value
## of its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m<<-solve
  getmatrix <- function() m
  list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}

## cacheSolve will calculate the inverse of the special matrix
## created in makeCacheMatrix IF it has not already been calculated. 

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
        ## Return a matrix that is the inverse of 'x'
  data <- x$get()
  m <- solve(data)
  x$setmatrix(m)
  m
}
