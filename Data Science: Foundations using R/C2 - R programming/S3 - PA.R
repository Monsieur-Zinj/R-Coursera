## Put comments here that give an overall description of what your
## functions do

## Theses functions shows how we can create "object-like" in R
## and storing variables if we don't want to compute them multiple times.



## Write a short comment describing this function

## makeCacheMatrix create an "object" with some "atributes" ( inv ) 
## and "methods" ( set, get, setinv, getinv)


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## Write a short comment describing this function

## take a "matrix object" as input, return the inverse of this matrix as output
## store the inverse at first call

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinv(inv)
  inv
  
}

