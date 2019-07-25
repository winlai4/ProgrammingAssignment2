## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL             ## set m as NULL to hold value of matrix inverse
  set <- function(y) {  ## defining set function to assign as 'new'  
    x <<- y             ## assigning y to matrix (x)  
    m <<- NULL          ## if there is a new matrix, reset inv to NULL
  }
  get <- function() x   ## define the get fucntion - returns value of the matrix argument

  setinverse <- function(inverse) m <<- inverse  ## assigns value of m in parent environment
  getinverse <- function() m                     ## gets the value of m when called
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## This function computes the inverser of the special "matrix" created from the function above if the inverse has already been calculated then the fucntin will retrieve the inverser from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
    
}
