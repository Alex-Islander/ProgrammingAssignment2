## Put comments here that give an overall description of what your
## functions do

## Function creates list of functions to access and mutate input data

makeCacheMatrix <- function(x = matrix()) {
  result <- NULL #assigns "result" variable within makeCacheMatrix environment to null
  set <- function(y=matrix()) {
    x <<- y
    result <<- NULL
  } #function assigns values to objects "x" and "result" in parent environment 
  get <- function() x #returns "x" value
  setinverse <- function(inverse) result <<- inverse #assigns value to "result" object in parent environment
  getinverse <- function() result #returns ""result" value
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse) #creates list of functions
}


## Function checks if no result is stored in cache and calculates inverse matrix

cacheSolve <- function(x, ...) {
  result <- x$getinverse()
  if(!is.null(result)) {
    message("getting cached data")
    return(result)
  }
  data <- x$get()
  result <- solve(data, ...)
  x$setinverse(result)
  result
}