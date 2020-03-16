##The two functions used are makeCacheMatrix( ) and cacheSolve( ).

## The makeCacheMatrix( ) initializes a matrix and creates a list 
##containing setting, getting the matrix; setting, getting the inverse of the 
##matrix.cacheSolve( ) calculates and caches or retrieves inverse of 
##the matrix initiated in makeCacheMatrix( ) if already calculated.

##makeCacheMatrix ( ) does the following
##creates a list containing a function to

##set the value of the matrix
##get the value of the matrix
##set the value of the inverse of the matrix
##get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, 
       getinverse = getinverse )

}


## cacheSolve( ) returns the calculated cached result if the 
##argument is not changed.If the inverse is calculated for the first
##time, returns the results and caches it.


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
