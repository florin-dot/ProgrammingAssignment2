## on the makeCacheMatrix; we assume that the matrix we supply here is invertible; we set the value of the matrix using another function
## we use the << sign,  a closer is a function written by another function; closers allow us to have 2 levels of parameters, one that controls how the function works and the other which does all the hard work.

## we then set and get the value of the matrix, and set the value of the inverse and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
 j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## we check if the inverse has already been calculated and if so we can get the inverse from the cache( a message with getting cached data will be displayed)

cacheSolve <- function(x, ...) {
   j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j      
}
