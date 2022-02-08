
makeCacheMatrix <- function(a = matrix()) {
       i <- NULL
       set <- function(matrix) {
        a <<- matrix
        i <<- NULL
       }
       
  get <- function() {
    a
  }
  
  setInverse <- function(inverse) {
    
    i <<- inverse
  }
    
  getInverse <- function() {
    i
  }
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  a <- x$getInverse()
  if(!is.null(a)) {
    message("getting the final result of cached data")
    return(a)
  }
  
  data <- x$get()
  a <- solve(data) %*% data
  x$setInverse(a)
  a
}
