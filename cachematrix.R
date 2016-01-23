## makeCacheMatrix creates and returns a list of functions
## used by cacheSolve to get or set the inverted matrix in cache
makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  get <- function() x
  setMatrix <- function(inverse) cache <<- inverse
  getInverse <- function() cache
  
  list(set = set, get = get,
       setMatrix = setMatrix,
       getInverse = getInverse)
}


## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix
## If the inverted matrix does not exist in cache,
## it it created in the working environment and it's inverted value
cacheSolve <- function(x, ...) {
  cache <- x$getInverse()

  if (!is.null(cache)) {
    message("getting cached data")
    
    return(cache)
  }
  
  matrix <- x$get()
  
  tryCatch( {
    cache <- solve(matrix, ...)
  },
  
  error = function(e) {
    message("Error:")
    message(e)
    return(NA)
  },
  
  warning = function(e) {
    message("Warning:")
    message(e)
    return(NA)
  },
  
  finally = {
    x$setMatrix(cache)
  } )
  
  return (cache)
}
