makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function() m <<- solve(x)
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

cachesolve <- function(x, ...) {
  m <- getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  m <- solve(x, ...)
  m
}
