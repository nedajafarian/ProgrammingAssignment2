## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ##initialize the matrix m
  m <- NULL
  
  ##define setmatrix
  
  setmatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ##define function getmatrix
  getmatrix <- function() x
  
  ##define function setcache
  setcache <- function(inverse) m <<- inverse
  
  #define function getcache
  getcache <- function() m
  
  ##list name of methods
  list(setmatrix = setmatrix,
       getmatrix = getmatrix,
       setcache = setcache,
       getcashe = getcache)
}

##function cacheSolve, return the inverse of a given matrix m
cachesolve <- function(x, ...) {
  
  m <- x$getcache()
  
  ## if not NULL then return m
  if (!is.null(m)) {
    message("load cache matrix")
    return(m)
  }
  
  ##if empty then use the cache matrix m
  else {
    dmatrix <- x$getmatrix()
    m <- solve(matrix,...)
    x$setcache(m)
    m
  }
  
  
}


## Example of this week
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set,
       get = get,
       setmean = setmean,
       getmean = getmean)
}

cachesolve <- function(x, ...) {
  m <- x$getmean()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}


