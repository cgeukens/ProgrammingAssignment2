## makeCacheMatrix is a function that stores a list of functions in order to cache the inverse of a matrix.
## the functions are:
## set: funcion in order to change the matrix in case a different matrix is called
## get: function that returns the called matrix
## setsolve: function that stores the inverse of the called matrix in the cache (object m)
## getsolve: function that get the inverse of the matrix from the cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve is a function that chechs whether or not there is already data in the cache (object m) 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve() ## calls the object from the cache which is  where the inverse is stored by the function getsolve and stores it in object m 
  if(!is.null(m)) {   ## In case there is an inverse stored in the cache, the value is shown
    message("getting cached data")
    return(m)
  }
  data <- x$get()  ## in case there isn't already an inverse available in the cache, the 'input' matrix is stored in the object 'data' by calling the 'get'-function
  m <- solve(data, ...) ## the inverse of the matrix is caculated and stored in object m
  x$setsolve(m) ## object m is stored in the cache
  m
}



