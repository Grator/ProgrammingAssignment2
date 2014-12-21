# this function contains 4 member functions: set, get, setInv

  makeCacheMatrix <- function(x = matrix()) {

      xinv <- NULL 
      set <- function(y) {
	  x <<- y
	  xinv <<- NULL # it also initialises xinv to null
      }

      get <- function() x # return the input matrix
      setInv <- function(inv) xinv <<- inv # set the inversed matrix
      getInv <- function() xinv # return the inversed matrix
      list(set = set, get = get,
	       setInv = setInv,
	       getInv = getInv)
  }

  cacheSolve <- function(x, ...) {
      m <- x$getInv() # inversed matrix from object x
      # it will be NULL if not calculated
      if(!is.null(m)) { # if the inversion result is there
	  message("getting cached data")
	  return(m) # return the calculated inversion
      }
      data <- x$get() # if not, we do x$get to get the matrix object
      m <- solve(data) # solve it
      x$setInv(m) # we then set
      m # return the result
  }

  
