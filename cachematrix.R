# High-level objective : To write an R function whcih is able to cache time-consuming computations
# such as matrix inversion. However, it may happen that such an operation may have been repeatedly computed.
# Then, rather than recomputing an inverse for that matrix, the function should be capable to caching 
# the previous matrix inversion result and return it. 

# For this purpose, we make use of scoping rules defined in R. The task is divided into two tasks: 
# 1) creating a cached matrix, and 2) definition and implementation of requisite functions shuch as 
# get Matrix()
# setMatrix()

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  # Set the matrix by using solve function (solve(): inverse of matrix)
  setmatrix<-function(solve) m<<- solve                 
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

# The following function calculates inverse of the matrix, set by makeCacheMatrix function.
# However, this function checks if inverse has already been computed for the given matrix. 
# If yes, it returns the inverse matrix from the cached result. Otherwise it computes inverse 
# and caches again using setMatrix function.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}

