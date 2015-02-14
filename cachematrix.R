#The first function, makeVector creates a special "vector", which is really a list containing a function to

# set the value of the vector
# get the value of the vector
# set the value of the inverse of the matrix 
# get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  set_Matrix<-function(solve) m<<- solve
  get_Matrix<-function() m
  list(set=set, get=get,
       set_Matrix=set_Matrix,
       get_Matrix=get_Matrix)
}

# The following function returns the inverse of the matrix. It Checks the cache to see if the value is there.
# If it is there it returns from cache. If not ; it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$get_Matrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$set_Matrix(m)
  m
}

## Sample run
## > x<- matrix(c(4,3,3,2), nrow=2 , ncol=2)
## > source("cachematrix.R")
## > m = makeCacheMatrix(x)
## > m$get()
## [,1] [,2]
## [1,]    4    3
## [2,]    3    2
## > cacheSolve(m)
## [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4
## > cacheSolve(m)
## getting cached data
## [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4