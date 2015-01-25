## The two fucntions that are given here create a matrix and also calcualte the inverse of it.  
## Matrix inversion is a costly computation and caching the inverse of a matrix rather than compute it repeatedly saves time.
## The two functions are 
## MakeCacheMatrix
## CacheSolve

## The makecacheMatrix takes in a matrix and creates a special vector which is a list of functions that 
## 1.Set the matrix
## 2.Get the matrix
## 3.Set the inverse of the matrix   
## 4.Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(new){
    x<<-new
    i<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) i<<- solve
  getmatrix<-function() i
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## The Cachesolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated ,
## then the cachesolve would retrieve the inverse from the cache.

## cacheSolve function returns a matrix that is the inverse of 'x'

cacheSolve <- function(x=matrix(), ...) {
  i<-x$getmatrix()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  matrix<-x$get()
  i<-solve(matrix, ...)
  x$setmatrix(i)
  i
}
