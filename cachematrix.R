## These two functions create a special matrix object with the capability to
## cache the inverse of the matrix. By caching the inverse, R does not have
## to recalculate the time-consuming inverse calculation


## makeCacheMatrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      s<- NULL
      set <- function(y) {
            x<<-y
            s<<-NULL
      }
      get <- function () x
      setinverse<-function(solve) s<<-solve
      getinverse<-function() s
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## CacheSolve computes the inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      s<-x$getinverse()
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      data<-x$get()
      s<-solve(data,...)
      x$setinverse(s)
      s
}
