
## make matrix inverse cached in the function.
## Write a short comment describing this function
## cache the matrix
##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      cachematrix <- NULL
      set<- function(y){
            x<<-y
            cachematrix<<-NULL
      }
      get <- function() x
      setinverse<-function(inverse) cachematrix <<- inverse
      getinverse<- function() cachematrix
      return (list(set = set, get= get,  serinverse =serinverse, getinverse= getinverse))
}


## Write a short comment describing this function

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      cachematrix <-x$getmean()
      if (!is.null(cachematrix)){
            message("getting cashed data")
            return (cachematrix)
      }
      data <- x$get()
      cachematrix<-solve(data,...)
      cachematrix<-setmean(cachematrix)
      cachematrix
}
