## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" that set the value of the matrix, get the value of the matrix,
## set the value of the inverse, and finally, get the value of the inverse
makeCacheMatrix <- function(x = matrix()){
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) m<<-inverse
  getinverse<-function()m
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
  
}


## The following fucntion calculates the inverse of the special "matrix" created from the function above. It first check
## whether the inverse has already been calcualted. If yes, it gets the inverse from the cache and stop the computation. 
## or else, it calculates the inverse of the data and set the value of the inverse in the cache via setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinverse(m)
  m
}

