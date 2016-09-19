## The functions below use caching to save computation time

## This function creates the matrix and stores in the cache

makeCacheMatrix <- function(x = matrix()) {
inv<-NULL  #caches matrix object inv
set<-function(y) {
x<<-y  #assigns a value to the matrix object in an enviroment different from the current environment 
inv<<-NULL
}
get<-function() x
setInverse<-function(solve) inv<<-solve
getInverse<-function() inv   #retrieves the inverse of the matrix
list(set=set, get=get,        #returns the function
setInverse=setInverse,
getInverse=getInverse)
}

##cacheSolve uses the matrix created above and takes the inverse.  The inverse matrix is 
##stored in the cache.  

cacheSolve <- function(x=matrix(),...) {
  inv<-x$getInverse()  #retrieves value stored in cached matrix and 
  #if the inverse already computed and skips the calculation
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #calculation of the inverse is here
  OrigMat<-x$get()  #retrieve original stored matrix
  inv<- solve(OrigMat,...)
  x$setInverse(inv)
  inv   #returns inverse of matrix x
}
