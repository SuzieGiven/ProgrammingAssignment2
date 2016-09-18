## The functions below use caching to save computation time

## This function creates the matrix and stores in the cache

makeCacheMatrix <- function(x = matrix()) {
mInv <- NULL  #caches matrix mInv
  set <- function(y) {

    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    
    x <<- y   
    mInv <<- NULL
  }
  
  get <- function() x
  setmInv <- function(MIO) mInv <<- solve  #stores the inverse of the matrix
  getmInv <- function() mInv                 #retrieves the inverse of the matrix
  list(set = set, get = get,                 #returns the function
       setmInv = setmInv,
       getmInv = getmInv)
}
}


##cacheSolve uses the matrix created above and takes the inverse.  The inverse matrix is 
##stored in the cache.  

cacheSolve <- function(x, ...) {
  mRet <- x$getmInv  #retrieves value stored in cached matrix
  if(!is.null(mRet)) {
    message("getting cached data")
    return(mRet)
  }
  mMat <- x$get() #retrieve original stored matrix
  mRet <- solve(mMat, ...)
  x$setsolve(mRet)
  mRet   #returns inverse of matrix x
}
