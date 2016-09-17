## Functions often require time-consuming computations.  Caching the result of a calcuation rather than
##computing the function each time improves efficiency since it could decrease the overall run time. 
#For example, matrix inversion is usually an expensive computation.   There may be a benefit to caching the inverse 
##of a matrix rather than computing it repeatedly. 

#This Programming assignment will illustrate scoping rules of the R language and how they 
#can be manipulated to preserve the state inside of an R object and save computation effort until 
#R has cached the function result.

## This function creates a matrix and signals R to look elsewhere (outside the current environment or curly brackets) 
##for further calculations. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {

    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    
    x <<- y
    m <<- NULL
  }
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse
  
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##The following function calculates the inverse of the matrix created with 
#the above function. However, it first checks to see if the inverse has already been 
#calculated. If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the 
#cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' using solve() (concept based on function cachemean.R 
        ## which was provided in this assignment).  
  m <- x$getsolve() #To try to reinforce the concept and code from this assignment, I ran cacheSolve in R Studio as follows:  
                    #>makeCacheMatrix<-matrix(c(2, 4, 3, 1, 5, 7),nrow=3,ncol=2) 
                    #cacheSolve(makeCacheMatrix)
                    #and an Error message said:  Error in x$getsolve : $ operator is invalid for atomic vectors
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setsolve(m)
  m
}


