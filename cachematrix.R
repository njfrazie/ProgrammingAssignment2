## Put comments here that give an overall description of what your
## functions do



##The following function "makeCacheMatrix" creates a list of functions that can do the following
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inversion
## 4.  get the value of the inversion
## When used in conjunction with CacheSolve, an inverted matrix can be stored and retrieved quickly

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   
   set <- function(y) {
     x <<- y
     inv <<- NULL
   }
   
   get <- function() x
   
   setinv <- function(inverse) {
      inv <<- inverse   
   }
   
   getinv <- function() inv
   
   list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
  
}


## Write a short comment describing this function

#CacheSolve uses functions that are specially stored in a list created by makeCacheMatrix.  The following function will check if an inverted
#matrix is created and report its value.  If its not, it will calculate it first then report it.

cacheSolve <- function(x, ...) {

   inv <- x$getinv()
  
   if(!is.null(inv)) {
     message("Getting Cached Data")
     return(inv)
   }
  
   data <- x$get()
   inv <- solve(data)
   x$setinv(inv)
   inv  
  
}  
