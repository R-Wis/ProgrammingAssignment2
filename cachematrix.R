## Functions calculating inverse of a matrix-----------------------------------


## This function defines 3 other functions to call from the second one.
## get pass a matrix to cacheSolve 
## setinverse put a inverse of matrix to variable inv
## getinverse pass inverse to second function

makeCacheMatrix <- function(x = matrix()) 
     {
               inv <- NULL
               get <- function() x
               setinverse <- function(inverse)  inv <<- inverse
               getinverse <- function()  inv
               list(get=get,setinverse=setinverse,getinverse=getinverse)
     }


## This function checks if inverse for a matrix exist and if yes it gets it from
## a cache printing a msg, if not it solves matrix set in cache and return

cacheSolve <- function(x, ...) 
     {
          if(!is.null(x$getinverse()))
          {
               message("Getting Cached Inverse Matrix")
               x$getinverse()
          } else 
          {
               inverse <- solve(x$get())
               x$setinverse(inverse)
               inverse
          }
     }
