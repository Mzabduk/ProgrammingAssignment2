## Inversion of a matrix may take a lot of time
## so these two functions allow to re-use matrix inversion result, once it has been created

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
       inv <- NULL
       set <- function(y) {
             x <<- y
           inv <<- NULL
        }
       get <- function() x
       setinv = function(inverse) inv <<- inverse 
       getinv = function() inv
       list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  if (!is.null(inv)){
             message("Cached data:")
             return(inv)
         }
       
         mat.data = x$get()
         inv = solve(mat.data, ...)
         
          x$setinv(inv)
         
          return(inv)
}



