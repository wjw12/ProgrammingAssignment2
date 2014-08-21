## The functions below calculate the inverse of a matrix and store 
## it in the cache.

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(inv) {
            i <<- inv
      }
      get <- function() x
      getinverse<- function() i
      list (set=set,get=get,getinverse=getinverse)

}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if (!is.null(inv)) {
            message ("Getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data,...)
      x$set(inv)
      inv
}
