##  To write a pair of functions that cache the inverse of a matrix


## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 
## inv<- denotes inverse Matrix; m<-matrix;       
      
      inv <- NULL
      set <- function(m) {
           x <<- m
           inv <<- NULL
      }
  
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      
      ## returning the variables- set and get & function setInverse and getInverse
      list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)

}


## cacheSolve: function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x) {
          
          inv <- x$getInverse()
          
          ##Checking inverse has already been calculated or not.. 
          if(!is.null(inv)) {
                message("getting cached data")
                return(inv) ## retriving the inverse from the cache
          }
  
          data <- x$get()
          
          ## calculating the inverse of matrix by sove() function in R
          inv <- solve(data)
          x$setInverse(inv)
          inv ## returning the Inverse of matrix..

}
