makeCacheMatrix <- function(x = numeric()) {
      
      # initially nothing is cached, so the value of cache is NULL. 
      # In cacheInverse function, we assign the value of Inverse to the cache
      cache <- NULL
      
      # 
      setMatrix <- function(newValue) {
            x <<- newValue
            # since the matrix is assigned a new value, flush the cache
            cache <<- NULL
      }
      
      # returns the stored matrix
      getMatrix <- function() {
            x
      }
      
      # cache the given argument 
      cacheInverse <- function(solve) {
            cache <<- solve
      }
      
      # get the cached value
      getInverse <- function() {
            cache
      }
      
      # return a list. Each named element of the list is a function
      list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


cacheSolve <- function(y, ...) {
      # get the cached value
      inverse <- y$getInverse()
      
      # if a cached value exists return it
      #EXECUTED ONLY WHEN CACHE <> NULL
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      # otherwise get the matrix, caclulate the inverse and store it in
      # the cache
      #EXECUTED ONLY WHEN CACHE =NULL
      else
      {
            data <- y$getMatrix()
            inverse <- solve(data)
            y$cacheInverse(inverse)
            
            # return the inverse
            inverse   
      }
      
}
