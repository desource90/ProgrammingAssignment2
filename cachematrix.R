##   The two functions defined below are used to cache computations of 
## the inverse of a matrix. The 1st function makeCacheMatrix function
## sets and stores the value of inverse for a matrix. The 2nd function 
## searches for the inverse if computed, else it computes the inverse.
      

CacheMatrix <- function(x = matrix()) {
# initializing the inverse variable 'inverse' to NULL
      inverse <- NULL
      set <- function(y){
      # this function is used to store the value of matrix in a different environment  
            x <<- y
            inverse <<- NULL
                    }
      get <- function() x
      # This function is used to obtain the value of the matrix

      setinverse <- function(solve) inverse <<- solve
      #   The setinverse function uses the R function 'solve' to get 
      # the inverse of the matrix x
  
      getinverse <- function() inverse
      #   The getinverse function is used to obtain the value of the inverse function
      # of the matrix
      
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
      }      
      

##   This function searches for the inverse of the matrix. If the inverse is found,
## then the value is retreived, otherwise it is computed and stored.
      

cacheSolve <- function(x, ...){
      inverse <- x$getinverse() 
      # searching the inverse of the function in different environment
            if (!is.null(inverse)){
                  message("getting cached data")
                  # if the inverse of the function is found then the inverse value will be printed
                  inverse
            }

      data <- x$get()
      # computing the inverse of the matrix using the solve function
      inverse <- solve(data, ...)
      # storing the inverse of the matrix
      x$setinverse(inverse)
      inverse }



      
