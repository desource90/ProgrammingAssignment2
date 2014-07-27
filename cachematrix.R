##   The two functions defined below are used to cache computations of 
## the inverse of a matrix. The 1st function makeCacheMatrix function
## sets and stores the value of inverse for a matrix. The 2nd function 
## searches for the inverse if computed, else it computes the inverse.
      

makeCacheMatrix <- function(x = matrix()) {
# initializing the inverse variable 'inv' to NULL
      inv <- NULL
      set <- function(y){
      # this function is used to store the value of matrix in a different environment  
            x <<- y
            inv <<- NULL
                    }
      get <- function() x
      # This function is used to obtain the value of the matrix

      setinv <- function(solve) inv <<- solve
      #   The setinverse function uses the inbuilt function 'solve' to determine 
      # the inverse of the matrix x
  
      getinv <- function() inv
      #   The getinverse function is used to obtain the value of the inverse function
      # of the matrix
      
      list(set = set, get = get, setinv = setinv, getinv = getinv)
      }      
      

##   This function searches for the inverse of the matrix. If the inverse is found,
## then the value is pulled out from the memory, else it is computed and stored in
## the memory
      

cacheSolve <- function(x, ...){
      inv <- x$getinv() 
      # searching the inverse of the function in different environment
            if (!is.null(inv)){
                  message("getting cached data")
                  # if the inverse of the function is found then the inverse value will be printed   
                  # from the memory
                  return(inv)
            }

      data <- x$get()
      # computing the inverse of the matrix using the solve function
      inv <- solve(data,..)
      # storing the inverse of the matrix in the memory
      x$setinv(inv)
      inv }



      
