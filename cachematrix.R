## The file contains two functions, which enable to cache a matrix and it's inverse in the system
## Therefore avoiding to recalculate the inverse of the matrix everytime the function is called
##
## - makeCacheMatrix: 
## - cacheSolve:  


## -----------------------------
## --------- makeCacheMatrix(x)
## -----------------------------
##
## this function creates a list of 4 functions for setting/getting both the provided matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
      M <- NULL # Initialization of the Matrix everytime makeCacheMatrix is called
      
      # The setting function enables calling the created object using prefix $set(My_new_invertible_matrix)
      set <- function(y) {
            x <<- y # Save the input vector provided by calling set function
            m <<- NULL # Here we re-initialize the cache for m to NULL
      }
      
      # The getting function retrieves the value of the numeric input 
      get <- function() x

      # The setting inverse function is a function that when called will 
      # calculate the inverse and store it in the cache using the superassignment
      setinverse <- function(inverse) M <<- inverse

      # The getting inverse function is a function that
      # retrieves the value of the calculated inverse matrix
      getinverse <- function() M
      
      # The below lists all of the functions in a list that will be called by cacheSolve
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## -----------------------------
## --------- cacheSolve(x)
## -----------------------------
##
## this function executes the following process
##    1- if the 4th element (of the provided result list from previous function)
##    , $getinverse(), returns 'NA', then
##          1.1- It calculates the inverse of the initial matrix (retrieved using $get()),
##          1.2- Stores it using $setinverse(M)
##          1.3- And Returns it's value
##    2- if the $getinverse() is not 'NA'
##          2.1- The function returns the following message "getting cached data"
##          2.2- As well as the value of M called via $getinverse() function

cacheSolve <- function(x) {
      
      ## The below captures the cached value of the inverse
      M <- x$getinverse()
      
      ## If the cached inversed is existant, then it returns a message and it's value
      if(!is.null(M)) {
            message("getting cached data")
            return(M) ## Note that the function ends if the return(M) is executed
      }
      
      ## If the IF condition has not been executed, the function continues
      
      MyMatrix <- x$get()     ## Captures the initial matrix and stores it under the variable MyMatrix
      M <- solve(MyMatrix)    ## Executes the solve function to calculate the inverse, then stores it under M
      x$setinverse(M)         ## Pushes the value of the inverse matrix M to the cache
      M                       ## And displays the value of the updated inverse matrix
}
