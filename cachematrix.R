

##This function gets the required inverted matrix from the cache or set the inverted matrix in the cache if it is not found in the cache.
makeCacheMatrix <- function(input_matrix = matrix()) 
{
  #Usage makeCacheMatrix(arg ) where arg is the user input matrix
  
  #Assign the inverse_matrix variable with NULL
  inverse_matrix <- NULL
  
  #Function to set the variables to the environment
  set <- function() 
  {
    inverse_matrix <<- NULL
  }
  
  #Function to return the input matrix
  get <- function()
  {
    input_matrix
  }
  
  #Function to set the variable inverse_matrix with the calculated inverted matrix
  setinverse <- function(m) 
  {
    inverse_matrix <<- m
  }
  
  #Function to return the pre-built inverted matrix from the environment
  getinverse <- function() 
  {
    inverse_matrix
  }
  
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

##This function searches the cache for the inverted matrix. If the inverted matrix is not found in the cache, then it calculates the inverted matrix and set the cache using the above function.

cacheSolve <- function(fn_makematrix, ...) 
{
  #Usage: cacheSolve(arg)
  # arg: Function to create the cache matrix i.e. the makeCacheMatrix() function
  # The "makeCacheMatrix" function has the user input matrix as the argument.
  #e.g. cacheSolve(makeCacheMatrix(matrix( c(4, 2, 7, 6),  nrow=2, ncol=2) ))
  # Output :      [,1] [,2]
  # 		  [1,]  0.6 -0.7
  # 		  [2,] -0.2  0.4
  
  # Getting the data from the cache
  m <- fn_makematrix$getinverse() 
  
  #Checking whether the data is present in the cache, if present displays it
  if (!is.null(m)) 
  {
    message("getting cached data")
    return(m)
  }
  
  #If the data not found in the cache,
  
  #Assign the user input matrix to the varaible "data"
  data <- fn_makematrix$get()    
  
  #Calculating the inverted matrix
  m <- solve(data, ...)         
  
  #Set the inverted matrix to the environment variable(cache) for the future use
  fn_makematrix$setinverse(m)   
  
  #Display the result
  m                             
}
