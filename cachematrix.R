
#The makechart function,  creates a special "matrix", 
#which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
       inverseMatrix <- NULL
       
       set <- function (y)  {
          x <<- y
          inverseMatrix <<- NULL
       }
   
       get <- function () {
          x
       }
       
       setInverse <- function ( solve ) {
          inverseMatrix <<- solve
       }
       
       getInverse <- function (){
          inverseMatrix
       }
       
       list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
       
}


## Calculates the mean of the special "matrix" created with the above function. 
#However, it first checks to see if the inverse Matrix has already been calculated. 
#If so, it gets the inverseMatrix from the cache and skips the computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   inverseMatrix <- x$getInverse()
   
   if(!is.null(inverseMatrix)) {
      message("getting cached Matrix data")
      return(inverseMatrix)
   }
   data <- x$get()
   message("Calculating for the first time...")
   inverseMatrix <- solve(data)
   x$setInverse(inverseMatrix)
   inverseMatrix
   
}

##### How to test this conmmand Line
#> b<-makeCacheMatrix()
#> b$set(matrix ( c(2,4,3,1,5,7,1,9,0), nrow=3, ncol=3))
#> cacheSolve(b)
# Expected result :  calculating...
#> If you run it again ie. cacheSolve(b)
#> you will see "getting cached Matrix data"



