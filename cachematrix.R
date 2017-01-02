## Put comments here that give an overall description of what your
## functions do
## These functions create a inverse of matrix and make it cacheable to reduce computation costs
## Write a short comment describing this function
## makeCacheMatrix() creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function(){
                x
        }
        setInverse <- function(inverse){
                        i <<- inverse
        }
        getInverse <- function(){
                        i
        } 
        list(set = set,get = get,setInverse = setInverse,
                  getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
              i <- x$getInverse()
              
              ## check if inverse exists 
              if(!is.null(i)) {
                message("getting cached data")
                return(i)
              }
              data <- x$get()
              i <- solve(data)
              x$setInverse(i)
              i
}
