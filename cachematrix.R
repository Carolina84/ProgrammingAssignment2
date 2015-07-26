# Caching the Inverse of a Matrix

#Below are two functions that create a special matrix object,
# compute and cache its inverse in order to save computation time.

#The first function, makeCacheMatrix(), creates a special "matrix",
#which is actually a list  which perform the following calculations:

makeCacheMatrix <- function(x = matrix(m = matrix()) { 
        # set the value of the input matrix
        inverse <- NULL  
        set <- function(y = matrix()){
                x <<- y
                inverse <<- NULL
        }  
        #get the value of the input matrix
        get <- function() {
                x
        }
        #set the value of the matrix inverse
        setinv <- function(i)  {
                inverse <<- i   
        }    
        #get the value of the matrix inverse
        getinv <- function() {
                inverse
        }
        ## return the created functions to the working environment
        list(set = set, get = get, setinv = setinv, getinv = getinv)      
}


#The second function, cacheSolve, computes the inverse of the special matrix created
#with the 'makeCacheMatrix' but only  the inverted matrix does not exist in cache

cacheSolve <- function(x, ...) {
        #output of makeCacheMatrix()
        inverse <- x$getinv()
        # if the inverse has already been calculated 
        #it get it from the cache and skips the computation
        if(!is.null(inv)){
                message("getting cached data")
                return(inverse)
        }
        # else calculates the inverse
        data <- x$get()
        inverse <- solve(data, ...)
        # sets the value of the inverse in the cache via the setinv function
        
        x$setinv(inverse)
        inverse       
}

## to test the functions:
x <- (matrix(1:4, 2, 2))
m = makeCacheMatrix(x)
m$getinv()
cacheSolve(m)
