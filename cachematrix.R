## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates an object to hold a matrix and it inverse
makeCacheMatrix <- function(x = matrix()) {
    
    # the force command is needed to ensure that x has been created since
    # R does lazy creation of variables it may not exist when we need it
    force(x)
    
    # initialize the matrix inverse
    cacheInverse <- NULL
    

    
    # The function to set the matrix cache to a matrix
    # also need to clear out the old matrix inverse since it won't be valid
    # for the new matrix
    set <- function(y) {
        #print("set")
        x <<- y
        cacheInverse <<- NULL
    }

    # function to get (return) the cached matrix
    get <- function() {
        #print("get")
        x
    }
    

    # set the matrix inverse value in the cached matrix object
    setInverse <- function(inverse) {
        #print("setInverse")        
        cacheInverse <<- inverse
    }
    
    # get the matrix inverse of the cached matrix
    # note the inverse is not computed here it is computed in the cacheSolve function so this
    # function may return a NULL it is wasn't computed and set with the setInverse function
    getInverse <- function() {
        
        cacheInverse
    }
    
    #set of the function list so that we can call the function
    list ( set = set, 
           get = get,
           setInverse = setInverse,
           getInverse = getInverse
         )
}



## This function computes the inverse of square matrices it
## it use the R solve function to compute it and then the inverse matrix
## is stored in the cacheMatrix via the setInverse function of the cacheMatrix object
cacheSolve <- function(cacheMatrixObj, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    #print("getInverse")
    cacheInverse <- cacheMatrixObj$getInverse()
    if (!is.null(cacheInverse)){
        message("Using the cached data")
        return (cacheInverse)
    }
    
    #print("must compute Inverse")
    
    # get the cached matrix
    data <- cacheMatrixObj$get()
    
    # compute the inverse of the cached matrix using the solve function
    cacheInverse <-  solve (data) 
    
    # set the inverse back in the cacheMatrix object so it doesn't have to be recomputed
    cacheMatrixObj$setInverse(cacheInverse)
    
    return(cacheInverse)
}
