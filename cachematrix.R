##Coursera R, Week 3 - Peer Reviewed Programming Assignemnt - Pranav Mehta
## Storing inverse of a matrix in cache

##Caching the inverse of matrices reduces the computation cost of the same

##makeCacheMatrix: Creating a special matrix to cache the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        ##1. This will store the inverse of the matrix
        invfnc <- NULL          
        
        ##2. Setting the value of the matrix
        setval <- function(m){  
        x <<- m
        invfn <<- NULL
        }
        ##3. Getting the value of the matrix
        get <- function() x     
        
        ##4. Set the inverse matrix value
        setinv <- function(inverse) invfnc <<- inverse 
        
        ##5. Getting the inverse value of the matrix
        getinv <- function() invfnc
        list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve computes the inverse of the matrix that has been returned by 
##makeCacheMatrix. If the inverse has been calculated already, it will 
##return the inverse matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ##Set invfnc
        invfnc <- x$getinv()
        
        ##If the inverse has been calculated already, then cacheSolve will 
        ##retrieve the value
        if(!is.null(invfnc)){
                message("Retrieving Cached Data")
                return(invfnc)
        }
        
        mat <- x$get()
        ##Returning the Inverse of the matrix
        invfnc <- solve(mat, ...)
        x$setinv(invfnc)
        invfnc
}