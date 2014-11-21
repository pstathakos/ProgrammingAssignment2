## These two functions allow a user to pass a matrix between the two functions and solve it's inverse.
## If the inverse has already been determined then the cached version of the inverse will
## be retuned instead.

## This function defines a number of functions which can be called. It returns a list of pointers
## to the functions, allowing them to be called from another function within the global scope

makeCacheMatrix <- function(x = matrix()) {

    ## Create object to store the inverse matrix and set it to NULL
    inv<-NULL
    
    
    ## Function to set the value of the x matrix and reset the inverse matrix to NULL
    set<-function(y) {
        x<<-y
        inv<<-NULL
    }
    
    ## Function to get the matrix
    get<-function() { x }
    
    ## Function to set the inverse of the matix
    setinv<-function(solve) { inv<<-solve }
    
    ## Function to return the cached inverse matrix
    getinv<-function() { inv }
    
    
    ## Create a list with pointers to the four functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve function determines if the inverse value of the matrix has already been cached
## and returns it if has been. If not, it calcualtes the inverse and stores it in the cache.

cacheSolve <- function(x, ...) {
        
    ## Call the getinv function to obtain the inverse matrix
    inv<-x$getinv()
    
    ## Check if the inverse matrix is NULL or has already been set
    if(!is.null(inv)) {
        
        message("Obtianing the cached matrix")
        
        ## Inverse matrix has already been cached, return the cached value
        return(inv)
    }
    
    
    ## Inverse matrix has not been cached, get the matrix and solve it's inverse
    ## then cache the newly calculated inverse
    
    data<- x$get()
    inv<-solve(data)
    
    ## Cache the invese matrix
    x$setinv(inv)
    
    ## Return the inverse of the matrix
    inv
}
