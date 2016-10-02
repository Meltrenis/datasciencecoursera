

## This function is aimed to calculate the inverse of a matrix. For use it,
## type "cacheSolve(makeCacheMatrix(<matrix>,...).
## makeCacheMatrix is just a compendium of functions. This functions will be
## called by CacheSolve using '$'.

makeCacheMatrix <- function(x = matrix()) {

        I <- NULL               #Set I to NULL to start
        
        set <- function(y) {    
                x <<- y         #x loads y value in parent environment (makeCacheMatrix)
                I <<- NULL      #Same for I (this is equal to line 8 sentence)
        }
        get <- function() x     #Returns x (because is the last sentence)
        setI <- function (inversa) I <<- inversa        #Inversa value to I
        getI <- function () I   #Returns I
        
        list(set=set,           #Make a list with this functions to call them
             get=get,           #later.
             setI=setI,
             getI=getI)
        
}


## This function uses functions which are storaged in makeCacheMatrix. 
## X must be a matrix.

cacheSolve <- function(x, ...) {        ##cacheSolve(makeCacheMatrix(A)
        
        I <- x$getI()             # Calls getI and loads its value (I) in I (I<-I)              
        if(!is.null(I)) {
                message("getting cached data")
                return (I)
        }
        data <- x$get()           # Calls get to put x value in data.
        I <- solve(data,...)      # Calculates inverse of I.
        x$setI(I)                 # I is the entry parameter in set, so loads I
        I                         # into I, and returns its value.
        ## Return a matrix that is the inverse of 'x'
}
