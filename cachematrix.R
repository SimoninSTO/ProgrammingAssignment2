## get a lists of 4 different functions

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setmatrix <- function(x) m <<- solve(x)
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
    
}

## the resulting 4 different functions will be input into the following
##functions, meaning z will be a list of 4 different functions, 
##set function, get function, setmatrix function and getmatrix function

cacheSolve <- function(z, ...) {
        m<-z$getmatrix() ##extract getmatrix function from z, this line equals 
                         ##m<-function() m, but by using the reference, one can
                         ##get the the variables and values from the 
                         ##makeCacheMatrix function above
        if(!is.null(m)){
          message("getting cached data")
          return(m)
        }
        data<-z$get()##data here is the get function extracted from z
                     ##namely, it equals to data<-function() x
                     ##it retrives the x value from the makeCacheMatrix function
        m<-solve(data)
        
        m
}
