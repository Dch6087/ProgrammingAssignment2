## those functions work together and "duplicate" the initial matrix by "sticking" its inverse in it


## this function creates a "special matrix" which stores its inverse if it has been previously calculated by the cacheSolve function
makeCacheMatrix <- function(matrice = matrix()) {      
        m <- NULL
        getf <- function() matrice # function which returns the matrix
        setinversef <- function(inv0) m <<- inv0 # function which stores the inverse in m
        getinversef <- function() m # function which returns the m value (ie : inverse of matrice or Null)
        
        list(get = getf, # NB : I suppress the set function from the example because there is no use here
             setinverse = setinversef,
             getinverse = getinversef)
}


## cachesolve is a function. It checks if the inverse exists in the "special matrix". If not it calculates it and stores it
cacheSolve <- function(y, ...) { #y is a special matrix calculate by makeCacheMatrix
        m <- y$getinverse() # uses the getinversef function (ie : the inverse or Null)
        if(!is.null(m)) { 
                message("getting cached data")
                return(m)
        }
        datamatrice <- y$get() # returns the initial matrix to inverse
        m <- solve(datamatrice)
        y$setinverse(m) # uses the setinversef function which store the inversed matrix in the "Special Matrix"
        m
}
