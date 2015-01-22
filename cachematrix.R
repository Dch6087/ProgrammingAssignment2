## those functions work together and "duplicate" the initial matrix by "sticking" its inverse in it


## this function create a "special matrix" which store its inverse if it has been previously calculate by the cacheSolve function
makeCacheMatrix <- function(matrice = matrix()) {      
        m <- NULL
        getf <- function() matrice # function which return the matrix
        setinversef <- function(inv0) m <<- inv0 # function which store the inverse in m
        getinversef <- function() m # function which return the m value (ie : inverse of matrice or Null)
        
        list(get = getf, # I suppress the set function from the example because there is no use
             setinverse = setinversef,
             getinverse = getinversef)
}


## cachesolve is a function. It checks if the inverse exist in the "special matrix". If not it calculate it and store it
cacheSolve <- function(y, ...) { #y is a special matrix calculate by makeCacheMatrix
        m <- y$getinverse() # use the getinversef function (ie : the inverse or Null)
        if(!is.null(m)) { 
                message("getting cached data")
                return(m)
        }
        datamatrice <- y$get() # return the initial matrix to inverse
        m <- solve(datamatrice)
        y$setinverse(m) # use the setinversef function which store the inversed matrix in the "Special Matrix"
        m
}
