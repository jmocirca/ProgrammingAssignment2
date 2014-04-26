## This set of functions allows for the caching of matrices and their inverses. The first function allows you to set a matrix and retrieve its value. It also allows you to set the inverse of the matrix and retrieve its value. The second function will actually solve the inverse of the matrix after checking to be sure that the value doesn't already exist in the cache. If the value already exists, it will retrieve the cache. If the value does not exist, it not only solves the matrix but also writes it to the cache so that it can be retrieved without re-computation. 

## Sets the value of a matrix and gets the matrix. Sets the value of a matrix's inverse and gets the inverse

makeCacheMatrix <- function(x = matrix()) {   #reads in matrix
	s<- NULL
	set<- function(y){						 #sets the value of the matrix
		x<<-y
		m<<-NULL
	}
	get<- function() x						#gets the value of the matrix
	setInverse<- function(solve) s <<- solve  #user defined setting of the inverse
	getInverse<- function() s				#gets the value of the inverse
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Takes a matrix object as input and checks the cache to see if its inverse has already been solved. If the inverse is not in the cache, it solves the matrix and writes the inverse to the cache. 

cacheSolve <- function(x, ...) {
	s <- x$getInverse()				#checks the cache to see if the inverse exists
	if(!is.null(s)){				#if inverse is in the cache
		message("getting cached data")  #returns message
		return(s)					#return the inverse matrix
	}
	data<- x$get()                  #if inverse is not in the cache, writes the cached matrix to data
	s<- solve(data, ...)			#solves the inverse
	x$setInverse(s)					#sets the value of the inverse in the makeCacheMatrix 
	s 								#return the inverse matrix
}
