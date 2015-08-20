# R Programming Course
# Assignment 2
# Github user: cainiaowang

# Assignment URL:
# https://class.coursera.org/rprog-031/human_grading/view/courses/975105/assessments/3/submissions

## These two fuctions are used to create a special matrix
## which has the ability to calcualte its own inverse
## and store this inverse for later retrieval. This can be usefull
## in situations when re-calculating this would be computationally too 
## expensive

## Note: Structure of these functions has been taken from the example 
##       provided on the assignment page (see url above)



## The makeCacheMatrix function creates a special Matrix
## This "matrix" contains a set of functions allowing it to:
## 1.   set the value of the matrix
## 2.   get the value of the matrix
## 3.   set the value of the inverse
## 4.   get the value of the inverse

makeCacheMatrix <- function(x = matrix()) 
{
    ## Set up Special Matrix

    # Pre-calculated Inverse: initialise to NULL
    inv <- NULL

    # Setup a new Matrix, reset pre-calc value
    set <- function(y) 
    {
        x <<- y
        inv <<- NULL
    }

    # Get the stored Matrix
    get <- function() x
    
    # Store the inverse for later
    setinv <- function(inverse) inv <<- inverse
    
    # Retrieve the pre-calulated inverse
    getinv <- function() inv

    # Stored list of functions
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## This Second function makes use of the above function to check
## to see if a solution has already been found for the matrix
## if it has it returns the pre-calculated value, alternatively it
## solves the inverse and store this for later retrieval.

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'

    # Retrieve the pre-calculated inverse if it exists
    inv <- x$getinv()
    
    # Check Retrieved value
    if(!is.null(inv)) 
    {
        # If Valid: return this pre-calculated value
        message("getting cached data")
        return(inv)
    }
    
    # Otherwise proceed with solving the Inverse
    # Get the Matrix
    data <- x$get()
    
    # Calculate the Inverse
    inv <- solve(data, ...)
    
    # Save this Inverse for later retrieval 
    x$setinv(inv)
    
    # Return this calculated inverse
    inv
}
