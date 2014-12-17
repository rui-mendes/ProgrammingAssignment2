## R Programming
## Rui Mendes (ruidamendes@ua.pt)
## Assigment 2
## December 2014

## Goal: write a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.
## Stores the given matrix and calculate several methods with information about the matrix itself ans his inverse
makeCacheMatrix <- function(x = matrix()) {
    
    ## Methods of the list: 
    #   - setMatrix = stores the matrix
    #   - getMatrix - returns the matrix
    #   - setInverseMatrix = stores the inverse matrix
    #   - getInverseMatrix = returns the inverse matrix
    
    ## Construct command (set inverse matrix to null)
    inverse <- NULL
    
    ## Function to store the matrix (to variable m)
    ## Note: the inverse matrix has not been calculated yet
    setMatrix <- function(matrix)
    {
      x <<- matrix
      inverse <<- NULL
    }
    
    ## Function to return the matrix itself (return x)
    getMatrix <- function() {
      x
    }
    
    ## Function to store the inverse matrix (to inverse variable)
    setInverseMatrix <- function(inverseMatrix) {
      inverse <<- inverseMatrix #(using cache feature)
    } 
    
    ## Function to return the inverse matrix (inverse variable)
    getInverseMatrix <- function() {
      inverse
    }
    
    ## List that contains the functions (getters and setters) of the matrix and inverse matrix
    list(
      setMatrix = setMatrix,
      getMatrix = getMatrix,
      setInverseMatrix = setInverseMatrix,
      getInverseMatrix = getInverseMatrix
    )
}


# This function returns the inverse matrix calculation. However, first it searchs if the inverse matrix is already calculated,
# If yes, then returns this matrix, else does the calculation of the inverse matrix, stores the result (cache) and returns it.
cacheSolve <- function(x, ...) {
  
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverseMatrix()
    
    ## Check if the inverse matrix has already calculated (is already in cache)
    if( !is.null(inverse) ) {    
      return(inverse)
    }
    
    ## Get the data of x, make the inverse calculation and set the inverse to the object
    newMatrix <- x$getMatrix()
    inverse <- solve(newMatrix, ...)
    
    ## Set the calculate inverse matrix to the object x
    x$setInverseMatrix(inverse)
    
    ## Return the inverse matrix
    inverse
}

## Example to test (from the course forum):
## source("ProgrammingAssignment2/cachematrix.R")
## set.seed(77)
## m <- matrix(sample.int(100,size=9,replace=TRUE), nrow=3)
## d <- makeCacheMatrix(m)
## cacheSolve(d)
##         [,1]        [,2]        [,3]
## [1,]  0.005466809 -0.02690806  0.03187788
## [2,]  0.058413206 -0.10244941  0.06464324
## [3,] -0.055484558  0.12374867 -0.08328009
## 