#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p){
  grid.r = r
  grid.c = c
  density = p
  
  ncars = round(p * grid.r * grid.c, 0)
  grid.size = grid.r * grid.c
  
  car.sample = c(rep(0, grid.size - ncars), rep(1, ncars/2), rep(2, ncars/2))
  m = matrix(sample(car.sample, grid.size, replace = T), nrow = grid.r)
  
 
 return(m)
}

image(t(m)[,nrow(m):1], axes=FALSE, col = c("white", "red", "blue"))



#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.


check.grid.east <- function(m,i,j){
  if (j == dim(m)[2]-1){
    if(m[i,j] == 1 & m[i,j+1] == 0){
      m[i,j] = 0
      m[i,j+1] = 1
    }
    else if(m[i,j] == 1 & m [i,j+1] == 1 & m[i,1] == 0){
      m[i,j+1] = 0
      m[i,1] = 1
    }
    else if(m[i,j] == 0 & m[i,j+1] == 1 & m[i,1] == 0){
      m[i,j+1] = 0
      m[i,1] = 1
    }
    else if(m[i,j] == 2 & m[i,j+1] == 1 & m[i,1] == 0){
      m[i,j+1] = 0
      m[i,1] = 1
    }
  }
  else if(j == dim(m)[2] & m[i,j] == 1 & m[i,1] == 0){
    m[i,j] = 0
    m[i,1] = 1
  }
  return(m)
}


check.grid.north <- function(m,i,j){
  if (i == 1 & m[1,j] == 2 & m[dim(m)[1],j] == 0){
    m[1,j] = 0
    m[dim(m)[1],j] = 2
  }
  else if ( i == 2 & m[2,j] == 0 & m[1,j] == 2 & m[dim(m)[1],j] == 0){
    m[dim(m)[1],j] = 2
    m[1,j] = 0
  }
  else if ( i == 2 & m[2,j] == 1 & m[1,j] == 2 & m[dim(m)[1],j] == 0){
    m[dim(m)[1],j] = 2
    m[1,j] = 0
  }
  else if ( i == 2 & m[2,j] == 2 & m[1,j] == 0){
    m[2,j] = 0
    m[1,j] = 2
  }
  else if ( i == 2 & m[1,j] == 2 & m[2,j] == 2 & m[dim(m)[1], j] == 0){
    m[dim(m)[1],j] = 2
    m[1,j] = 0
  }
  return(m)
}


bml.step.east <- function(m){
  i=1
  max=dim(m)[2]
  for(i in 1:dim(m)[1]){
    j=1
    while(j < dim(m)[2] - 1){
      if(m[i,j] == 1 & m[i,j+1] == 0){
        m[i,j] = 0
        m[i,j+1] = 1
        j = j+2
      }
      else j = j+1 
    }
    m=check.grid.east(m,i,j)
  }
  
  return(m)
}







bml.step.north <- function(m){
  j=1
  for(j in 1:dim(m)[2]){
    i=dim(m)[1]
    while(i > 2){
      if(m[i,j] == 2 & m[i-1,j] == 0){
        m[i,j] = 0
        m[i-1,j] = 2
        i = i-2
      }
      else i = i-1
    }
    m=check.grid.north(m,i,j)
    
  }
  return(m)
}


bml.step <- function(m){
  if(length(m) == 1) return(list(m, as.logical('FALSE')))
  else{
    m1 = m
    m = bml.step.east(m)
    m = bml.step.north(m)
    grid.new = any(m1 != m)
    return(list(m,grid.new))
  }
}





#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)







## I have set bml.sim function to have the max iteration step at 20,000.
## This function returns the list with the 'initial grid', 'end grid' and number of steps taken,
## and the number of steps which will be either 20,000 which means that the grid has a free-flow or 
## some number of steps taken until the grid got gridlocked.

bml.sim <- function(r, c, p){
  steps=20000             #this is where we set the max number of iterations
  s=2
  m.initial = bml.init(r,c,p)
  x = bml.step(m.initial)
  m=x[[1]]
  check.gridlock=x[[2]]
  while(check.gridlock & s < steps){
    x=bml.step(m)
    m=x[[1]]
    check.gridlock=x[[2]]
    s=s+1   
  }
 return(list(m.initial,m,s))      #output has initial grid, end grid and number of iterations reached
  
}



## This function help us observe the behaviour of the 'n' different sample grids 
## of the same size ('r' x 'c') and same density 'p'. It helps us examine the 
## structure of the grids that get gridlocked and those that aren't.
## I used this function to examine and plot initial and end grids for the free-flow and
## gridlocked one.

bml.sim2 <- function(r,c,p,n){     #this function generates 'n' different samples of the same grid size and density
  x=c()
  while (n > 0){
    x = c(x, bml.sim(r,c,p))
    n=n-1
  }
  return(list(x))        #this test function returns an initial, an end grid, as well as the number of iterations for each grid for number of 'n' samples
}


## This functions serves us to observe the behaviour of different grid sizes and different denisities.
## It generates 'n' sample grids of the same size and denisity and it returns the number
## of iterations needed for each one of them.

bml.sim3 <- function(r,c,p,n){       
  number.of.iterations = c()
  while (n > 0){
    number.of.iterations = c(number.of.iterations, bml.sim(r,c,p)[[3]])
    n=n-1
  }
  return(number.of.iterations)
  
}



image(t(m)[,nrow(m):1], axes=FALSE, col = c("white", "red", "blue"))

initial.grid.lock.free = matrix(c(1,1,0,2,1,1,0,0,1,0,2,2,0,2,1,2,2,2,1,2,1,2,2,1,1), nrow=5)
end.grid.lock.free = matrix(c(1,2,0,1,1,1,1,0,1,1,2,2,2,1,1,2,0,2,2,2,1,2,2,0,0), nrow=5)

image(t(initial.grid.lock.free)[,nrow(initial.grid.lock.free):1], axes=FALSE, col = c("white", "red", "blue"))
image(t(end.grid.lock.free)[,nrow(end.grid.lock.free):1], axes=FALSE, col = c("white", "red", "blue"))
