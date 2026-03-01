###########################
# sierpinski triangle
###########################

# The three corners
corners_T <- matrix(c(0, 0,  1, 0,  0.5, sqrt(3)/2), ncol = 2, byrow = TRUE)
corners_T #each rows are vertices of triangle
# Three functions
S1 <- function(point) point/ 2 + corners_T[1, ]/2 #(x+p)/2
S2 <- function(point) point/2 + corners_T[2, ]/2 #(x+q)/2
S3 <- function(point) point/2 + corners_T[3, ]/2 #(x+r)/2
S1(c(0,0))
S2(c(0,0))
S3(c(0,0))
# Short code to apply functions to multiple points
#apply fn will apply a fn to every row of input matrix 1..row 2..column
S1_set <- function(points) t(apply(points, 1, S1)) #t..transpose to keep same dimension
S2_set <- function(points) t(apply(points, 1, S2))
S3_set <- function(points) t(apply(points, 1, S3))
####################################################

S1 <- function(point) point/ 2
S1(c(0,0))


## Q1(a)
### Mapping corners of T
S1_corner <- S1_set(corners_T) #applies S1 on the set of corners 
S2_corner <- S2_set(corners_T) #applies S2 on the set of corners 
S3_corner <- S3_set(corners_T) #applies S3 on the set of corners 
S1_corner
S2_corner
S3_corner
par(mfrow = c(2,2))
plot(corners_T, pch = 19, 
     xlab = "x", ylab = "y",  main = "Corners of T", asp = 1)

plot(corners_T, pch = 19, xlab = "x", ylab = "y", 
     main = "S1(corners)", type = "n", asp = 1)
points(S1_corner, pch = 19)

plot(corners_T, pch = 19, xlab = "x", ylab = "y", 
     main = "S2(corners)", type = "n", asp = 1)
points(S2_corner, pch = 19)

plot(corners_T, pch = 19, xlab = "x", ylab = "y",
     main = "S3(corners)", type = "n")
points(S3_corner, pch = 19)


### Mapping edges of T
## Question 1 (b)

#################################
# creating useful subsets of T
# edges

## a function that obtain points on a line
#a is the first point on the line
#b is the second point on the line
points_on_line <- function(a, b)
{
  lam <- seq(0, 1, length = 50)
  #for every value of lam find lam*a+(1-lam)*b
  t(sapply(lam, function(l) l*a + (1-l)*b))
}

# get points on each edge
l1 <- points_on_line(corners_T[1,], corners_T[2, ]) #p and q as inputs, line connecting p and q
l2 <- points_on_line(corners_T[1,], corners_T[3, ]) #p and r as inputs
l3 <- points_on_line(corners_T[2,], corners_T[3, ]) #q and r  as inputs
edges_T <- rbind(l1, l2, l3) #combining all the 150 set of coordinates that are the boundary of triangle

#################################
# Write your code for Q1 (b) below
S1_edges <- S1_set(edges_T) #applies S1 on the set of corners 
S2_edges <- S2_set(edges_T) #applies S2 on the set of corners 
S3_edges <- S3_set(edges_T) #applies S3 on the set of corners 

par(mfrow = c(2,2))
plot(edges_T, pch = 19, 
     xlab = "x", ylab = "y",  main = "edges of T", asp = 1)

plot(edges_T, pch = 19, xlab = "x", ylab = "y", 
     main = "S1(edges)", type = "n", asp = 1)
points(S1_edges, pch = 19)

plot(edges_T, pch = 19, xlab = "x", ylab = "y", 
     main = "S2(edges)", type = "n", asp = 1)
points(S2_edges, pch = 19)

plot(edges_T, pch = 19, xlab = "x", ylab = "y",
     main = "S3(edges)", type = "n",asp=1) #asp for good proportion
points(S3_edges, pch = 19)

#union of sets..rbind

# Q2
##########################
## Creating the iterative function
iterative_func <- function(set) rbind(S1_set(set), S2_set(set), S3_set(set))
#to calculate Tn=S1(Tn-1)unionS2(Tn-1)unionS3(Tn-1)
## Q 2(a)
T0 <- corners_T         # change this to edges and points
# next find T1, T2, T3, T4, T5
T1<-iterative_func(T0)
#T1.. dim of T1 9x2 since it binds 3 sets of 3x2 matrices
T2<-iterative_func(T1)
T3<-iterative_func(T2)
T4<-iterative_func(T3)
T5<-iterative_func(T4)

par(mfrow = c(2,3))
plot(T0, pch = 20, cex = .1)  
plot(T1, col = 2, pch = 20, cex = .5)  
plot(T2, col = 3, pch = 20, cex = .5)  
plot(T3, col = 4, pch = 20, cex = .5)  
plot(T4, col = 5, pch = 20, cex = .5)  
plot(T5, col = 6, pch = 20, cex = .5)  


## Write code for Q2 (c)
T0 <- edges_T         # change this to edges and points
# next find T1, T2, T3, T4, T5
T1<-iterative_func(T0)
#T1.. dim of T1 9x2 since it binds 3 sets of 3x2 matrices
T2<-iterative_func(T1)
T3<-iterative_func(T2)
T4<-iterative_func(T3)
T5<-iterative_func(T4)

par(mfrow = c(2,3))
plot(T0, pch = 20, cex = .1)  
plot(T1, col = 2, pch = 20, cex = .5)  
plot(T2, col = 3, pch = 20, cex = .5)  
plot(T3, col = 4, pch = 20, cex = .5)  
plot(T4, col = 5, pch = 20, cex = .5)  
plot(T5, col = 6, pch = 20, cex = .5)  


## Q2 (d)
################################
### Grid of points in T
# Function that fills the points between three points
filling_T <- function(a, b, c)
{
  points <- NULL
  lam1 <- seq(0, 1, length = 25)
  lam2 <- seq(0, 1, length = 25)
  for(l1 in lam1)
  {
    for(l2 in lam2)
    {
      if(l1 + l2 < 1) points <- rbind(points, l1*a + l2*b + (1 - l1 - l2)*c)
    }
  }
  points
}

# calling the function for the corners
points_T <- filling_T(corners_T[1,], corners_T[2, ], corners_T[3, ])

#################################

## Write code for  Q2 (d)

T0 <- points_T         # change this to edges and points
# next find T1, T2, T3, T4, T5
T1<-iterative_func(T0)
#T1.. dim of T1 9x2 since it binds 3 sets of 3x2 matrices
T2<-iterative_func(T1)
T3<-iterative_func(T2)
T4<-iterative_func(T3)
T5<-iterative_func(T4)

par(mfrow = c(2,3))
plot(T0, pch = 20, cex = .1)  
plot(T1, col = 2, pch = 20, cex = .5)  
plot(T2, col = 3, pch = 20, cex = .5)  
plot(T3, col = 4, pch = 20, cex = .5)  
plot(T4, col = 5, pch = 20, cex = .5)  
plot(T5, col = 6, pch = 20, cex = .5) 





#######################################
# Question 3 (a)

# Make a function that does the transformation
# x is a vector
# h > 0
# centroid is a vector
# theta is in radians!!!
affine_map <- function(x, h, centroid, theta)
{
  # write your function here
  #M(x y)=(x y)+(-1/2 -1/2(root3))

}



# Question 3(b)


?sample
sample(1:4,size = 1, prob= c(1,1,1,1)/4)

sample(1:4,size = 1, prob= c(1,0,3,0)/4)





sample(1:4,size = 1, prob= c(1,0,0,3)/4)






