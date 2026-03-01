##Worksheet 2################
##Q3#########################

#### curve sketching for a one dimensional function

######### load the following libraries
library(mosaicCalc) ## necessary to use slice_plot() function

library(viridisLite)  ## to use viridis colours in plots

## generating viridis colour palatte of size, i.e., n = 3
v_col <- viridis(n = 3, option = "D") 
## here "D" stands for viridis
## you can change option from "A" to "H" for various color possibilities

### define function f(x)
f <- function(x){
  x^5-15*x^3
}

#### plot f(x) using slice_plot() for a given domain [-5, 5]
slice_plot( f(x) ~ x, domain(x = range(-5, 5)), color = v_col[3]) 


### obtaining zeros of f(x)
### using polyroot function
### which takes coefficients as input in increasing order of the polynomial
f.zeros <- polyroot(c(0,0,0,-15,0,1))
f.zeros

### note that polyroot gives zeros in complex number from 
### but actualy they are real numbers 
### so we use Re() to do make them real

### adding zeros of f(x) in the plot
### using %>% pipe operator

gf_point(f(Re(f.zeros)) ~ Re(f.zeros),  color = v_col[1])%>%
  gf_labs(x= "x",
          y = "f(x)",
          main = "Plot of f(x) and its derivatives")%>%
  slice_plot( f(x) ~ x, domain(x = range(-5, 5)), color = v_col[1])

#### f'(x) 
f.prime <- function(x){
  5*x^4-45*x^2
}

### obtaining zeros of f'(x) using polyroot 
fprime.zeros <- polyroot(c(0,0,-45,0,5))
fprime.zeros

### adding zeros of f'(x) in the plot
gf_point(f(Re(f.zeros)) ~ Re(f.zeros),  color = v_col[1])%>%
  gf_labs(x= "x",
          y = "f(x)",
          main = "Plot of f(x) and its derivatives")%>%
  gf_point(f.prime(Re(fprime.zeros)) ~ Re(fprime.zeros),  color = v_col[2])%>%
  slice_plot( f(x) ~ x, domain(x = range(-5, 5)), color = v_col[1])

####  f''(x) 
f.dobprime <- function(x){
  20*x^3-90*x
}

### obtaining zeros of f''(x) using polyroot 
fdobprime.zeros <- polyroot(c(0,-90,0,20))
fdobprime.zeros

### adding zeros of f''(x) in the plot
gf_point(f(Re(f.zeros)) ~ Re(f.zeros),  color = v_col[1])%>%
  gf_labs(x= "x",
          y = "f(x)",
          main = "Plot of f(x) and its derivatives")%>%
  gf_point(f.prime(Re(fprime.zeros)) ~ Re(fprime.zeros),  color = v_col[2])%>%
  gf_point(f.dobprime(Re(fdobprime.zeros)) ~ Re(fdobprime.zeros),  color = v_col[3])%>%
  slice_plot( f(x) ~ x, domain(x = range(-5, 5)), color = v_col[1])



