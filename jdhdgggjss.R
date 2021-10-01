# sample data frame
df <- structure(list(numbers = c(
  1:300
)),
class = "data.frame",
row.names = c(NA, -10L))

require(plyr)
require(dplyr)

# count of numbers
n <- nrow(df)

# create all possible combinations to get sum
comb <- lapply(1:n, function(x) combn(df$numbers, x))


# transpose each matrix and append them together
all_comb <- rbind.fill.matrix(sapply(comb, function(x) t(x)))


# subset all rows from matrix that contains desired sum
result <- subset(all_comb, rowSums(all_comb, na.rm = T) == 50)


# get rid of columns that contains only NA
result <- as.data.frame(result)
result <- result[colSums(!is.na(result)) > 0]


# another way to make combinations
# in this example - 3 number combinations
comb2 <- expand.grid(df$numbers,df$numbers,df$numbers)


##try

# Import lpSolve package
library(lpSolve)

# Set coefficients of the objective function
f.obj <- c(5, 7)

# Set matrix corresponding to coefficients of constraints by rows
# Do not consider the non-negative constraint; it is automatically assumed
f.con <- matrix(c(1, 0,
                  2, 3,
                  1, 1), nrow = 3, byrow = TRUE)

# Set unequality signs
f.dir <- c("<=",
           "<=",
           "<=")

# Set right hand side coefficients
f.rhs <- c(16,
           19,
           8)

# Final value (z)
lp("max", f.obj, f.con, f.dir, f.rhs)

# Variables final values
lp("max", f.obj, f.con, f.dir, f.rhs)$solution

# Sensitivities
lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$sens.coef.from
lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$sens.coef.to

# Dual Values (first dual of the constraints and then dual of the variables)
# Duals of the constraints and variables are mixed
lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$duals

# Duals lower and upper limits
lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$duals.from
lp("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$duals.to

#3rd try
subset_sum = function(numbers,target,partial=0){
  if(any(is.na(partial))) return()
  s = sum(partial)
  if(s == target) print(sprintf("sum(%s)=%s",paste(partial[-1],collapse="+"),target))
  if(s > target) return()
  for( i in seq_along(numbers)){
    n = numbers[i]
    remaining = numbers[(i+1):length(numbers)]
    subset_sum(remaining,target,c(partial,n))
  }
}
