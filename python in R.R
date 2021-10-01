gl <- data.table(gl)
require(data.table)
library("data.table")
gl
require(reticulate)
install.packages("reticulate")
py_run_string("import numpy as np")
Y
py_run_string(" def subset_sum(numbers, target, partial=[]):
  s = sum(partial)

# check if the partial sum is equals to target
if s == target: 
  print "sum(%s)=%s" % (partial, target)
if s >= target:
  return
for i in range(len(numbers)):
  n = numbers[i]
remaining = numbers[i+1:]
subset_sum(remaining, target, partial + [n])" )
