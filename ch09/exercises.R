# 9.1
ls("package:methods")[1:20]
length(ls("package:methods"))

environment(fun = read.table)
environment(fun = data)
environment(fun = matrix)
environment(fun = jpeg)

any(ls("package:graphics") == "smoothScatter")