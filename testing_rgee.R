

library(reticulate)
Sys.which("python")
Sys.which("python3")
use_python("C:/Users/sears/AppData/Local/r-miniconda/envs/r-reticulate/python.exe")

#testing that python is working correctly
np <- reticulate::import("numpy", convert = FALSE)
ee <- reticulate::import("ee", convert = FALSE)

a <- np$array(c(1:4))
print(a)  # this should be a Python array

print(py_to_r(a)) #this should be an R array
(sum <- a$cumsum()) ## Answer should be: [ 1  3  6 10]
print(py_to_r(sum)) ## Answer should be: [1]  1  3  6 10

#load in rgee package


library(rgee)

virtualenv_create("rgee", 
                  python = "python3")
use_virtualenv("rgee")
ee_install_rgee_python_packages()

rgee::ee_install()
