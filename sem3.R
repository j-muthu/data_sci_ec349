install.packages("tensorflow")
tensorflow::install_tensorflow()

library(tensorflow)
tf$constant("TensorFlow is working!")

library(reticulate)
py_config()

library(keras)
library(tidyverse)