# Stub to load the blackboard grader tool and evaluate the files in this folder.
library(this.path)

grader_path <- 'C:/Users/ndg00008/Dropbox/Courses/rblackboardgrader/rblackboardgrader.R'
source(grader_path)

this_directory <- this.dir()

setwd(this_directory)
grade_all()
