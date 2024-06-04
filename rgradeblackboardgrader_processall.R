# Stub to load the blackboard grader tool and evaluate the files in this folder.
library(this.path)

this_directory <- this.dir()
setwd(this_directory)


grader_path <- 'C:/Users/ndg00008/Dropbox/wvu/Courses/rblackboardgrader/rblackboardgrader.R'
source(grader_path)

test_path <- paste(this_directory, '/test.R', sep = '')

# Verify that the solution works
calcGrades(submission_dir = './solution/',
           your_test_file = test_path)

# Grade submissions
grade_first_blackboard_zip_file_in_wd_and_save_results(test_path)

