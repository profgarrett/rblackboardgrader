# Grade all files in downloaded zip
#
# NDG, updated 6/4/24
#
# To use:
# calcGrades: grade an already expanded folder of files against a test.R
# grade_first_blackboard...:  grab the first zip file, unzip, test, and
#   update the downloaded csv of student grades.

library(testthat)
library(dplyr)
library(stringr)



#################################################################################
#################################################################################
# gradeR Sourcecode
# https://cran.r-project.org/web/packages/gradeR/
#
# MIT License
#################################################################################
#################################################################################



#' This function finds unreadable files.
#'
#'  A function that finds student submissions with poorly encoded characters
#' @param submission_dir where the assignments are located
#' @keywords calcGrades findBadEncodingFiles
#' @export
#' @examples
#' \donttest{
#' # change paths to *your* paths
#' submissions <- "extdata/assignment1_submissions/"
#' findBadEncodingFiles(submissions) # perhaps ask these students to resubmit
#' }
findBadEncodingFiles <- function(submission_dir){
  scripts_to_grade <- list.files(path = submission_dir,
                                 recursive = T,
                                 pattern = "\\.r$",
                                 ignore.case = T)
  atLeastOneBadFile <- FALSE
  for(script in scripts_to_grade) {
    globalPath <- paste(submission_dir, script, sep = "")
    lines <- readLines(globalPath, warn = F)
    badLines <- lines[!validUTF8(readLines(globalPath, warn = F))]

    if(length(badLines) > 0) {
      atLeastOneBadFile <- TRUE
      cat("======================================================================\n")
      cat("In ", script, "\n")
      for(elem in badLines) cat(elem, "\n")
    }
  }
  if(atLeastOneBadFile)
    cat("======================================================================\n")
}


#' This function finds files with global file paths.
#'
#'  A function that finds student submissions that refer to machine-specific file paths
#' @param submission_dir where the assignments are located
#' @keywords calcGrades findGlobalPaths
#' @export
#' @examples
#' \donttest{
#' # change paths to *your* paths
#' submissions <- "extdata/assignment1_submissions/"
#' findGlobalPaths(submissions) # perhaps ask these students to resubmit
#' }
findGlobalPaths <- function(submission_dir) {
  scripts_to_grade <- list.files(path = submission_dir,
                                 recursive = T,
                                 pattern = "\\.r",
                                 ignore.case = T)
  print(scripts_to_grade)
  atLeastOneBadFile <- FALSE
  for(script in scripts_to_grade) {
    globalPath <- paste(submission_dir, script, sep = "")
    lines <- readLines(globalPath, warn = F)
    badLines <- suppressWarnings(lines[grep("^[^#].+[\\\\/]+", lines)])

    if(length(badLines) > 0) {
      atLeastOneBadFile <- TRUE
      cat("======================================================================\n")
      cat("In ", script, "\n")
      for(elem in badLines) cat(elem, "\n")
    }
  }
  if(atLeastOneBadFile)
    cat("======================================================================\n")
}


#' The grading function.
#'
#' This function grades a bunch of R script assignments
#' @param submission_dir where the assignments are located
#' @param your_test_file the path to your testthat test file (e.g. grade_hw1.R)
#' @param suppress_warnings warning handlers prevent code from being run after they catch something. Suppress this behavior by setting this argument to TRUE.
#' @param verbose set to true if you want to print the name of the file as it's being ran
#' @keywords calcGrades
#' @export
#' @examples
#' \donttest{
#' # change paths to *your* paths
#' submissions <- "extdata/example/assignment1_submissions/"
#' my_test_file <- system.file("extdata/example", "grade_hw1.R", package = "gradeR")
#' results <- calcGrades(submissions, my_test_file)
#' }
calcGrades <- function(submission_dir, your_test_file, suppress_warnings = T, verbose = T){

  if(missing(submission_dir) | missing(your_test_file))
    stop("the first two arguments are required")

  paths <- list.files(path = submission_dir,
                      recursive = T,
                      pattern = "\\.r",
                      ignore.case = T)

  print(paths)

  trial_test <- testthat::test_file(your_test_file, reporter = "minimal")
  number_questions <- length(trial_test)

  if(number_questions == 0)
    stop("You need at least one graded question")

  number_students <- length(paths)
  score_data <- data.frame("id" = vector(mode = "character", length = number_students),
                           matrix(data = 0, nrow = number_students,
                                  ncol = number_questions),
                           stringsAsFactors = F)

  student_num <- 1
  for(path in paths ){

    # run student's submission in a separate process
    # https://stackoverflow.com/questions/63744905/attaching-packages-to-a-temporary-search-path-in-r/63746414#63746414
    tmp_full_path <- paste(submission_dir, path, sep = "")
    if (T) cat("grading: ", tmp_full_path, "\n")
    # run student's submission in a separate process
    # https://stackoverflow.com/a/63746414/1267833
    rogueScript <- function(source_file_path){
      rogueEnv <- new.env()
      source(source_file_path, rogueEnv)
      rogueEnv
    }
    # remove previous scriptResults in case an error is triggered and it's never re-created
    if( exists("scriptResults") ) rm(scriptResults)

    if( suppress_warnings ){
      tryCatch(
        suppressWarnings(scriptResults <- callr::r(rogueScript,
                                                   args = list(tmp_full_path),
                                                   show = FALSE, package = TRUE)),
        error = function(e){
          print(paste0("error: ", e$parent$call))
          print(e$parent$trace)
        },
        message = function(m){
          print(paste0("message: ", m))
        })
    }else{ # not suppressing warnings
      tryCatch(
        scriptResults <- callr::r(rogueScript,
                                  args = list(tmp_full_path),
                                  show = TRUE, package = TRUE),
        error = function(e){
          print(paste0("error: ", e$parent$call))
          print(e$parent$trace)
        },
        message = function(m){
          print(paste0("message: ", m))
        },
        warning = function(w){
          print(paste0("warning: ", w))
        })
    }

    # test the student's submissions
    # note that scriptResults might not exist if there was an error in the tryCatch block
    if( exists("scriptResults") ){
      lr <- testthat::ListReporter$new()
      out <- testthat::test_file(your_test_file,
                                 reporter = lr,
                                 env = scriptResults)

      # parse the output
      score_data[student_num,1] <- tmp_full_path
      for(q in (1:number_questions)){

        # true or false if question was correct
        assertionResults <- lr$results$as_list()[[q]]$results
        success <- all(sapply(assertionResults,
                              methods::is,
                              "expectation_success"))

        # TODO incorporate point values
        if(success){
          score_data[student_num, q+1] <- 1
        }else{
          score_data[student_num, q+1] <- 0
        }
      }

    }else{
      print("assigning all zeros for this student due to bug in submissions")
      score_data[student_num,1] <- tmp_full_path
    }

    # increment
    student_num <- student_num + 1
  }

  # make the column names prettier before returning everything
  colnames(score_data)[-1] <- sapply(trial_test, `[[`, "test")
  return(score_data)
}

################################################################################
################################################################################


#grade_solution <- function() {
  # get the grades
#  results <- calcGrades(submission_dir = './solution/',
#                        your_test_file = "test.r")
#  print(results)
#}

grade_first_blackboard_zip_file_in_wd_and_save_results <- function(test_file_path) {

  # make sure that we have a filepath
  if (missing(test_file_path)) {
    stop('Please provide the url for the test file')
  }

  # Extract the first zip in this folder into submissions.
  files <- list.files(pattern = '.zip$')
  if (length(files) != 1) {
    stop('Could not find a zip in the working directory')
  }
  if (file.exists('./submissions')) {
    unlink('./submissions', recursive = TRUE)
  }
  unzip(files[1], exdir = 'submissions', overwrite = TRUE)


  # Get list of files to grade
  hw_files <- list.files(path = './submissions/',
                         pattern = '_attempt_(.*).R',
                         ignore.case = TRUE)

  # Add path
  hw_files <- paste('./submissions/', hw_files, sep = '')

  # Delete text files
  del_files <- list.files(path = './submissions/',
                         pattern = '.txt',
                         ignore.case = TRUE)
  # Add path
  if (length(del_files) > 0) {
    del_files <- paste('./submissions/', del_files, sep = '')
    file.remove(del_files)
  }



  # get the grades
  results <- calcGrades(submission_dir = './submissions/',
                       your_test_file = test_file_path)



  # Convert tibble of numerical values into a vector of summarized text
  # Example: summary <- select(results, -id)
  toText <- function(t_i_values) {
    col_names <- names(t_i_values)
    vector_of_results <- c()

    for (i in 1:nrow(t_i_values)) {
      row <- t_i_values[i,]

      row_as_text <- mapply(function(x,i) {
                    return(paste('[Q', '. ', col_names[i], '] ',
                                 ifelse(x == 1, 'Yes', 'No'), sep = ''))
                  },
                  row,
                  seq_along(row))

      vector_of_results <- append(vector_of_results, paste(row_as_text, collapse = '<br/> '))
    }

    return(vector_of_results)
  }


  # Create a cleaned up version of the results
  # Has a standardized layout useful for dealing with dynamic tests
  t_results <- tibble(results) %>%
    mutate(id = str_split_i(id, '_',2),
           comment = toText(select(results, -id)),
           points = rowSums(across(where(is.numeric)))
           ) %>%
    select(id, comment, points) %>%
    arrange(id)

  print(t_results)



  # Remove prior csv files from this process running
  if (file.exists('blackboard_updated_file.csv')) {
    file.remove('blackboard_updated_file.csv')
  }

  # Open downloaded grading file
  csv_files <- list.files(pattern = '.csv$')
  if (length(csv_files) != 1) {
    stop('Error, wrong number of csv files in folder')
  }

  print('Updating grades')
  t_grades_raw <- tibble(read.csv(csv_files[1],
                                   header = TRUE,
                                   sep = ',',
                                   quote = '"',
                                   check.names = FALSE))
  if (length(names(t_grades_raw)) != 12) {
    stop('Error, wrong number of columns')
  }

  # Change name of point column to 'grade_as_text', saving old as old_name
  grade_columns <- names(t_grades_raw)
  old_name <- grade_columns[8]
  grade_columns[8] <- 'grade_as_text'
  names(t_grades_raw) <- grade_columns

  # Update all found items
  t_grades <- t_grades_raw %>%
    left_join(t_results, by = c('Username' = 'id')) %>%
    mutate(grade_as_dbl = ifelse(grade_as_text == '' | is.na(grade_as_text), 0, as.double(grade_as_text)),
           points = ifelse(is.na(points), 0, points),
           grade_as_text = paste(ifelse(points > grade_as_dbl, points, grade_as_dbl)),
           grade_as_text = ifelse(grade_as_text == 'NA', '', grade_as_text),
           `Feedback to Learner` = paste(`Feedback to Learner`,
                                       ifelse(is.na(comment), '', comment),
                                       sep = ''),
           `Feedback Format` = 'HTML',
           `Notes Format` = 'SMART_TEXT') %>%
    mutate(comment = NULL,
           points = NULL,
           grade_as_dbl = NULL)

  # Write
  grade_columns[8] <- old_name
  names(t_grades) <- grade_columns
  write.csv(t_grades, 'blackboard_updated_file.csv', row.names = FALSE, na = "")


  print('Completed process!')

}