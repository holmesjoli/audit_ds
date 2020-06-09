courses <- function(cls, ...) UseMethod("courses")

courses.default <- function(cls, ...) {
  return(NULL)
}

#' @title Create courses for program id: u6_p1
#' @note Courses from: https://www.eng.auburn.edu/comp/files/dse-brochure.pdf
#' @export
courses.u6_p1 <- function(cls, ...) {

  df <- data.frame(program_id = cls$program_id,
                   required = c(rep(TRUE, 7), rep(FALSE, 3)),
                   elective = c(rep(FALSE, 7), rep(TRUE, 3)),
                   course_area_abb = c(rep("COMP", 3), rep("STAT", 4), rep("STAT", 2), "COMP"),
                   course_number = c(6120, 6130, 6630, 6000, 6600, 6650, 7940, rep(NA, 3)),
                   or_groups = NA,
                   stringsAsFactors = FALSE) %>% 
    dplyr::mutate(course_id = 
                    dplyr::case_when(!is.na(course_area_abb) & !is.na(course_number) ~ 
                                       paste(course_area_abb, course_number)
                    )
    )

  cls$courses <- df

  return(cls)
}

#' @title Create courses for program id: u7_p1
#' @note Courses from: http://www.augie.edu/academics/majors-and-programs/data-science
#' @export
courses.u7_p1 <- function(cls, ...) {
 
  df <- data.frame(program_id = cls$program_id,
                   required = c(rep(TRUE, 11), rep(FALSE, 10)),
                   elective = c(rep(FALSE, 11), rep(TRUE, 10)),
                   course_area_abb = c(rep("COSC", 6), rep("MATH", 5), rep("BIOL", 3), rep("COSC", 4), rep("MATH", 3)),
                   course_number = c(210, 211, 212, 215, 315, 322, 151, 152, 315, 320, 327, 233, 270, 360, 241, 342, 260, 380, 220, 345, 397),
                   or_groups = c(rep(NA, 11), rep(1, 3), rep(2, 2), rep(3, 2), rep(4, 3)),
                   stringsAsFactors = FALSE) %>% 
    dplyr::mutate(course_id = 
                    dplyr::case_when(!is.na(course_area_abb) & !is.na(course_number) ~ 
                                       paste(course_area_abb, course_number)
                    )
    )
  
  cls$courses <- df
  
  return(cls) 
}

#' @note Courses from: http://augie.smartcatalogiq.com/2019-2020/2019-2020-Undergraduate-General-Catalog/Academic-Program/Data-Science/Data-Science-Minor
#' @export
courses.u7_p2 <- function(cls, ...) {

  df <- data.frame(program_id = cls$program_id,
                   required = c(rep(TRUE, 5), rep(FALSE, 3)),
                   elective = c(rep(FALSE, 5), rep(TRUE, 3)),
                   course_area_abb = c(rep("COSC", 5), "BIOL", "ECON", "MATH"),
                   course_number = c(210, 212, 215, 315, 322, 270, 270, 315),
                   or_groups = c(rep(NA, 5), rep(1, 1), rep(2, 1), rep(3, 1)),
                   stringsAsFactors = FALSE) %>% 
    dplyr::mutate(course_id = 
                    dplyr::case_when(!is.na(course_area_abb) & !is.na(course_number) ~ 
                                       paste(course_area_abb, course_number)
                    )
    )

  cls$courses <- df

  return(cls) 

}