electives <- function(cls, ...) UseMethod("electives")

electives.default <- function(cls, ...) {
  return(NULL)
}

#' @note Courses from: https://www.eng.auburn.edu/comp/files/dse-brochure.pdf
electives.u6_p1 <- function(cls, ...) {

  cls$electives <- cls$catalog %>% 
    dplyr::filter(course_id > 6000) %>% 
    dplyr::select(course_area_abb, course_number, course_id) %>% 
    dplyr::mutate(program_id = cls$program_id,
                  required = FALSE,
                  elective = TRUE)

  return(cls)
}

