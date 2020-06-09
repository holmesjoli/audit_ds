#' @title Create University
#' @description Creates a table unique on university
#' @export
create_university <- function(df) {

  uni <- df %>% 
    dplyr::distinct(uni_name) %>% 
    dplyr::mutate(uni_id = paste0("u", seq(1, dplyr::n(), 1))
    ) %>% 
    dplyr::select(uni_id, uni_name)

  write.csv(uni, "./data/db/university.csv", row.names = FALSE)
  return(uni)
}

#' @title Create program
#' @description Creates a table unique on program
#' @export
create_program <- function(df, uni) {

  program <- df %>%
    dplyr::select(uni_name, program_name, grad, program_type, degree_type, url) %>%
    dplyr::left_join(uni) %>%
    dplyr::group_by(uni_id) %>%
    dplyr::mutate(program_id = paste0(uni_id, "_p", seq(1, dplyr::n(), 1))
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(uni_id, program_id, program_name, grad, program_type, degree_type, url)

  test <- program %>% 
    dplyr::filter(program_type == "Minor")
  assertthat::assert_that(all(test$degree_type == test$program_type))

  test <- program %>% 
    dplyr::filter(program_type == "Certificate")
  assertthat::assert_that(all(test$degree_type == test$program_type))

  test <- program %>% 
    dplyr::filter(!program_type %in% c("Major", "Minor", "Certificate"))
  assertthat::assert_that(all(test$grad == "Graduate"))

  test <- program %>% 
    dplyr::filter(program_type %in% c("Major", "Minor"))
  assertthat::assert_that(all(test$grad == "Undergraduate"))

  write.csv(program, "./data/db/program.csv", row.names = FALSE)

  return(program)
}

#' @title Write out data frame
#' @export
write_out_df <- function(cls_list, df_name) {

  cls_list <- plyr::compact(cls_list)

  df <- cls_list %>%
    purrr::map(magrittr::extract(df_name))

  df <- do.call("rbind", df)
  
  df <- df %>%
    dplyr::distinct()

  write.csv(df, file.path("./data/db/", paste0(df_name, ".csv")), row.names = FALSE)

  return(df)
}