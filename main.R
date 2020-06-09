library(magrittr)

rm(list = ls())

config <- yaml::read_yaml(here::here("./config.yaml"))

sapply(list.files("./R", full.names = TRUE), source, .GlobalEnv)

df <- readr::read_csv("./data/program_summary.csv") %>% 
  dplyr::arrange(uni_name, grad, program_type)

uni <- create_university(df)

prgrm <- create_program(df, uni)

prgrm_cls <- lapply(1:nrow(prgrm),
                    FUN = function(i, prgrm) {
                      structure(list(uni_id = prgrm$uni_id[i],
                                     program_id = prgrm$program_id[i]), 
                           class = c(prgrm$uni_id[i], prgrm$program_id[i]))
                    }, prgrm = prgrm)

prgrm_cls <- lapply(prgrm_cls, FUN = catalog)
catalog_df <- write_out_df(prgrm_cls, df_name = "catalog") %>% 
  dplyr::select(uni_id, course_id, catalog_year, course_number, course_area_abb, course_name, 
                course_area_general, course_area, credit, course_description, ethics_course, course_url)

prgrm_cls <- lapply(prgrm_cls, FUN = electives)
electives_df <- write_out_df(prgrm_cls, df_name = "electives")

prgrm_cls <- lapply(prgrm_cls, FUN = courses)
courses_df <- write_out_df(prgrm_cls, df_name = "courses")
