
create.mastersindatascience <- function() {

  source <- "https://www.mastersindatascience.org/specialties/bachelor-degrees-in-data-science/"
  
  ds <- xml2::read_html(source) 

  df <- ds %>% 
    rvest::html_table() %>% 
    .[[1]] %>% 
    dplyr::rename(degree_type = `Degree Type`,
                  uni = University,
                  delivery = Delivery,
                  program_length = `Program Length`) %>% 
    dplyr::mutate(uni_id = paste0("u", seq(1, dplyr::n(), 1)),
                  degree_desc = degree_type)
  
  university_xwalk <- df %>% 
    dplyr::select(uni_id, uni)

  df <- df %>% 
    dplyr::bind_cols(url = ds %>% 
                       rvest::html_nodes("table > tbody > tr > td > a") %>% 
                       rvest::html_attr("href")) %>% 
    dplyr::mutate(degree_type = tolower(degree_type),
                  degree_type = gsub("\\.", "", degree_type),
                  BS = ifelse(grepl("bs", degree_type), 1, 0),
                  BA = ifelse(grepl("ba", degree_type), 1, 0),
                  Major = ifelse(grepl("major", degree_type) | 
                                 grepl("undergraduate", degree_type) |
                                 grepl("bachelor", degree_type) |
                                 BS == 1 | BA == 1, 1, 0),
                  Minor = ifelse(grepl("minor", degree_type), 1, 0),
                  source = source,
                  degree_type = "Bachelors"
                  )
  
  # program_xwalk <- df %>% 
  #   dplyr::select(id, university, degree_desc, url, program_type, BA_ds, BS_ds, major, minor, delivery, source)
  # 
  program_summary <- df %>% 
    dplyr::select(uni_id, uni, degree_type, url, Major, Minor, source) %>% 
    tidyr::gather(program_type, value, c(Major, Minor)) %>% 
    dplyr::filter(value == 1) %>% 
    dplyr::group_by(uni_id) %>% 
    dplyr::mutate(program_id = paste(uni_id, paste0("p", seq(1, dplyr::n(), 1)), sep = "-")) %>% 
    dplyr::select(-value)
   
  
  write.csv(university_xwalk, "./data/university_xwalk.csv", row.names = FALSE)
  write.csv(program_summary, "./data/program_summary.csv", row.names = FALSE)
  # write.csv(program_xwalk, "./data/program_xwalk.csv", row.names = FALSE)
  
  return(df)
}

create.blogcollegevine <- function() {

  url <- "https://blog.collegevine.com/the-list-of-all-u-s-colleges-with-a-data-science-major/"

  df <- xml2::read_html(url) %>% 
    rvest::html_table() %>% 
    .[[1]]

  return(df)
}

create.discoverdatascience <- function() {
  
  url <- "https://www.discoverdatascience.org/programs/bachelors-in-data-science/"
  
  df <- xml2::read_html(url) %>% 
    rvest::html_node("body > div.site-container > div > div > div > main > article > div > p:nth-child(27)")
  
}