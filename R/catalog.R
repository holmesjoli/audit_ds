#' @title
#' @description Searches for ethics in course description and course name
#' @export
ethics_course <- function(df) {

  assertthat::assert_that(all(c("course_description", "course_name") %in% names(df)))

  df <- df %>% 
    dplyr::mutate(ethics_course = ifelse(grepl("ethics", tolower(course_description)) |
                           grepl("ethics", tolower(course_name)),
                         TRUE, FALSE)
    )
  return(df)
}

catalog <- function(cls, ...) UseMethod("catalog")

catalog.default <- function(cls, ...) {
  return(NULL)
}

catalog.u6 <- function(cls,
                       url = "http://bulletin.auburn.edu/coursesofinstruction/",
                       course_areas = c("stat", "comp"),
                       catalog_year = "2019-2020",
                       ...) {
  
  func <- function(course_area, url, 
                   uni_id = cls$uni_id) {
    
    html <- xml2::read_html(paste0(url, course_area)) %>% 
      rvest::html_nodes(".courseblock p")
    
    course_name <- html %>% 
      rvest::html_nodes("strong") %>% 
      rvest::html_text()
    
    course_description <- names <- html %>% 
      rvest::html_text()
    
    df <- data.frame(uni_id = uni_id,
                     course_area_abb = toupper(course_area),
                     course_name = course_name,
                     course_description = course_description,
                     course_url = url,
                     stringsAsFactors = FALSE)
    return(df)
  }
  
  l <- lapply(course_areas, FUN = func, url = url)
  
  df <- do.call("rbind", l)
  
  df <- df %>% 
    dplyr::mutate(course_description = qdap::mgsub(course_name, "", course_description),
                  course_number = as.numeric(stringr::str_extract(course_name, "(?i)(?<=\\D)\\d+")),
                  course_name = qdap::mgsub(paste(course_area_abb, course_number), "", course_name),
                  catalog_year = catalog_year,
                  course_area_general = ifelse(course_area_abb == "STAT", "Statistics", "Computer Science"),
                  course_area = ifelse(course_area_abb == "STAT", "Statistics", "Computer Science and Engineering"),
                  course_id = paste(course_area_abb, course_number),
                  credit = NA) %>% 
    ethics_course()
  
  cls$catalog <- df
  
  return(cls)
}

catalog.u7 <- function(cls,
                       url = "http://augie.smartcatalogiq.com/en/2019-2020/2019-2020-Undergraduate-General-Catalog/Academic-Program/Data-Science/Data-Science-Major",
                       url2 = "http://augie.smartcatalogiq.com/2019-2020/2019-2020-Undergraduate-General-Catalog/Academic-Program/Data-Science/Data-Science-Minor") {
  
  func <- function(url, slice) {

    dfs <- xml2::read_html(url) %>%
      rvest::html_nodes('table')

    dfs <- dfs[slice]

    l <- lapply(dfs, function(df) {
      df %>% 
        rvest::html_table(fill = TRUE) %>% 
        dplyr::rename(course_id = X1,
                      course_name = X2,
                      credit = X3) %>% 
        dplyr::filter(course_id != "")
    })

    df <- do.call("rbind", l)
    
    df <- df %>%
      dplyr::mutate(course_id = gsub(".*/","", course_id))

    links <- xml2::read_html(url) %>%
      rvest::html_node('table') %>% 
      rvest::html_nodes(xpath = "//td/a") %>% 
      rvest::html_attr("href")

    links <- paste0("http://augie.smartcatalogiq.com", links)

    l <- sapply(links, function(link){
      course_description = xml2::read_html(link) %>% 
        rvest::html_node("#main > .desc > p") %>% 
        rvest::html_text()
    }, USE.NAMES = TRUE, simplify = FALSE)

    catalog <- do.call("rbind.data.frame", l) %>% 
      dplyr::rename(course_description = 1) %>% 
      dplyr::bind_cols(data.frame(course_url = links,
                                  stringsAsFactors = FALSE)) %>% 
      dplyr::mutate(course_id2 = gsub(".*/","", links))

    df <- df %>% 
      dplyr::mutate(course_id2 = gsub(" ", "-", course_id),
                    uni_id = cls$uni_id,
                    course_number =  gsub(".* ","", course_id),
                    course_area_abb = gsub(" .*","", course_id),
                    course_area_general = dplyr::case_when(
                      course_area_abb == "COSC" ~ "Computer Science",
                      course_area_abb == "MATH" ~ "Mathematics",
                      course_area_abb == "BIOL" ~ "Biology",
                      course_area_abb == "ECON" ~ "Economics"
                    ),
                    course_area = course_area_general,
                    catalog_year = "2019-2020") %>% 
      dplyr::left_join(catalog) %>% 
      dplyr::select(-course_id2) %>% 
      ethics_course()
  }

  df <- func(url= url, slice = c(1, 3:6))
  df2 <- func(url = url2, slice = c(1, 2))

  df <- df %>%
    dplyr::bind_rows(df2)

  cls$catalog <- df

  return(cls)
}
