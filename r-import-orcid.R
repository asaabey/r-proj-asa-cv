## ---------------------------------------------------------
pacman::p_load(
  rorcid,
  rcrossref,
  dplyr,
  purrr,
  janitor,
  here,
  yaml,
  jsonlite,
  humaniformat,
  snakecase,
  purrr
)

rm(list=ls())

file.edit("~/.Renviron")

# https://github.com/noamross/noamross.net/blob/018ec3915dced73a7a66ea56d0ee270efc6e3bac/scripts/get-orcid-data.R

## ----------------------------------------------------------
my_orcid <- "0000-0003-3153-5051"
works <- orcid_works(my_orcid)
#cites <- orcid_citations(my_orcid, cr_format = "citeproc-json")
#cites <- orcid_citations(my_orcid, cr_format = )
cites <- orcid_citations(orcid = my_orcid, cr_format = "citeproc-json")


c1 <- orcid_citations(orcid=my_orcid, put_code = 98188100, cr_format = "citeproc-json")


c2 <- works2 %>% 
  select(put_code) %>% 
  mutate(
    #citation_js = orcid_citations(orcid=my_orcid, put_code = put_code, cr_format = "citeproc-json"))[0]
    citation_js = put_code +1
  )

purrr::map(works2, orcid_citations(orcid=my_orcid, put_code = works2$put_code, cr_format = "citeproc-json")[0])

jobs <- orcid_employments(my_orcid)
edu <- orcid_educations(my_orcid)

set_class <- function(x, new_class) {
  class(x) <- new_class
  x
}

## ------------------------------------------------------------------------
works2 <- works[[my_orcid]]$works %>% 
  clean_names()

cites2 <- cites %>% 
  distinct(ids, .keep_all = TRUE)



exurl1 <- paste0("https://dx.doi.org/",cites2$ids)

cites2 <- filter(cites, put %in% works2[["put-code"]]) %>%
  distinct(ids, .keep_all = TRUE) %>%
  mutate(citation = purrr::map(citation, jsonlite::fromJSON))

publist <- cites2$citation %>% 
  purrr::map(function(x) {
    date <-
      list(
        name = paste0(x$author$family[1], "_", x$created$`date-parts`[1]),
        doi = x$DOI,
        type = "paper",
        title = x$title,
        authors = as.list(paste(x$author$given, x$author$family)),
        date = as.character(as.Date(paste(x$created$`date-parts`, collapse="-"))),
        exurl = paste0("https://dx.doi.org/", x$DOI))
  })
names(publist) <- map_chr(publist, "name")
write_yaml(publist, here("data", "papers_orcid.yaml"))
publist_manual <- yaml::read_yaml(here("data", "papers_manual.yaml"))
publist <- c(publist, publist_manual[!(names(publist_manual) %in% names(publist))])
for (pubname in names(publist_manual)) {
  if (pubname %in% names(publist)) {
    for (field in names(publist_manual[[pubname]])) {
      publist[[pubname]][[field]] <- publist_manual[[pubname]][[field]]
    }
  }
}
write_yaml(publist, here("data", "papers.yaml"))


joblist <- purrr::map(jobs[[my_orcid]][["affiliation-group"]][["summaries"]], function(x) {
  list(
    name = paste0(to_snake_case(paste(x[["employment-summary.organization.name"]], x[["employment-summary.start-date.year.value"]]))),
    title = x[["employment-summary.role-title"]],
    organization = x[["employment-summary.organization.name"]],
    location  = paste0(x[["employment-summary.organization.address.city"]], ", ", x[["employment-summary.organization.address.region"]]),
    start_date = as.Date(paste(
      x[["employment-summary.start-date.year.value"]],
      x[["employment-summary.start-date.month.value"]],
      x[["employment-summary.start-date.day.value"]],
      sep = "-")),
    end_date = as.Date(paste(
      x[["employment-summary.end-date.year.value"]],
      x[["employment-summary.end-date.month.value"]],
      x[["employment-summary.end-date.day.value"]],
      sep = "-"))
  )
})
names(joblist) <- map_chr(joblist, "name")
write_yaml(joblist, here("data", "employment.yaml"), handlers = list(Date = function(x) as.character (x)))

edulist <- purrr::map(edu[[my_orcid]][["affiliation-group"]][["summaries"]], function(x) {
  list(
    name = paste0(to_snake_case(paste(x[["education-summary.organization.name"]], x[["education-summary.end-date.year.value"]]))),
    type = "degree",
    title = x[["education-summary.role-title"]],
    university = x[["education-summary.organization.name"]],
    department = x[["education-summary.department-name"]],
    location  = paste0(x[["education-summary.organization.address.city"]], ", ", x[["education-summary.organization.address.region"]]),
    date = paste(
      x[["education-summary.end-date.year.value"]],
      x[["education-summary.end-date.month.value"]],
      x[["education-summary.end-date.day.value"]],
      sep = "-"))
})
names(edulist) <- map_chr(edulist, "name")
write_yaml(edulist, here("data", "education.yaml"), handlers = list(Date = function(x) as.character (x)))
