
library(magrittr)
library(rvest)
library(tibble)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(janitor)
library(lubridate)
library(tidyquant)
library(timetk)
library(ggplot2)
library(dygraphs)
library(listviewer)

# get CPI data to be able to convert incomes into real terms
cpinsa <-
    tq_get("CPIAUCNS", get = "economic.data", from = "1913-01-01", to = "2018-12-31") %>%
    mutate(year = year(date)) %>%
    select(-date) %>%
    group_by(year) %>%
    summarise(cpi = mean(price)) %>%
    ungroup()

# get data on federal income tax brackets from https://www.tax-brackets.org/federaltaxtable/
print("Scraping federal income tax brackets data from https://www.tax-brackets.org/federaltaxtable. This takes a while, please hold on.")

yfst <- 1913
ylst <- 2018

federal_tax_brackets <-
    tibble(taxyear = yfst:ylst) %>%
    mutate(link = str_c("https://www.tax-brackets.org/federaltaxtable/", taxyear),
           webpage = map(link, read_html),
           types = map(webpage, . %>%
                           html_nodes(".col-md-6 b") %>%
                           html_text() %>%
                           str_extract(pattern = "(?<=Federal - [0-9]{4}).*(?=Tax Brackets)") %>%
                           str_trim() %>%
                           str_replace_all(pattern = " ", replacement = "_") %>%
                           str_to_lower()),
           brackets = map(webpage, . %>%
                              html_table() %>%
                              map(clean_names) %>%
                              # map(~mutate_all(.x, funs(str_replace_all(.,"[\\$,\\+%]", "") %>% as.numeric()))) %>%
                              map(~mutate_all(.x, funs(parse_number)))) %>%
                          set_names(str_c("ty", taxyear)),
           brackets = map2(brackets, types, ~.x %>% magrittr::extract(1:length(.y)) %>% set_names(.y)),
           # add missing tables for Married Filing Jointly for year 1949-1954: same tax rates as Married Filing Separately by bracket cutoffs double
           # in addition fix mistakes for years 1967, 2011, 2012
           brackets = brackets %>%
                           modify_if(taxyear %in% c(1949:1954, 1967),
                                     ~update_list(.x, married_filing_jointly = ~married_filing_separately %>%
                                                      mutate(tax_bracket = 2*tax_bracket))) %>%
                           modify_if(taxyear %in% c(2011:2012),
                                     ~update_list(.x, married_filing_jointly = ~married_filing_jointly %>%
                                                      mutate(tax_rate = if_else(tax_bracket == 0, 10, tax_rate))))
           )

print("Done. All your base are belong to us.")

federal_tax_brackets %>%
    pull("brackets") %>%
    str(list.len = 3, max.level = 2)

federal_tax_brackets %>%
    pull("brackets") %>%
    jsonedit(mode = "view")

# rearrange tax brackets data in a way that can be easily used by the shiny app
federal_tax_brackets_clean <-
    federal_tax_brackets %>%
    pull("brackets") %>%
    transpose() %>%
    map( ~.x %>%
             bind_rows(.id = "tax_year") %>%
             as_tibble() %>%
             mutate(tax_year = parse_number(tax_year)) %>%
             group_by(tax_year) %>%
             mutate(tax_rate = tax_rate/100,
                    tax_bracket_low = tax_bracket,
                    tax_bracket_upp = c(tax_bracket[2:n()], Inf)) %>%
             ungroup() %>%
             select(tax_year, tax_bracket_low, tax_bracket_upp, tax_rate))

min_and_max_tax_rate <-
    federal_tax_brackets_clean %>%
    map(~.x %>%
            group_by(tax_year) %>%
            summarise_at(vars(tax_rate), funs(min, max)))

save(federal_tax_brackets, federal_tax_brackets_clean, cpinsa, min_and_max_tax_rate,
     file = "us_tax_brackets.Rdata")
