---
title: "Inspecting 'People' attribute in the EDH dataset"
author: "Petra Hermankova"
date: "20/10/2020"
output: html_document
---

# Initial setup

## Setup of the environment:

```{r setup, echo=TRUE, message=FALSE}
devtools::install_github("sdam-au/sdam") # loading SDAM custom package, if not working try devtools::install_github("mplex/cedhar", subdir="pkg/sdam")
#devtools::install_github("mplex/cedhar", subdir="pkg/sdam")
library(tidyverse)
library(sdam)
library(jsonlite)
library(getPass)
```

# Loading data as anonymous use (no credentials needed)

Please ignore and close the pop-up window asking for username and login. The data will then download itself without login credentials.
```{r}
resp = request("EDH_attrs_cleaned_2020-09-30_sample.json", path="/public/b6b6afdb969d378b70929e86e58ad975", method="GET")
```
Now you can move to the next step and Make a tibble from the request's resp.

3. Make a list and tibble from the request function
```{r}
list_json <- jsonlite::fromJSON(resp)
EDH_tibble <- as_tibble(list_json)
```

Display the first 6 records
```{r}
head(EDH_tibble)
```


# Exploration of the attribute 'People'

People attribute contains nested lists of other attributes, that I have difficulties accessing. I am interested in answering the following questions, but I don't know how... Any suggestions are appreaciated.

```{r}
head(EDH_tibble$people)
```



0. What are names of all attributes within the 'people' attribute
1. How many people are in total in the EDH database?
2. How many people there are per inscription (average, min, max)
3. What is the gender ratio of people on inscriptions? (male, female, NA)
4. What are the names of unique values in the 'status' attribute? 
5. What is the ratio of different statuses, e.g. slave vs freedman
6. How many inscriptions have ‘Age’ category?
7. What is the average age of people (years, months, days)


When I have the pointers how to get the data out of 'people' I will be looking at the following specific cases:

Specific case (funerary inscriptions; attribue 'type_of_inscription_clean' == 'epitaph')
1. How many people are on funerary inscriptions (total, average, min, max)
2. What is the ratio of genders on funerary inscriptions (male, female, NA)
3. What is the age of people on funerary inscriptions (total number of insc with age, average, min, max)
4. What is the average age of people on funerary inscriptions by province

Specific case (gender composition)
1. Ratio of men/women on different types of inscriptions (attribue 'type_of_inscription_clean')

# Petra's solution using tidyverse
```{r}
EDH_unnested<- EDH_tibble %>% 
  unnest(people)
```

## What are names of all attributes within the 'people' attribute
```{r}
setdiff(names(EDH_unnested), names(EDH_tibble))
```

## How many people are in total in the EDH database?

One way through gender
```{r}
EDH_unnested %>% 
  count(gender, sort = TRUE) -> gender
sum(gender$n)
```

Second way through nrow
```{r}
nrow(EDH_unnested)
```


## How many people there are per inscription (average, min, max)
```{r}
summary(as.numeric(EDH_unnested$person_id))
```

## What is the gender ratio of people on inscriptions? (male, female, NA)
```{r}
EDH_unnested %>% 
  count(gender, sort = TRUE)
```

## What are the names of unique values in the 'status' attribute?
```{r}
EDH_unnested$status %>% 
  unique()
```


## What is the ratio of different statuses, e.g. slave vs freedman

```{r}
EDH_unnested %>% 
  select(status) %>% 
  count(status, sort = TRUE)
```


## How many inscriptions have ‘Age’ category?

```{r}
EDH_unnested %>% 
  select('age: days', 'age: months', 'age: hours', 'age: years') %>%
  filter(!is.na(EDH_unnested$`age: years`) | !is.na(EDH_unnested$`age: months`) | !is.na(EDH_unnested$`age: days`) |!is.na(EDH_unnested$`age: hours`))
```


What are the unique values for years
```{r}
unique(EDH_unnested$`age: years`)
```

How many people have their age stated in years
```{r}
sum(!is.na(EDH_unnested$`age: years`))

EDH_unnested %>% 
  select('age: days', 'age: months', 'age: hours', 'age: years') %>%
  filter(!is.na(EDH_unnested$`age: years`))
```

How many people have their age stated in months
```{r}
sum(!is.na(EDH_unnested$`age: months`))

EDH_unnested %>% 
  select('age: days', 'age: months', 'age: hours', 'age: years') %>%
  filter(!is.na(EDH_unnested$`age: months`))
```

How many people have their age stated in days

```{r}
sum(!is.na(EDH_unnested$`age: days`))

EDH_unnested %>% 
  select('age: days', 'age: months', 'age: hours', 'age: years') %>%
  filter(!is.na(EDH_unnested$`age: days`))
```

How many people have their age stated in hours
```{r}
sum(!is.na(EDH_unnested$`age: hours`))

EDH_unnested %>% 
  select('age: days', 'age: months', 'age: hours', 'age: years') %>%
  filter(!is.na(EDH_unnested$`age: hours`))
```


## What is the average age of people (years, months, days)

Not ideal method as it skips a lot of textual descriptions
```{r}
summary(as.numeric(EDH_unnested$`age: years`))
summary(as.numeric(EDH_unnested$`age: months`))
summary(as.numeric(EDH_unnested$`age: days`))
summary(as.numeric(EDH_unnested$`age: hours`))
```

Better method using regular expressions to detect years and converting them as numeric
```{r}
EDH_unnested %>% 
  select('age: days', 'age: months', 'age: hours', 'age: years') %>% 
  mutate(age_years = as.numeric(str_extract(EDH_unnested$'age: years', pattern = "[:digit:]+"))) %>% 
  mutate(age_months = as.numeric(str_extract(EDH_unnested$'age: months', pattern = "[:digit:]+"))) %>%
  mutate(age_days = as.numeric(str_extract(EDH_unnested$'age: days', pattern = "[:digit:]+"))) %>%
  mutate(age_hours = as.numeric(str_extract(EDH_unnested$'age: hours', pattern = "[:digit:]+"))) -> ages
```

```{r}
summary(ages$age_years)
summary(ages$age_months)
summary(ages$age_days)
summary(ages$age_hours)
```

# More complex questions

## What is the average age of people on funerary inscriptions by province
```{r}
EDH_unnested %>% 
  filter(type_of_inscription_clean == "epitaph") -> epitaph
```

```{r, fig.height=8}
epitaph %>% 
  select(`age: years`, province_label_clean) %>% 
  mutate(age_years = as.numeric(str_extract(epitaph$'age: years', pattern = "[:digit:]+"))) %>% 
  count(age_years, province_label_clean) %>% 
  ggplot(aes(x=age_years, y= province_label_clean)) + geom_point(alpha=0.5, color="darkblue") +
  theme_minimal()
ggsave("Age_years_provinces.png", width = 8, height = 8) 
```


