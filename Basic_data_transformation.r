# Learn about load data file
#csv
# txt ; tab
# html
# json
# xml
# sql databases
# tidyr long->wide#


# library for learn
library(tidyverse) # everything for data project
library(lubridate) # date
library(jsonlite) # json
library(rvest) # web scraping
library(readxl) # excel
library(xml2) # xml

# get working directory
getwd()

# read csv file
student1 <- read_csv("data/student1.csv")

# read txt file
student2 <- read_delim("data/student2.txt", delim = ";")

# read txt2 file
student3 <- read_delim("data/student3.txt", delim = "\t")

# read excel file
student4 <- read_excel("data/student4.xlsx", "Sheet1")

student5 <- read_excel("data/student4.xlsx", 2)

student6 <- read_excel("data/student4.xlsx", 3, skip = 3)

## combine all six data frames into one
list_students <- list(
  student1,
  student2,
  student3,
  student4,
  student5,
  student6
)

all_students <- bind_rows(list_students)


# write csv file
write_csv(all_students, "data/all_students.csv")

# read json file
library(jsonlite)

employee <- data.frame(fromJSON("data/employees.json"))

glimpse(employee)

# convert data type
employee$ID <- as.numeric(employee$ID)
employee$Salary <- as.numeric(employee$Salary)

# load library for working with date
library(lubridate)

### Ex.1 
x <- "2022-01-15"
class(x)
date_x <- ymd(x)
class(date_x)

ymd("2022 september 9")
dmy("15 Jan 2022")
mdy("Apr 01 2022 ")

employee$StartDate <- mdy(employee$StartDate)

glimpse(employee)

### เวลาทำงานใน data ต้องมั่นใจว่า data type ถูกต้อง

year(employee$StartDate)
month(employee$StartDate, label=TRUE)
day(employee$StartDate)
wday(employee$StartDate)
wday(employee$StartDate, label=TRUE)

# create new column 
employee$year <- year(employee$StartDate)
employee$month <- month(employee$StartDate, label=TRUE)



## read HTML file
library(rvest)
# web scraping

html <- read_html("data/school_website.html")

html %>% html_node("title") %>% html_text()
html %>% html_node("h2") %>% html_text()

html %>% html_nodes("h2") %>% html_text()
html %>% html_nodes("li") %>% html_text()

teachers <- html %>% html_nodes("ul li") %>% html_text()
companies <- html %>% html_nodes("ol li") %>% html_text()

df <- data.frame(teachers, companies)


### Ex.1 
## %>% 
x <- 1:5
sum(x)
x %>% sum()
x %>% sum


# load data from XML format
library(xml2)

xml_dog <- read_xml("https://gist.githubusercontent.com/toyeiei/7495caf051daec2d45d70cb2c3daa251/raw/aaccdae3a6f2c34e9b2c326bdb9b578565c06d37/test_email.xml")

xml_dog %>% xml_nodes("from") %>% xml_text()
xml_dog %>% html_nodes("from") %>% html_text()

## -------------------------------------------------------------------------------------- ##


## IMDB project (web scraping)

url <- "https://www.imdb.com/chart/top/"

imdb <- read_html(url)

imdb %>% 
html_nodes("td.titleColumn") %>% 
html_text() %>%
str_remove_all("\n") %>%
str_trim() %>%
str_replace_all("\\s+", " ") %>% # \\s ดึงข้อมูลที่มี white space
head()

### ------- Project ดึง ชื่อหนัง ปี rating ในIMDB






## --------------------------------------------------##

# dplyr
# data transformation
# 1. select
# 2. filter
# 3. arrange
# 4. mutate
# 5. summarise

library(tidyverse)
mtcars <- mtcars %>% 
  rownames_to_column()

mtcars <- mtcars %>% as_tibble() # tibble will print 10 row up

# select column
df <- mtcars %>% 
  select(milePerGallon = mpg,
         horsePower = hp,
         weight = wt)

## Data pipeline in R
result <- mtcars %>%
  select(model = rowname, mpg, hp, wt) %>%
  filter(str_detect(model, "^[MT]")) %>%
  mutate(segment = str_sub(model, 1, 1)) %>%
  group_by(segment) %>%
  # arrange(segment, -mpg) %>% # - is desc
  summarise( avg_mpg = mean(mpg),
             sum_mpg = sum(mpg),
             median_wt = median(wt),
             n = n())

write_csv(result, "data/result.csv")

### dplyr sampling
set.seed(42)
mtcars %>% 
  sample_n(5)

### dplyr pipeline example
mtcars %>%
  select(-rowname, -hp) %>% # - delete column
  mutate(am = as.factor(ifelse(am == 0, "Auto", "Manual"))) %>%
  group_by(am) %>%
  summarise_all(mean)

## ---------------------------------------- ##

## Connect R to database
library(RSQLite)

# 1. Connect DB
conn <- dbConnect(SQLite(), "data/chinook.db")

# 2. List Tables
dbListTables(conn)

# 3. List Fileds / Columns
dbListFields(conn, "customers")

# 4. Query Data
customers <- dbGetQuery(conn, "SELECT *
           FROM customers LIMIT 5")

# 5. join Tables
my_query <- " SELECT artists.name, albums.title
              FROM artists
              JOIN albums
              ON artists.artistid = albums.artistid"

artist_album <- dbGetQuery(conn, my_query)

# Find artists name start with M
artist_album %>% 
  as.tibble() %>%
  filter(str_detect(Name, "^M"))

# 6. Close Connection
dbDisconnect(conn)



### Connect to PostgreSQL
library(RPostgreSQL)

conn <- dbConnect(PostgreSQL(),
                  user = "srikbkol",
                  password = "tfwPdK4ebvE_JHtbZsC08uWxiZ_gh3Ro",
                  host = "arjuna.db.elephantsql.com (arjuna-01)",
                  port = 5432,
                  dbname = "srikbkol")

## ไม่ทัน11:55:00





## ---------------------------------------------##
## tidy data

# 1. read csv worldphones
worldphone <- read_csv("https://gist.githubusercontent.com/toyeiei/d9e267754d0b7a7045e182b3d0011636/raw/04cf2d5b211dc3df279b36d968fde11ed1c9bb67/worldphone.csv")

# 2. wide to long format 
long_wp <- worldphone %>%
  pivot_longer(N.Amer:Mid.Amer,
               names_to = "Region",
               values_to = "Sales")

# 3. long to wide format
long_wp %>%
  pivot_wider(names_from = Region,
              values_from = Sales)

## In Power BI is Pivot

## ------------------------------------------ ##

## Join DATA

student <- read_excel("data/scholarships.xlsx", 1)
address <- read_excel("data/scholarships.xlsx", 2)
scholarship <- read_excel("data/scholarships.xlsx", 3)

# join tables
inner_join(student, address, by = "id")
left_join(student, address, by = "id")

student %>% 
  left_join(student, address, by = "id") %>%
  inner_join(scholarship, by = "id")

student <- student %>%
  rename(student_id = id)

student %>%
  left_join(address, by = c("student_id" = "id")) %>%
  inner_join(scholarship, by = c("student_id" = "id"))





