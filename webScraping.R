# Retrieving data from the internet in R
# CDSI workshop, Faculty of Science, McGill University
# Dr. Tim Elrick, Geographic Information Centre (GIC)
# February 16, 2022
# CC-BY-SA 4.0

# You need to install the following packages:
# install.packages(c("tidyverse", "sf", "tidyRSS", "lubridate", "tidygeoRSS",
#                    "rnaturalearth", "rvest", "httr", "jsonlite"))

library(tidyverse)
library(sf)
library(lubridate)

# Open Data Portals --------------------------------------------------------

# Taxi stands in Montreal
# Go to https://donnees.montreal.ca/
url <- "https://data.montreal.ca/dataset/a305bf87-97af-4f9c-9142-39e7b39f496c/resource/d89f5ed8-e252-453c-adc3-0fad63568163/download/postestaxi.csv"
taxi <- read_csv(url)

taxi

# If you want to work with Excel files, use the readxl package,
# for SPSS or Stata files, use the haven package,
# and for OpenOffice/LibreOffice files, use the readODS package
#install.packages("readxl", "readODS", "haven")

# RSS feeds ---------------------------------------------------------------

# with the tidyRSS package you can retrieve RSS feeds
library(tidyRSS)

# This is the RSS feed of CBC news headlines:
url <- "https://rss.cbc.ca/lineup/canada.xml"

# tidyfeed() will retrieve the RSS feed as a list
tidyRSS::tidyfeed(url) %>% 
  pull(item_title)

# here a different RSS feed on the current weather in Montreal
feeds <- tidyRSS::tidyfeed("https://weather.gc.ca/rss/city/qc-147_e.xml") 

feeds %>%
  # as the content is contained in the entry_title column, we access it here:
  .$entry_title %>%
  # then we limit it to the current conditions:
  .[str_detect(., "Current")] %>% 
  # use a bit of regex magic to either extract the current condition as such 
  str_extract(., "(?<=:).+") %>% 
  # or just the temperature
  str_extract(., "(-|\\d|\\.)+") %>% 
  as.numeric()

library(tidygeoRSS)
earthquakes <- 
  tidygeoRSS::tidygeo("https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/2.5_day.geojson")

world <- 
  rnaturalearth::ne_countries() %>% st_as_sf()

ggplot() +
  geom_sf(data = world) +
  geom_sf(data = earthquakes,
          mapping = aes(color = mag)) +
  #make robinson projection
  coord_sf(crs = "+proj=robin") +
  #change scale color
  scale_color_viridis_c(direction = -1) +
  theme_void()


# APIs --------------------------------------------------------------------

# the httr package allows you to easily tap into APIs
library(httr)

# You can find a list of publicly available APIs here:
# https://github.com/public-apis/public-apis

# In this example we will use the French governmental geospatial API:
# https://api.gouv.fr/les-api/api-geo
# At the website you go to test API to see the full code and copy it into a
# GET() function of the httr package as below:

url <- "https://geo.api.gouv.fr/communes?nom=Toulon&fields=nom,code,codesPostaux,codeDepartement,codeRegion,population&format=json&geometry=centre"

# GET() retrieves data from an API
ds <- GET(url)

# You can look at what our API call retrieved:
class(ds)
ds$url
ds$status_code   # 200 is a successful response
                 # see https://restfulapi.net/http-status-codes/ for more
                 # status codes
ds$headers       # this shows you the header of your API call
ds$content       # and here is what you are actually after, the content
                 # well, it is in binary format


# one way of making sense of the content is to use rawToChar()
ds$content %>% 
  rawToChar() %>%
  # as the retrieved data is JSON, we have to use fromJSON()
  jsonlite::fromJSON() %>%
  as_tibble()

# a slightly faster way is to use content() from the httr package;
# i.e. you don't need the jsonlite package
ds %>% 
  # content() parses the data into the most appropriate format (it thinks
  # a list would be best) 
  content() %>% 
  # we think a data frame would be best and turn the list into a data frame
  # with reduce() from the purrr package
  reduce(bind_rows) 

 
# We can change the API call to a dynamic call:
name <- "Bordeaux"

GET(paste0("https://geo.api.gouv.fr/communes?nom=", 
           name,
           "&fields=nom,population&format=json&geometry=centre")) %>% 
  content() %>% 
  reduce(bind_rows)

#to get all cities
GET(paste0("https://geo.api.gouv.fr/communes?&fields=nom,population&format=json&geometry=centre")) %>% 
  content() %>% 
  reduce(bind_rows)

# Webscraping -------------------------------------------------------------

# The rvest packages allows you to scrape data from a website:
library(rvest)

# You first have to go to a website that you want to scrape data from, e.g.
# https://climate.weather.gc.ca/climate_data/hourly_data_e.html 
# and choose a preliminary setting

# Now copy the URL for this website, e.g.:
url <- "https://climate.weather.gc.ca/climate_data/hourly_data_e.html?hlyRange=2013-02-13%7C2021-03-08&dlyRange=2013-02-14%7C2021-03-08&mlyRange=%7C&StationID=51157&Prov=QC&urlExtension=_e.html&searchType=stnName&optLimit=yearRange&StartYear=1840&EndYear=2021&selRowPerPage=25&Line=4&searchMethod=contains&Month=3&Day=8&txtStationName=Montreal&timeframe=1&Year=2021"


# then you can modify this URL and make it dynamically 

my_date <- ymd("2022-02-13")

url <- paste0("https://climate.weather.gc.ca/climate_data/hourly_data_e.html?",
              "StationID=51157&Prov=QC&urlExtension=_e.html&searchType=",
              "stnName&searchMethod=contains&Month=", 
              month(my_date), "&Day=", day(my_date),
              "&txtStationName=Montreal&timeframe=1&Year=",
              year(my_date))

# now we have built our own URL: 
url

# To extract data from this URL we feed it into a pipe
url %>% 
  # and read the html content with
  read_html() %>% 
  # html_node() will allow you to navigate within the webpage to the element
  # that you are interested either as CSS selector or as an XPath;
  # to get an CSS selector or XPath you have to inspect the element in the
  # context menu of your browser and then copy the CSS selector or XPath
  # from there into the html_node() function, here it will navigate to the 
  # table on the website
  html_node(xpath = "/html/body/main/div[5]/table") %>%
  # html_table() will turn the retrieved html code into a data frame
  html_table() %>% 
  # now, just onto a bit of data cleaning, as the variable headings contain
  # a 'Definition' suffix that we don't need
  rename_with(~str_remove(.x, "Definition"), contains("Definition")) %>%
  # let's add the date we chose to have a real datetime object
  mutate(TIMELST = paste(my_date, TIMELST) %>% ymd_hm()) 

#extract text
url %>% 
  read_html() %>% 
  # this XPath will navigate to the weather station's name
  html_node(xpath = "/html/body/main/p[2]") %>% 
  # then we retrieve the text with html_text()
  html_text()

# to retrieve more than the first appearance of a CSS selector you use
# html_nodes() instead of html_node()
url %>% 
  read_html() %>% 
  # here, we retrieve all paragraphs from the website; 'p' is the CSS/HTML
  # code for paragraph
  html_nodes(css = "p") %>% 
  html_text()

url %>% 
  read_html() %>% 
  # when you are after a certain CSS class, e.g. 'text-center', you use a dot
  html_nodes(".text-center") %>% 
  html_text()

url %>% 
  read_html() %>% 
  # when you are after a certain CSS ID, e.g. 'rtm-other', you use a hashtag
  html_nodes("#climateNav") %>% 
  html_text()

# Note, that the rvest package only supports retrieving data from plain HTML
# websites. If the data is produced by JavaScript code, you need to use
# the RSelenium package.


# RStudio Addin: datapasta ------------------------------------------------

# For getting data quickly from a website, sometimes the old copy&paste
# approach is best. The datapasta package helps you with pasting it in R
# as the Addins menu in RStudio allows you to paste the data as vector or
# data frame/tibble:

#install.packages("datapasta")


# Twitter -----------------------------------------------------------------

library(rtweet)

## store api keys (these are fake example values; replace with your own keys)
api_key <- "afYS4vbIlPAj096E60c4W1fiK"
api_secret_key <- "bI91kqnqFoNCrZFbsjAWHD4gJ91LQAhdCJXCj3yscfuULtNkuu"
access_token <- "9551451262-wK2EmA942kxZYIwa5LMKZoQA4Xc2uyIiEwu2YXL"
access_token_secret <- "9vpiSGKg1fIPQtxc5d5ESiFlZQpfbknEN1f1m2xe5byw7"

## authenticate via web browser
token <- create_token(
  app = "rstatsjournalismresearch",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

get_token()
search_tweets("#rstats", n=18000) #max = 18K
tw <- search_tweets("@McGill") #goes back 7 days

users_data(tw)
ts_plot(tw)

gic_friends <- get_friends("GIC_McGill")
lookup_users(gic_friends$user_id)

get_followers("GIC_McGill")

get_timeline("GIC_McGill", n = 20)

get_trends("montreal")
