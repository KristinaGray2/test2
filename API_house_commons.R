install.packages("rvest")
library(rvest)
library(tidyverse)

basic <- read_html("https://votes.parliament.uk/Votes/Commons/Division/1054")

title <- basic %>% html_nodes("div") %>% html_text()
print(ti/.as;dkojihjntle)

print(a)

basic$html$head
