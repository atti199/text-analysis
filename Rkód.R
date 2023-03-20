setwd("/Users/user/Documents/text-based szakdoga")
getwd()
# install.packages("arrow")
# install.packages("tdyverse")
library(arrow)
library(tidyverse)

# fájl beolvasása

df <- read_parquet("text1.parquet")

# Dátum kinyerése a text oszlopból (legyen benne AM vagy PM ÉS egy hónap neve (vagy 2 esetben a "yesterday"))

df$Date <- NA

df$Date[((grepl("PM", df$text) | grepl("AM", df$text)) &(grepl("January", df$text) | 
                                                           grepl("February", df$text) |
                                                           grepl("March", df$text) |
                                                           grepl("April", df$text) |
                                                           grepl("May", df$text) |
                                                           grepl("June", df$text) |
                                                           grepl("July", df$text) |
                                                           grepl("August", df$text) |
                                                           grepl("September", df$text) |
                                                           grepl("October", df$text) |
                                                           grepl("November", df$text) |
                                                           grepl("December", df$text)|
                                                           grepl("Yesterday", df$text)|
                                                           grepl("Today", df$text)
                                                           )
)] <- df$text[((grepl("PM", df$text) | grepl("AM", df$text)) &(grepl("January", df$text) | 
                                                              grepl("February", df$text) |
                                                              grepl("March", df$text) |
                                                              grepl("April", df$text) |
                                                              grepl("May", df$text) |
                                                              grepl("June", df$text) |
                                                              grepl("July", df$text) |
                                                              grepl("August", df$text) |
                                                              grepl("September", df$text) |
                                                              grepl("October", df$text) |
                                                              grepl("November", df$text) |
                                                              grepl("December", df$text) |
                                                              grepl("Yesterday", df$text)  |
                                                              grepl("Today", df$text) )
)]

# dátumok alkalmazása az összes alatti sorra

df <- df%>%
  fill(Date, .direction = "down")

# dátumok és helyek, rövid karakterszámú szövegek kiszedése a textből

df <- df[!((grepl("PM", df$text) | grepl("AM", df$text)) &(grepl("January", df$text) |
                                                                 grepl("February", df$text) |
                                                                 grepl("March", df$text) |
                                                                 grepl("April", df$text) |
                                                                 grepl("May", df$text) |
                                                                 grepl("June", df$text) |
                                                                 grepl("July", df$text) |
                                                                 grepl("August", df$text) |
                                                                 grepl("September", df$text) |
                                                                 grepl("October", df$text) |
                                                                 grepl("November", df$text) |
                                                                 grepl("December", df$text) |
                                                                 grepl("Yesterday", df$text)  |
                                                                 grepl("Today", df$text) )
),]

df <- filter(df, nchar(df$text)>10)
df <- df[!grepl("added", df$text),]
df <- df[!(grepl("France", df$text) | grepl("Egypt", df$text)| grepl("Hungary", df$text)),]


# df <- df[c(1,5)]

df$Words <- NA
List <- strsplit(df$text, " ")
df2 <- data.frame(Id=rep(df$Date, sapply(List, length)), Words=unlist(List))
df2$Words <- tolower(df2$Words)
text_wordcounts <- df2  %>% count(Words, sort = TRUE)  %>% filter(n>100)

ggplot(data=text_wordcounts, aes(x=reorder(Words, n),n))+
  geom_col()+
  coord_flip()

