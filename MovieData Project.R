# librarie Required
#install.packages("rvest")
#install.packages('devtools')
#install.packages("tidyverse")
#devtools::install_github("hrbrmstr/omdbapi")

library(rvest)                    # Web Scraping
library(stringr)                  # String manipulation library
library(devtools)                 # use to install packages for non-traditional way (such as github)
library(omdbapi)                  # using omdb api to use IMDB database
library(tidyverse)
library(ggplot2)

# ________________________ Scrapping Yearly box office Data ______________________
# Reading Box Office Mojo
url <- 'https://www.boxofficemojo.com/yearly/'
webpage <- read_html(url)

year_html <- html_nodes(webpage,'b font a')      #Using CSS selectors to scrap the year selection
year_data <- html_text(year_html)

# Total yearly gross earning
earning_html <- html_nodes(webpage, 'tr+ tr td:nth-child(2) font')
earning_data <- html_text(earning_html)

# Total tickets sold
tickets_html <- html_nodes(webpage, 'tr+ tr td:nth-child(4) font')
tickets_data <- html_text(tickets_html)

# Number of movies yearly
movies_html <- html_nodes(webpage, 'tr+ tr td:nth-child(6) font')
movies_data <- html_text(movies_html)

# yearly avg ticket price
ticketprice_html <- html_nodes(webpage, 'tr+ tr td:nth-child(8) font')
ticketprice_data <- html_text(ticketprice_html)

yearlydf <- data.frame('Year' = as.numeric(year_data), 'Total Movies' = movies_data,
                       'Gross Earning(million)' = as.numeric(str_replace_all(earning_data, "[$,]", "")),
                       'Total Tickets Sold(million)' = as.numeric(str_replace_all(tickets_data, "[,]", "")),
                       'Avg Ticket Price($)' = as.numeric(str_replace_all(ticketprice_data, "[$,]", "")),
                       stringsAsFactors = FALSE)

# _____________________ Inflation Adjustment ______________________

# Keeping data uptil 2001 and using the ticket prices for inflation adjustment
yearlydf2011 <- yearlydf[1:18,]
yearlydf2011$Inflation.Adjustment <- yearlydf2011$Avg.Ticket.Price.../yearlydf2011$Avg.Ticket.Price...[18]
names(yearlydf2011) <- c('Year', 'Total.Movies.Released', 'Gross.Earning.million',
                         "Total.Tickets.Sold.million", "Avg.Ticket.Price", "Inflation.Adjustment")
str(yearlydf2011)
yearlydf2011$Gross.Earning.million <- yearlydf2011$Gross.Earning.million*1000000
yearlydf2011$Total.Tickets.Sold.million <- yearlydf2011$Total.Tickets.Sold.million*1000000

# __________________________ Scrapping Yearly Movie Data _____________________________

# Reading each years top 100 earning movie data
listofyear <- c(2018, 2017, 2016, 2015, 2014, 2013, 2012, 2011, 2010,
               2009, 2008, 2007, 2006, 2005, 2004, 2003, 2002, 2001)
Moviedf <- data.frame('Rank' = numeric(), 'Movie Name' = character(), 'Studio Name' = character(),
                      'Total Gross Earning(million)' = numeric(), 'Theater' = numeric(),
                      'Earning Opening Day(million)' = numeric(), 'Year' = numeric(),
                      stringsAsFactors = FALSE)
for (yr in listofyear){
  url2 <- paste0('https://www.boxofficemojo.com/yearly/chart/?yr=',yr,'&p=.htm')
  webpage <- read_html(url2)
  
  # Rank of the Movie
  rank_html <- html_nodes(webpage, 'td td tr+ tr td:nth-child(1) font')
  rank_data <- html_text(rank_html)
  rank_data <- rank_data[1:100]
  # Movie Name
  movie_html <- html_nodes(webpage,'td td b font a')
  movie_data <- html_text(movie_html)
  movie_data <- movie_data[1:100]
  # Studio
  studio_html <- html_nodes(webpage,'tr+ tr td:nth-child(3) a')
  studio_data <- html_text(studio_html)
  studio_data <- studio_data[3:102]
  # Total Domestic Gross
  totearning_html <- html_nodes(webpage,'td td tr+ tr td+ td font b')
  totearning_data <- html_text(totearning_html)
  # Theatres
  theater_html <- html_nodes(webpage,'tr+ tr td:nth-child(5) font')
  theater_data <- html_text(theater_html)
  theater_data <- theater_data[3:102]
  # Opening Earning
  openearn_html <- html_nodes(webpage,'tr+ tr td:nth-child(6) font')
  openearn_data <- html_text(openearn_html)
  openearn_data <- openearn_data[2:101]
  
  df <- data.frame('Rank' = as.numeric(rank_data), 'Movie Name' = movie_data, 'Studio Name' = studio_data,
                   'Total Gross Earning(million)' = as.numeric(str_replace_all(totearning_data, "[$,]", "")), 
                   'Theater' = as.numeric(str_replace_all(theater_data, "[,]", "")),
                   'Earning Opening Day(million)' = as.numeric(str_replace_all(totearning_data, "[$,]", "")),
                   'Year'=yr, stringsAsFactors = FALSE)
  
  Moviedf <- rbind(Moviedf, df)
}

# Remove Brackets and everything inside from Movie Name
Moviedf$Movie.Name <- gsub("\\s*\\([^\\)]+\\)","",as.character(Moviedf$Movie.Name))
# https://stackoverflow.com/questions/24173194/remove-parentheses-and-text-within-from-strings-in-r


# Using the OMDB API to access IMDB database and updating multiple data fields required for analysis
moviedatanotfoundindex <- c()
Moviedf['Rated'] <- ''
Moviedf['Genre'] <- ''
Moviedf['Runtime'] <- ''
Moviedf['Director'] <- ''
# run from here -->
for (i in 1:1000){
  info <- try(movieinfo <- find_by_title(Moviedf$Movie.Name[i], year_of_release = Moviedf$Year[i]), 
              silent = TRUE)
  if('omdb' %in% class(info)) {
    Moviedf[i, 'Rated'] <- movieinfo$Rated[1]
    Moviedf[i, 'Genre'] <- movieinfo$Genre[1]
    Moviedf[i, 'Director'] <- movieinfo$Director[1]
    Moviedf[i, 'Runtime'] <- movieinfo$Runtime[1]
    } 
  else {
    print(i)
    moviedatanotfoundindex <- c(moviedatanotfoundindex, i)
    next
    }
}

# Note: As without being a patreon, only 1000 api calls are allowed for each API Key in a day therefore
# running the same query (from 1000 onwards) in different R Session using a new APIKey.

xyz <- read.csv("Moviedf2.csv")
Moviedf2 <- rbind(Moviedf[1:991,], xyz[992:1800,])
moviedatanotfoundindex2 <- c(moviedatanotfoundindex, 993, 1010, 1014, 1048, 1068, 1075, 1078,
                            1109, 1110, 1118, 1123, 1145, 1164, 1185, 1234, 1244, 1268, 1270, 1298,
                            1350, 1351, 1352, 1357, 1359, 1370, 1418, 1426, 1433, 1436, 1489, 1506,
                            1519, 1532, 1565, 1566, 1628, 1631, 1652, 1753, 1757, 1788)

#write.csv(Moviedf2, "Moviedf_full.csv", row.names = FALSE)

# Note: Getting the data Manually for movieS which cannot be pulled using the query above 
# (due to difference in some movies are named in different websites) again doing that part in the
# 2nd R Session (due to limit in API calls available with each Key)

# Changing the movie names to what is used in IMDB.

for(i in moviedatanotfoundindex2){
  print(Moviedf2$Movie.Name[i])
}

namestouse <- c('The Grinch', "Ocean's Eight", "Christopher Robin", "Instant Family", "Acrimony",
                'Paddington 2', 'Chappaquiddick', 'Split', "John Wick: Chapter 2",
                "Boo 2! A Madea Halloween", "xXx: Return of Xander Cage", "47 Metres Down",
                "Monster Trucks", "Underworld: Blood Wars", "Resident Evil: The Final Chapter",
                "Victoria & Abdul", "Allegiant", "13 Hours", "Insurgent", "Taken 3", "Paddington",
                "The Woman in Black 2: Angel of Death", "Black or White", "Oculus", "Bad Grandpa",
                "Hansel & Gretel: Witch Hunters", "A Madea Christmas", 
                "Temptation: Confessions of a Marriage Counselor", "Pain & Gain", "21 & Over",
                "The Avengers", "The Lorax", "Les Misérables", "Madea's Witness Protection", 
                "Titanic 3D", "Ghost Rider: Spirit of Vengeance", "", "", "Good Deeds", "",
                "Gnomeo & Juliet", "", "Insidious", "Madea's Big Happy Family", 
                "Spy Kids 4: All the Time in the World", "The Debt", "Jackass 3D", "Knight and Day", 
                "Why Did I Get Married Too?", "Hubble 3D", "Step Up 3D", "Love & Other Drugs",
                "Daybreakers", "Brooklyn's Finest", "Monsters vs Aliens", "Fast & Furious",
                "Paranormal Activity", "Madea Goes to Jail", "This Is It", "The Taking of Pelham 123",
                "I Can Do Bad All By Myself", "My Bloody Valentine", "Precious", "Notorious",
                "Under the Sea 3D", "", "Horton Hears a Who!", 
                "Marley & Me", "Hannah Montana and Miley Cyrus: Best of Both Worlds Concert",
                "Meet the Browns", "Harold & Kumar Escape from Guantanamo Bay", "The Family That Preys",
                "Alvin and the Chipmunks", "300", "Fantastic 4: Rise of the Silver Surfer", 
                "I Now Pronounce You Chuck & Larry", "Why Did I Get Married?", 
                "Aliens vs. Predator - Requiem", "Daddy's Little Girls", "Barnyard", 
                "Madea's Family Reunion", "Nanny McPhee", "Deep Sea", "Garfield 2",
                "Yours, Mine & Ours", "Corpse Bride", "Kicking & Screaming", 
                "Diary of a Mad Black Woman", "Miss Congeniality 2: Armed & Fabulous", 
                "The Adventures of Sharkboy and Lavagirl 3-D", "A Series of Unfortunate Events",
                "Starsky & Hutch", "AVP: Alien vs. Predator", "Garfield", "Open Water", 
                "X-Men 2", "Spy Kids 3: Game Over", "Legally Blonde 2: Red, White & Blonde",
                "Phone Booth", "28 Days Later...", "Space Station 3D", "Spy Kids 2: Island of Lost Dreams",
                "Peter Pan II: Return to Neverland", "Kate & Leopold", "13 Ghosts", "Memento")

# Use year with movie: Notorious, idx :980, year: 2009.

# After Re-running the query in 2nd R Sessionand pulling the data
Moviedf_final <- read.csv("Moviedf_full2.csv")

# ______________ Pre-Processing the Data _______________
str(Moviedf_final)

Moviedf_final$Movie.Name <- as.character(Moviedf_final$Movie.Name)
Moviedf_final$Rated <- as.character(Moviedf_final$Rated)
Moviedf_final$ImdB.names.used <- as.character(Moviedf_final$ImdB.names.used)
Moviedf_final$Genre <- as.character(Moviedf_final$Genre)
Moviedf_final$Director <- as.character(Moviedf_final$Director)
Moviedf_final$Runtime <- gsub("[ min]", "", as.character(Moviedf_final$Runtime))
Moviedf_final$Runtime <- as.numeric(Moviedf_final$Runtime)

names(Moviedf_final) <- c(names(Moviedf_final)[1:3],"Total.Movie.Earning.million",
                          names(Moviedf_final)[5:9], "Runtime.min", names(Moviedf_final)[11:12])

# Handling the missing data
# Step 1: Removing rows which do not have data obtained from IMdb. I found eight rows that 
# did not have the data so removing those 8 rows

Moviedf_final2 <- Moviedf_final[-c(50, 656, 678, 682, 694, 733, 865, 993),]

# Finding number of NA values in each column
colSums(is.na(Moviedf_final))

# As we probabaly wont be using the director column for our analysis removing the 'NA' rows present
# in other dataframe
Moviedf_final2 <- Moviedf_final2[complete.cases(Moviedf_final2[,1:10]), ]
Moviedf_final2$Genre <- gsub(" ", "",Moviedf_final2$Genre)

# One-Hot Encoding 

# Converting the Genre variable into dummy variables for better use

Moviedf_final3 <- Moviedf_final2 %>%
  mutate(id = row_number(),                               # add row id (useful for reshaping)
         value = 1) %>%             # add a column of 1s to denote existence
  separate_rows(Genre, sep = "[,]") %>%                  # create one row per name keeping relevant info
  spread(Genre, value, fill = 0) %>%                     # reshape
  select(-id) %>%                                           # remove row id
  cbind(Moviedf_final2$Genre)

names(Moviedf_final3) <- c(names(Moviedf_final3)[1:33], "Genre")

# Create dummy variables for Rated column
Moviedf_final3 <- Moviedf_final3 %>%
  mutate(id = row_number(),                               # add row id (useful for reshaping)
         value = 1,
         Rated2 = paste0("Rating_",Rated)) %>%            # add a column of 1s to denote existence
  spread(Rated2, value, fill = 0) %>%                     # reshape
  select(-id)                                             # remove row id

#write.csv(Moviedf_final3, "Moviedf_full3.csv", row.names = FALSE)

# ____ Merging two datasets ______

Moviedf_final3 <- merge(Moviedf_final3, yearlydf2011, by= 'Year')
Moviedf_final3 <- Moviedf_final3[order(Moviedf_final3$Year, decreasing = TRUE),]

# Inflation Adjusted Earning
Moviedf_final3$Adj.Total.Movie.Earning.million <- Moviedf_final3$Total.Movie.Earning.million/Moviedf_final3$Inflation.Adjustment
Moviedf_final3$Adj.Total.Gross.Earning.million <- Moviedf_final3$Gross.Earning.million/Moviedf_final3$Inflation.Adjustment
Moviedf_final3$Adj.Earning.Opening.Day.million <- Moviedf_final3$Earning.Opening.Day.million./Moviedf_final3$Inflation.Adjustment

# percentage of total earning by a movie
Moviedf_final3$Percentage.Earning <- Moviedf_final3$Adj.Total.Movie.Earning.million*100/Moviedf_final3$Adj.Total.Gross.Earning.million
Moviedf_final3$Percentage.Earning <- round(Moviedf_final3$Percentage.Earning, 3)

# Hot Encoding the studio name
Moviedf_final4 <- Moviedf_final3 %>%
  mutate(id = row_number(),                         # add row id (useful for reshaping)
         value = 1) %>%                             # add a column of 1s to denote existence
  spread(Studio.Name, value, fill = 0) %>%          # reshape
  select(-id)                                       # remove row id

write.csv(Moviedf_final4, "Moviedf_full4.csv", row.names = FALSE)
# ___________________ Data Exploration _________________________

# 1. 
# Top 10 Earning Movies in the 21st Century
Moviedf_final3[order(Moviedf_final3$Adj.Total.Movie.Earning.million, decreasing = TRUE),]$Movie.Name[1:10]

# 2. 
# Top Movie Earning Year in the 21st Century
#unique(Moviedf_final3[order(Moviedf_final3$Adj.Total.Gross.Earning.million, decreasing = TRUE),]$Year)[1:5]
#or
best.earning.yr <- Moviedf_final3[,c("Adj.Total.Gross.Earning.million", "Year")]
best.earning.yr <- best.earning.yr[!duplicated(best.earning.yr),]
best.earning.yr[order(best.earning.yr$Adj.Total.Gross.Earning.million, decreasing = TRUE),]$Year[1:5]

# 3.
# Top Earning Studio in 21st Century
Moviedf_final3$Studio.Name <- as.character(Moviedf_final3$Studio.Name)

highest.earning.studio <- aggregate(Moviedf_final3$Total.Movie.Earning.million, 
                                    by=list(Studio.Name=Moviedf_final3$Studio.Name), FUN=sum)
names(highest.earning.studio) <- c("Studio", "Total Earning")
highest.earning.studio[order(highest.earning.studio$`Total Earning`, decreasing = TRUE),]$Studio[1:5]

# 4.
# Highest Earning Genre in 21st Century
Genre <- Moviedf_final3 %>%
  mutate(id = row_number(),                          # add row id (useful for reshaping)
         value = 1) %>%                              # add a column of 1s to denote existence
  separate_rows(Genre, sep = "[,]")                  # create one row per name keeping relevant info

highest.earning.genre <- aggregate(Genre$Adj.Total.Movie.Earning.million, by=list(Genre=Genre$Genre), FUN=sum)
highest.earning.genre[order(highest.earning.genre$x, decreasing = TRUE),]$Genre[1:5]

# 5. 
# Average Run Time of Highest Earning Studio
avg.runtime.studio <- aggregate(Moviedf_final3$Runtime.min, 
                                    by=list(Studio.Name=Moviedf_final3$Studio.Name), FUN=mean)
names(avg.runtime.studio) <- c("Studio", "Avg.Runtime")
Studio.Info <- merge(highest.earning.studio, avg.runtime.studio, by='Studio')

# 6.
studio.runtime <- Studio.Info[order(Studio.Info$`Total Earning`, decreasing = TRUE),c("Studio", "Avg.Runtime")]
studio.runtime[1:5,]

# 7.
# Highest Earning Rating and their Avg Movie Run Time
highest.earning.rating <- aggregate(Moviedf_final3$Total.Movie.Earning.million, 
                                    by=list(Rating=Moviedf_final3$Rated), FUN=mean)
names(highest.earning.rating) <- c("Rating", "Earning")
highest.earning.rating[order(highest.earning.rating$Earning, decreasing = TRUE),]

# 8.
runtime.rating <- aggregate(Moviedf_final3$Runtime.min, 
                                    by=list(Rating=Moviedf_final3$Rated), FUN=mean)
names(runtime.rating) <- c("Rating", "Avg. Runtime")
rating.info <- merge(highest.earning.rating, runtime.rating, by='Rating')

rating.movies <- as.data.frame(table(Moviedf_final3$Rated))
names(rating.movies) <- c("Rating", "Number of Movies")

merge(rating.info, rating.movies, by='Rating')

# 9.
# Opening Day Earning
Moviedf_final3[order(Moviedf_final3$Adj.Earning.Opening.Day.million, decreasing = TRUE),]$Movie.Name[1:5]

# _______________________ Visualizations __________________________

# 1. Top 20 Movies
X <- Moviedf_final3[order(Moviedf_final3$Adj.Total.Movie.Earning.million, decreasing = TRUE),
                    c("Movie.Name", "Adj.Total.Movie.Earning.million")]
X$Adj.Total.Movie.Earning.million <- X$Adj.Total.Movie.Earning.million/1000000
ggplot(data=X[1:20,], aes(x=reorder(Movie.Name, -Adj.Total.Movie.Earning.million), 
                                 y=Adj.Total.Movie.Earning.million, fill=Movie.Name)) + 
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Movie Name") + ylab("Adj Movie Earning (in millions)") + 
  ggtitle("Top 20 Movie Earnings in 21st Century") +
  theme(plot.title = element_text(size = 18, face = "bold"))

# 2. 
best.earning.yr$Adj.Total.Gross.Earning.million <- best.earning.yr$Adj.Total.Gross.Earning.million/1000000
ggplot(data=best.earning.yr, aes(x=reorder(Year, -Adj.Total.Gross.Earning.million), 
                          y=Adj.Total.Gross.Earning.million, fill=Year)) + 
  geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Year") + ylab("adj Gross Movie Earning (in millions)") + 
  ggtitle("Total Gross Earning in each Year (2001-2018)") +
  theme(plot.title = element_text(size = 18, face = "bold"))

# 3. 
Y <- highest.earning.studio[order(highest.earning.studio$`Total Earning`, decreasing = TRUE),]
Y$`Total Earning` <- Y$`Total Earning`/1000000
ggplot(data=Y[1:10,], aes(x=reorder(Studio, -`Total Earning`), 
                                 y=`Total Earning`, fill=Studio)) + 
  geom_bar(stat="identity", colour="black") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Studio") + ylab("adj Total Gross Earning (in Millions)") + 
  ggtitle("Top 10 Studio's with highest total earning") +
  theme(plot.title = element_text(size = 18, face = "bold"))

# 4.
highest.earning.genre$x <- highest.earning.genre$x/1000000
ggplot(data=highest.earning.genre, aes(x=reorder(Genre, -x), 
                                 y=x, fill=Genre)) + 
  geom_bar(stat="identity", colour="black") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Genre") + ylab("Adj Total Earning (in millions)") + 
  ggtitle("Total Earning for each Genre") +
  theme(plot.title = element_text(size = 18, face = "bold"))

# 5.
ggplot(Moviedf_final3, aes(x=Runtime.min)) + 
  geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..)) +
  stat_function(fun=dnorm,
                color="red",
                args=list(mean=mean(Moviedf_final3$Runtime.min), 
                          sd=sd(Moviedf_final3$Runtime.min))) +
  xlab("Run Time for Movies") + ggtitle("Density Plot for Run time for movies") +
  theme(plot.title = element_text(size = 18, face = "bold"))

# ______________________ Modelling (Not Complete) __________________________

# Creating a train and test split 
# creating a sample
s <- sample(1:nrow(Moviedf_final4), 1000)
film.train <- Moviedf_final4[s, ]
film.test <- Moviedf_final4[-s, ]

# 1st Model
# Creating a full model
lm.full <- lm(Adj.Total.Movie.Earning.million~Theater+Runtime.min+Action+Adventure+
                Animation+Biography+Comedy+Crime+Documentary+Drama+Family+Fantasy+History+
                Horror+Music+Musical+Mystery+Romance+`Sci-Fi`+Short+Sport+Thriller+War+
                Western+Rating_G+`Rating_NOT RATED`+Rating_PG+`Rating_PG-13`+Rating_R+
                `Rating_TV-14`+Rating_UNRATED+Avg.Ticket.Price+Adj.Earning.Opening.Day.million+
                BV+Fox+WB+Uni.+Sony+Par., data=film.train)

summary(lm.full)



# _______ Useful Links _______
#1. https://stackoverflow.com/questions/52631921/find-unique-values-in-a-character-vector-separated-by-commas-and-then-one-hot-en
#2. https://stackoverflow.com/questions/4862178/remove-rows-with-all-or-some-nas-missing-values-in-data-frame
#3. https://stackoverflow.com/questions/26273663/r-how-to-total-the-number-of-na-in-each-col-of-data-frame



