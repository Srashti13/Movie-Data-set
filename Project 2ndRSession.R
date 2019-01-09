
library(rvest)                    # Web Scraping
library(stringr)                  # String manipulation library
library(devtools)                 # use to install packages for non-traditional way (such as github)
#devtools::install_github("hrbrmstr/omdbapi")
library(omdbapi)                  # using omdb api to use IMDB database

# __________________________ Yearly Data ________________________________
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
for (i in 1001:1800){
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
write.csv(Moviedf, "Moviedf2.csv", row.names = FALSE)


# ____ Missing Titles Data ____ 

moviedatanotfoundindex2 <- c(10, 19, 29, 50, 61, 66, 98, 123, 135, 157, 163, 164, 176,
                             180, 190, 196, 247, 265, 323, 333, 340, 390, 399, 496, 533,
                             564, 567, 568, 570, 599, 601, 611, 618, 646, 656, 665, 678,
                             682, 693, 694, 731, 733, 764, 765, 777, 798, 823, 845, 856,
                             865, 876, 890, 896, 899, 911, 917, 930, 936, 946, 951, 960,
                             961, 965, 980, 981, 993, 1010, 1014, 1048, 1068, 1075, 1078,
                             1109, 1110, 1118, 1123, 1145, 1164, 1185, 1234, 1244, 1268,
                             1270, 1298, 1350, 1351, 1352, 1357, 1359, 1370, 1418, 1426,
                             1433, 1436, 1489, 1506, 1519, 1532, 1565, 1566, 1628, 1631, 
                             1652, 1753, 1757, 1788)

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

Moviedf3 <- read.csv("Moviedf_full.csv")
Moviedf3$Movie.Name <- as.character(Moviedf3$Movie.Name)
Moviedf3$ImdB.names.used <- Moviedf3$Movie.Name
for(i in 1:106){
  Moviedf3$ImdB.names.used[moviedatanotfoundindex2[i]] <- namestouse[i]
}

for (idx in moviedatanotfoundindex2){
  if (idx == 980){
    info <- try(movieinfo <- find_by_title(Moviedf3$ImdB.names.used[idx], 
                                           year_of_release = Moviedf3$Year[idx]), silent = TRUE)
  }else{
    info <- try(movieinfo <- find_by_title(Moviedf3$ImdB.names.used[idx]), silent = TRUE)
  }
  
  if('omdb' %in% class(info)){
    Moviedf3[idx, 'Rated'] <- movieinfo$Rated[1]
    Moviedf3[idx, 'Genre'] <- movieinfo$Genre[1]
    Moviedf3[idx, 'Director'] <- movieinfo$Director[1]
    Moviedf3[idx, 'Runtime'] <- movieinfo$Runtime[1]
  }else {
    print(idx)
    next
  }
}

write.csv(Moviedf3, "Moviedf_full2.csv", row.names = FALSE)

# __________________________________



