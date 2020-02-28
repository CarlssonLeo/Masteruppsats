library(readxl)
library(tidyverse)
library(rvest)
library(haven)
library(lubridate)
library(reticulate)

# Data ----------------------------------------------------------------------------------------
# Common
pop <- read_excel("scrape_sweden/pop_sweden_1950-2019.xlsx", 
                  skip = 5) %>% slice(1:290) %>% select(municipality = Kommun,`2002`,`2006`,`2010`,`2014`,`2018`) 
ID <- read_excel("scrape_sweden/kommunlankod27.xls", 
                 skip = 5) %>% select(municipality = Name,
                                      ID = Code,
                                      province = ...3,
                                      province_ID = ...5) %>%
        filter(!str_detect(municipality," län")) %>%
        mutate(muni_ID = ID)


# 2006 ----------------------------------------------------------------------------------------
# Import
elections_sweden <- read_html("https://data.val.se/val/val2006/slutlig/K/rike/delar.html") %>%
        html_table(header = TRUE, fill = TRUE)
elections_sweden <- elections_sweden[[1]]
elections_sweden <- elections_sweden %>% 
        gather(party,vote_share,2:9) %>% 
        arrange(Område) %>% 
        select(municipality = Område,
               party,
               vote_share,
               turnout = VDT) %>%
        filter(!party == "ÖVR") %>%
        mutate(municipality = if_else(municipality == "Malung", "Malung-Sälen",municipality))


#Clean away dashes and commas, make data into numeric values 
for(j in 3:ncol(elections_sweden)){
        elections_sweden[,j]=sub("%", "", elections_sweden[,j], fixed = TRUE)
        elections_sweden[,j]=as.numeric(sub(",", ".", elections_sweden[,j], fixed = TRUE))
}

other <- read_excel("scrape_sweden/election_2006.xls", 
                    sheet = "Tabell 9", skip = 6) %>% slice(6:373) %>%
        select(municipality = Kommun,
               party = ...2,
               votes = Antal,
               vote_share = `% av samtliga giltiga röster`,
               seats_won = Totalt) %>%
        filter(!is.na(vote_share)) %>%
        mutate(municipality = if_else(municipality == "Heby2", "Heby", municipality)) %>%
        mutate(municipality = if_else(municipality == "Malung", "Malung-Sälen",municipality)) %>%
        mutate(party = if_else(party == "Sverigedemokraterna", "SD", party)) %>%
        mutate(party = if_else(party == "SPI - Sveriges Pensionärers Intresseparti", "SPI", party)) %>% 
        fill(municipality)


elections_sweden <- merge(elections_sweden, pop, by = "municipality", all = TRUE)
elections_sweden <- elections_sweden %>% select(-`2010`,-`2014`,-`2018`,-`2002`) %>%
        rename(pop = `2006`)
elections_sweden <- merge(elections_sweden, ID, by = "municipality", all = TRUE)

#Seats        
mandates <- read_html("https://data.val.se/val/val2006/slutlig_ovrigt/statistik/kommun/mandat_kommun_parti.html") %>%
        html_table(header = TRUE, fill = TRUE) %>%
        first() %>% filter(Kommun != "SUMMA") %>%
        gather(party,seats_won,2:11) %>% 
        rename(municipality = Kommun,
               mandates = TOT) %>% 
        separate_rows(seats_won, sep = ", ") %>% #These could be split into separate rows using gsub and regular expressions
        na_if("") %>%
        mutate(municipality = if_else(municipality == "Malung", "Malung-Sälen",municipality)) %>%
        group_by(municipality) %>%
        mutate(nParties = sum(!is.na(seats_won))) %>%
        filter(!is.na(seats_won)) %>%
        arrange(municipality)

local <- merge(elections_sweden,mandates, by = c("municipality", "party"), all = TRUE)

local <- local %>% mutate(cname = "Sweden",
                          ccode = "SE",
                          date = "2006-09-17",
                          clevel = "municipality")

voter <- read_excel("scrape_sweden/election_2006.xls", 
                    sheet = "Tabell 7", skip = 5) %>% 
        select(municipality = ...1,
               eligible_voters = ...3,
               total_voters = ...5,
               valid_votes = ...10,
               M=m,C=c,FP=fp,KD=kd,MP=mp,S=s,V=v,ÖVR=övr)%>%
        mutate(municipality = if_else(municipality == "Heby2", "Heby", municipality)) %>%
        mutate(municipality = if_else(municipality == "Malung", "Malung-Sälen",municipality)) %>%
        filter(!str_detect(municipality," län")) %>% #NOTE space before län! if not, removes the muni Borlänge
        filter(!is.na(municipality)) %>% 
        filter(municipality != "Hela riket") %>%
        slice(-291:-293) %>%
        gather(party,votes,5:12) %>%
        arrange(municipality)

local <- merge(local,voter, by = c("municipality", "party"), all = TRUE) %>% 
        filter(!party == "SD") %>%
        filter(!party == "ÖVR")
local <- merge(local, other, by = c("municipality","votes","party","vote_share","seats_won"), all = TRUE) %>% 
        arrange(municipality, turnout)
local <- local %>% mutate(seats_won = ifelse(is.na(seats_won), 0, seats_won)) %>% 
        fill(turnout:valid_votes) %>% 
        select(cname, ccode, date, clevel, turnout, ID,
                          province_ID, province, muni_ID, municipality, pop, eligible_voters, total_voters,
                          valid_votes, party, votes, vote_share, seats_won, mandates, nParties) %>%
        arrange(date, municipality, desc(vote_share))

sweden <- local



# 2010 ----------------------------------------------------------------------------------------
# Import
elections_sweden <- read_html("https://data.val.se/val/val2010/slutresultat/K/rike/index.html") %>%
        html_table(header = TRUE, fill = TRUE)
elections_sweden <- elections_sweden[[2]]

elections_sweden <- elections_sweden[,c(1,2,4,6,8,10,12,14,16,18,20,22,24)] %>%
        gather(party,vote_share, 2:9) %>%
        arrange(Område) %>% 
        select(municipality = Område,
               party,
               vote_share,
               turnout = VDT) %>% 
        filter(!municipality == "Sverige")


votes <- read_html("https://data.val.se/val/val2010/slutresultat/K/rike/index.html") %>%
        html_table(header = TRUE, fill = TRUE)
votes <- votes[[2]]
votes <- votes[,c(1,3,5,7,9,11,13,15,17)] %>%
        gather(party,votes, 2:9) %>%
        rename(municipality = Område)%>% 
        filter(!municipality == "Sverige")
elections_sweden <- merge(elections_sweden, votes, by = c("municipality", "party"))

#Clean away dashes and commas, make data into numeric values 
for(j in 3:ncol(elections_sweden)){
        elections_sweden[,j]=sub("%", "", elections_sweden[,j], fixed = TRUE)
        elections_sweden[,j]=as.numeric(sub(",", ".", elections_sweden[,j], fixed = TRUE))
}

#Getting the non-national parties
other <- read_excel("scrape_sweden/election_2010.xls", 
                    sheet = "Tabell 11", skip = 11) %>%
        select(municipality = `Stockholms län`,
               party = ...3,
               votes = ...4,
               vote_share = ...5,
               seats_won = ...9)%>%
        filter(!is.na(vote_share)) %>%
        fill(municipality) %>%
        mutate(municipality = if_else(municipality == "Malung", "Malung-Sälen",municipality))

elections_sweden <- merge(elections_sweden, other, by = c("municipality","party","votes","vote_share"), all = TRUE)
elections_sweden <- elections_sweden %>% arrange(municipality, turnout) %>%
        fill(turnout)

#Seats for national parties        
mandates <- read_html("https://data.val.se/val/val2010/slutresultat/K/rike/valda.html") %>%
        html_table(header = TRUE, fill = TRUE)
mandates <- mandates[[2]] %>% filter(Område != "Sverige") %>%
        gather(party,seats_won,2:9) %>% 
        rename(municipality = Område,
               mandates = TOT) %>% 
        group_by(municipality) %>%
        mutate(nParties = sum(!is.na(seats_won))) %>%
        mutate(seats_won = ifelse(is.na(seats_won),0,seats_won)) %>%
        arrange(municipality) %>% select(-ÖVR)

#Merge seats and all other election data
local <- merge(elections_sweden,mandates, by = c("municipality", "party"), all = TRUE)
local <- local %>% mutate(seats_won = if_else(is.na(seats_won.x), seats_won.y, seats_won.x))%>%
        select(-seats_won.x, -seats_won.y) %>% arrange(municipality, mandates) %>%
        fill(mandates:nParties)


#generate the static variables
local <- local %>% mutate(cname = "Sweden",
                          ccode = "SE",
                          date = "2010-09-19",
                          clevel = "municipality")
#pop data and IDs
local <- merge(local, pop, by = "municipality", all = TRUE)
local <- local %>% select(-`2006`,-`2014`,-`2018`,-`2002`) %>%
        rename(pop = `2010`)
local <- merge(local, ID, by = "municipality", all = TRUE)

#Voter data
voter <- read_excel("scrape_sweden/election_2010.xls", 
                    sheet = "Tabell 9", skip = 5) %>% 
        select(municipality = ...1,
               eligible_voters = `berät-`,
               total_voters = ...5,
               valid_votes = giltiga) %>%
        slice(7:336) %>%
        filter(!str_detect(municipality," län")) %>% #NOTE space before län! if not, removes the muni Borlänge
        filter(!is.na(municipality)) %>%
        mutate(municipality = if_else(municipality == "Malung", "Malung-Sälen",municipality))

local <- merge(local,voter, by = "municipality", all = TRUE)

#reorder and arrange
local <- local %>% select(cname, ccode, date, clevel, turnout, ID,
                          province_ID, province, muni_ID, municipality, pop, eligible_voters, total_voters,
                          valid_votes, party, votes, vote_share, seats_won, mandates, nParties) %>%
        arrange(date, municipality, desc(vote_share))

sweden <- rbind(sweden, local)


# 2014 ----------------------------------------------------------------------------------------
# Import
elections_sweden <- read_html("https://data.val.se/val/val2014/slutresultat/K/rike/index.html") %>%
        html_table(header = TRUE, fill = TRUE)
elections_sweden <- elections_sweden[[2]]

elections_sweden <- elections_sweden[,c(1,2,4,6,8,10,12,14,16,18,20,22,24,26)] %>%
        gather(party,vote_share, 2:10) %>%
        arrange(Område) %>% 
        select(municipality = Område,
               party,
               vote_share,
               turnout = VDT)%>% 
        filter(!municipality == "Sverige")

votes <- read_html("https://data.val.se/val/val2014/slutresultat/K/rike/index.html") %>%
        html_table(header = TRUE, fill = TRUE)
votes <- votes[[2]]
votes <- votes[,c(1,3,5,7,9,11,13,15,17,19)] %>%
        gather(party,votes, 2:10) %>% #Fi!
        rename(municipality = Område) %>% 
        filter(!municipality == "Sverige")

elections_sweden <- merge(elections_sweden, votes, by = c("municipality", "party"))
rm(votes)

#Clean away dashes and commas, make data into numeric values 
for(j in 3:ncol(elections_sweden)){
        elections_sweden[,j]=sub("%", "", elections_sweden[,j], fixed = TRUE)
        elections_sweden[,j]=as.numeric(sub(",", ".", elections_sweden[,j], fixed = TRUE))
}
rm(j)

#Getting the non-national parties
other <- read_excel("scrape_sweden/election_2014.xlsx", 
                    sheet = "Tabell 11", skip = 11) %>%
        select(municipality = `Stockholms län`,
               party = ...3,
               votes = ...4,
               vote_share = ...5,
               seats_won = ...9)%>%
        filter(!is.na(vote_share)) %>% 
        filter(!party == "Feministiskt initiativ")%>%
        mutate(municipality = if_else(municipality == "Malung", "Malung-Sälen",municipality))

elections_sweden <- merge(elections_sweden, other, by = c("municipality","party","votes","vote_share"), all = TRUE)
elections_sweden <- elections_sweden %>% arrange(municipality, turnout) %>%
        fill(turnout)
rm(other)

#Seats for national parties        
mandates <- read_html("https://data.val.se/val/val2014/slutresultat/K/rike/valda.html") %>%
        html_table(header = TRUE, fill = TRUE)
mandates <- mandates[[2]] %>% filter(Område != "Sverige") %>%
        gather(party,seats_won,2:10) %>% #Fi!
        rename(municipality = Område,
               mandates = TOT) %>% 
        group_by(municipality) %>%
        mutate(nParties = sum(!is.na(seats_won))) %>%
        mutate(seats_won = ifelse(is.na(seats_won),0,seats_won)) %>%
        arrange(municipality) %>% select(-ÖVR)

#Merge seats and all other election data
local <- merge(elections_sweden,mandates, by = c("municipality", "party"), all = TRUE)
local <- local %>% mutate(seats_won = if_else(is.na(seats_won.x), seats_won.y, seats_won.x))%>%
        select(-seats_won.x, -seats_won.y) %>% arrange(municipality, mandates) %>%
        fill(mandates:nParties)
rm(mandates)

#generate the static variables
local <- local %>% mutate(cname = "Sweden",
                          ccode = "SE",
                          date = "2014-09-14",
                          clevel = "municipality")
#pop data and IDs
local <- merge(local, pop, by = "municipality", all = TRUE)
local <- local %>% select(-`2006`,-`2010`,-`2018`,-`2002`) %>%
        rename(pop = `2014`)
local <- merge(local, ID, by = "municipality", all = TRUE)

#Voter data
voter <- read_excel("scrape_sweden/election_2014.xlsx", 
                    sheet = "Tabell 9", skip = 5) %>% 
        select(municipality = ...1,
               eligible_voters = `berät-`,
               total_voters = ...3,
               valid_votes = giltiga) %>%
        slice(7:336) %>%
        filter(!str_detect(municipality," län")) %>% #NOTE space before län! if not, removes the muni Borlänge
        filter(!is.na(municipality)) %>%
        mutate(municipality = if_else(municipality == "Malung", "Malung-Sälen",municipality))

local <- merge(local,voter, by = "municipality", all = TRUE)
rm(voter)

#reorder and arrange
local <- local %>% select(cname, ccode, date, clevel, turnout, ID,
                          province_ID, province, muni_ID, municipality, pop, eligible_voters, total_voters,
                          valid_votes, party, votes, vote_share, seats_won, mandates, nParties) %>%
        arrange(date, municipality, desc(vote_share))

sweden <- rbind(sweden, local)


# 2018 ----------------------------------------------------------------------------------------
# Import
elections_sweden <- read_html("https://data.val.se/val/val2018/slutresultat/K/rike/index.html") %>%
        html_table(header = TRUE, fill = TRUE)
elections_sweden <- elections_sweden[[2]]
elections_sweden <- elections_sweden[,c(1,2,4,6,8,10,12,14,16,18,20,22,24,26,28)] %>%
        gather(party,vote_share, 2:10) %>%
        arrange(Område) %>% 
        select(municipality = Område,
               party,
               vote_share,
               turnout = VDT)%>% 
        filter(!municipality == "Sverige")

votes <- read_html("https://data.val.se/val/val2018/slutresultat/K/rike/index.html") %>%
        html_table(header = TRUE, fill = TRUE)
votes <- votes[[2]]
votes <- votes[,c(1,3,5,7,9,11,13,15,17,19)] %>%
        gather(party,votes, 2:10) %>% #Fi!
        rename(municipality = Område)%>% 
        filter(!municipality == "Sverige")
elections_sweden <- merge(elections_sweden, votes, by = c("municipality", "party"))
rm(votes)

#Clean away dashes and commas, make data into numeric values 
for(j in 3:ncol(elections_sweden)){
        elections_sweden[,j]=sub("%", "", elections_sweden[,j], fixed = TRUE)
        elections_sweden[,j]=as.numeric(sub(",", ".", elections_sweden[,j], fixed = TRUE))
}
rm(j)

#Getting the non-national parties
other <- read_excel("scrape_sweden/other_2018.xlsx", skip = 11) %>%
        select(municipality = `Stockholms län`,
               party = ...3,
               votes = ...4,
               vote_share = ...5,
               seats_won = ...9)%>%
        filter(!is.na(vote_share))%>%
        filter(!party == "Feministiskt initiativ") %>%
        mutate(municipality = if_else(municipality == "Malung", "Malung-Sälen",municipality))

elections_sweden <- merge(elections_sweden, other, by = c("municipality","party","votes","vote_share"), all = TRUE)
elections_sweden <- elections_sweden %>% arrange(municipality, turnout) %>%
        fill(turnout)
rm(other)

#Seats for national parties        
mandates <- read_html("https://data.val.se/val/val2018/slutresultat/K/rike/valda.html") %>%
        html_table(header = TRUE, fill = TRUE)
mandates <- mandates[[2]] %>% filter(Område != "Sverige") %>%
        gather(party,seats_won,2:10) %>% #Fi!
        rename(municipality = Område,
               mandates = TOT) %>% 
        group_by(municipality) %>%
        mutate(seats_won = ifelse(is.na(seats_won),0,seats_won)) %>%
        arrange(municipality) %>% select(-ÖVR)

#Merge seats and all other election data
local <- merge(elections_sweden,mandates, by = c("municipality", "party"), all = TRUE)
local <- local %>% mutate(seats_won = if_else(is.na(seats_won.x), seats_won.y, seats_won.x))%>%
        select(-seats_won.x, -seats_won.y) %>% arrange(municipality, mandates) %>% 
        fill(mandates)
rm(mandates)

#generate the static variables
local <- local %>% mutate(cname = "Sweden",
                          ccode = "SE",
                          date = "2018-09-09",
                          clevel = "municipality")
#pop data and IDs
local <- merge(local, pop, by = "municipality", all = TRUE)
local <- local %>% select(-`2006`,-`2010`,-`2014`,-`2002`) %>%
        rename(pop = `2018`)
local <- merge(local, ID, by = "municipality", all = TRUE)

#Voter data
voter <- read_excel("scrape_sweden/election_2018.xlsx", 
                    skip = 5) %>% 
        select(municipality = ...1,
               eligible_voters = tigade,
               total_voters = ...3,
               valid_votes = `val-`) %>%
        slice(6:388) %>%
        mutate(municipality = if_else(municipality == "Malung", "Malung-Sälen",municipality)) %>%
        filter(municipality %in% ID$municipality)

local <- merge(local,voter, by = "municipality", all = TRUE)
rm(voter)

local <- local %>% group_by(municipality) %>% 
        mutate(seatsCount = ifelse(seats_won == 0, NA, seats_won)) %>%
        mutate(nParties = sum(!is.na(seatsCount))) %>% ungroup()

#reorder and arrange
local <- local %>% select(cname, ccode, date, clevel, turnout, ID,
                          province_ID, province, muni_ID, municipality, pop, eligible_voters, total_voters,
                          valid_votes, party, votes, vote_share, seats_won, mandates, nParties) %>%
        arrange(date, municipality, desc(vote_share))

sweden <- rbind(sweden, local)
rm(local)
rm(elections_sweden)

sweden <- sweden %>%  mutate(party = if_else(party == "FP", "L", party)) %>% select(-cname,-ccode,-clevel)


# 2002 ----------------------------------------------------------------------------------------
#missing local parties
# 2002
elections_sweden <- read_excel("scrape_sweden/2002_votes.xlsx", skip = 3) %>% 
        select(muni_ID = ...1,
               municipality = ...2,
               party = ...3,
               votes = `2002...5`,
               vote_share = `2002...6`) %>%
        fill(muni_ID,municipality) %>%
        slice(1:3201) %>%
        filter(municipality != "Bara")

votes <- elections_sweden %>% group_by(municipality) %>%
        mutate(eligible_voters = sum(votes)) %>%
        filter(party != "VALSKOLKARE") %>% 
        mutate(total_voters = sum(votes)) %>%
        filter(party != "OGILTIGA") %>%
        mutate(valid_votes = sum(votes)) %>%
        mutate(turnout = total_voters/eligible_voters) %>%
        filter(party == "M") %>%
        filter(municipality != "Bara") %>%
        select(muni_ID, municipality, eligible_voters:turnout)

elections_sweden <- merge(elections_sweden, votes, by = c("muni_ID", "municipality"))     
rm(votes)

mandates <- read_excel("scrape_sweden/2002_mandates.xlsx", skip = 1) %>% 
        select(muni_ID = ...1,
               municipality = ...2,
               party = ...3,
               seats_won = `2002`) %>%
        fill(muni_ID,municipality) %>%
        slice(1:2619) %>%
        mutate(seats_won = as.integer(seats_won)) %>%
        group_by(municipality) %>%
        mutate(mandates = sum(seats_won)) %>%
        filter(!is.na(seats_won))

elections_sweden <- merge(elections_sweden, mandates, by = c("muni_ID", "municipality", "party"))
rm(mandates)

elections_sweden <- merge(elections_sweden, pop, by = "municipality", all = TRUE)
elections_sweden <- elections_sweden %>% select(-`2006`,-`2010`,-`2014`,-`2018`) %>%
        rename(pop = `2002`)
elections_sweden <- merge(elections_sweden, ID, by = c("municipality","muni_ID"), all = TRUE)
rm(pop)
rm(ID)

elections_sweden <- elections_sweden %>%
        mutate(date = "2002-09-15",
               nParties = NA,
               party = if_else(party == "FP", "L", party)) %>%
        select(date, turnout, ID,
               province_ID, province, muni_ID, municipality, pop, eligible_voters, total_voters,
               valid_votes, party, votes, vote_share, seats_won, mandates, nParties)
sweden <- rbind(sweden, elections_sweden)
rm(elections_sweden)

# misc ----------------------------------------------------------------------------------------
sweden <- sweden %>% mutate(turnout = as.numeric(turnout),
                            eligible_voters = as.integer(eligible_voters),
                            valid_votes = as.integer(valid_votes),
                            votes = as.integer(votes),
                            vote_share = as.numeric(vote_share),
                            seats_won = as.integer(seats_won))
procurement <- read_dta("./Data/procurement/bdf_replication.dta")


# Majorities ----------------------------------------------------------------------------------
url <- "https://sv.wikipedia.org/wiki/Lista_över_kommun-_och_landstingsstyren_i_Sverige_2018-2022"
#Blekinge
df <- read_html(url) %>% 
        html_nodes('table') %>% 
        `[[`(2) %>% 
        html_table(fill = TRUE)
majorities <- df

#Dalarna
df <- read_html(url) %>% 
        html_nodes('table') %>% 
        `[[`(4) %>% 
        html_table(fill = TRUE)
majorities <- rbind(majorities, df)

#Gotland
df <- read_html(url) %>% 
        html_nodes('table') %>% 
        `[[`(5) %>% 
        html_table(fill = TRUE)
majorities <- rbind(majorities, df)

#gävleborg
df <- read_html(url) %>% 
        html_nodes('table') %>% 
        `[[`(7) %>% 
        html_table(fill = TRUE)
majorities <- rbind(majorities, df)

#Halland
df <- read_html(url) %>% 
        html_nodes('table') %>% 
        `[[`(9) %>% 
        html_table(fill = TRUE)
majorities <- rbind(majorities, df)

#Jämtland
df <- read_html(url) %>% 
        html_nodes('table') %>% 
        `[[`(11) %>% 
        html_table(fill = TRUE)
majorities <- rbind(majorities, df)

#Jönköping
df <- read_html(url) %>% 
        html_nodes('table') %>% 
        `[[`(13) %>% 
        html_table(fill = TRUE)
majorities <- rbind(majorities, df)

#kronoberg
df <- read_html(url) %>% 
        html_nodes('table') %>% 
        `[[`(17) %>% 
        html_table(fill = TRUE)
majorities <- rbind(majorities, df)

#norrbotten
df <- read_html(url) %>% 
        html_nodes('table') %>% 
        `[[`(19) %>% 
        html_table(fill = TRUE)
majorities <- rbind(majorities, df)

#Stockholm
df <- read_html(url) %>% 
        html_nodes('table') %>% 
        `[[`(23) %>% 
        html_table(fill = TRUE)
majorities <- rbind(majorities, df)

#Södermanland
df <- read_html(url) %>% 
        html_nodes('table') %>% 
        `[[`(25) %>% 
        html_table(fill = TRUE)
majorities <- rbind(majorities, df)

#Uppsala
df <- read_html(url) %>% 
        html_nodes('table') %>% 
        `[[`(27) %>% 
        html_table(fill = TRUE)
majorities <- rbind(majorities, df)

#Västerbotten
df <- read_html(url) %>% 
        html_nodes('table') %>% 
        `[[`(31) %>% 
        html_table(fill = TRUE)
majorities <- rbind(majorities, df)


#Västernorrland
df <- read_html(url) %>% 
        html_nodes('table') %>% 
        `[[`(33) %>% 
        html_table(fill = TRUE)
majorities <- rbind(majorities, df)

#västra götaland
df <- read_html(url) %>% 
        html_nodes('table') %>% 
        `[[`(37) %>% 
        html_table(fill = TRUE)
majorities <- rbind(majorities, df)

#örebro
df <- read_html(url) %>% 
        html_nodes('table') %>% 
        `[[`(39) %>% 
        html_table(fill = TRUE)
majorities <- rbind(majorities, df)
        
#Östergötland
df <- read_html(url) %>% 
        html_nodes('table') %>% 
        `[[`(41) %>% 
        html_table(fill = TRUE)
majorities <- rbind(majorities, df)

majorities <- majorities %>% select(municipality = X1,
                                    rule = X8,
                                    num_mandates = X9) %>%
        filter(municipality != "Kommun")

#Västmanland
df <- read_html(url) %>% 
        html_nodes('table') %>% 
        `[[`(35) %>% 
        html_table(fill = TRUE)
df <- df[,-2] 
df <- df %>% select(municipality = X1,
                    rule = X9,
                    num_mandates = X10) %>%
        filter(municipality != "Kommun")
majorities <- rbind(majorities, df)

#Kalmar
df <- read_html(url) %>% 
        html_nodes('table') %>% 
        `[[`(15) %>% 
        html_table(fill = TRUE)
df <- df[,-10]
df <- df %>% select(municipality = X1,
                    rule = X8,
                    num_mandates = X9) %>%
        filter(municipality != "Kommun")
majorities <- rbind(majorities, df)

#Troublesome
#skåne
df <- tribble(
        ~municipality,  ~rule,                   ~num_mandates,
        "Bjuv",	        "S+C+KD+L(2018–2019)",	"14 av 31 (minoritet)",
        "Bjuv",	        "SD+M(2020–)",  	"16 av 31",
        "Bromölla",	"SD+M+KD",      	"19 av 41 (minoritet)",
        "Burlöv",	"M+L+C",	        "13 av 41 (minoritet)",
        "Båstad",	"M+S+L+KD",     	"21 av 41",
        "Eslöv",	"S+M+L",        	"26 av 51",
        "Helsingborg",	"M+L+C+KD",        	"27 av 65 (minoritet)",
        "Hässleholm",	"M+KD+L",        	"17 av 61 (minoritet)",
        "Höganäs",	"M+C+KD",        	"22 av 41",
        "Hörby",	"SD",           	"14 av 41 (minoritet)",
        "Höör", 	"M+L+C+KD",        	"19 av 41 (minoritet)",
        "Klippan",	"S+M+C",        	"20 av 41 (minoritet)",
        "Kristianstad",	"M+L+C+KD",        	"29 av 65 (minoritet)",
        "Kävlinge",	"M+L+C+KD",        	"24 av 49 (minoritet)",
        "Landskrona",	"L+M+MP",        	"24 av 51 (minoritet)",
        "Lomma",	"M+L+C+KD",        	"26 av 45",
        "Lund", 	"M+L+FNL+C+KD",         "32 av 65 (minoritet)",
        "Malmö",	"S+L",          	"24 av 61 (minoritet)",
        "Osby", 	"C+M+KD+L",        	"16 av 41 (minoritet)",
        "Perstorp",	"PF+M+C+KD+MP",	        "19 av 35",
        "Simrishamn",	"M+C+L+KD",        	"21 av 49 (minoritet)",
        "Sjöbo",	"M+C+L+MP",        	"20 av 49 (minoritet)",
        "Skurup",	"M+C+SkuP",        	"14 av 41 (minoritet)",
        "Staffanstorp",	"M+SD",	                "23 av 41",
        "Svalöv",	"C+M+L+KD(2018–2019)",	"13 av 35 (minoritet)",
        "Svalöv",	"SD+M+KD(2019–)",	"16 av 35 (minoritet)",
        "Svedala",	"M+C+BAP+L+KD",	        "18 av 45 (minoritet)",
        "Tomelilla",	"M+C+MP+L+KD",	        "19 av 41 (minoritet)",
        "Trelleborg",	"M+KD",	                "15 av 51 (minoritet)",
        "Vellinge",	"M",    	        "26 av 51",
        "Ystad",	"M+L+KD",	        "16 av 49 (minoritet)",
        "Åstorp",	"S+C",          	"12 av 31 (minoritet)",
        "Ängelholm",	"M+C+L+KD+MP",        	"26 av 51",
        "Örkelljunga",	"M+KD+C",        	"16 av 37 (minoritet)",
        "Östra Göinge",	"M+C+KD",        	"17 av 31"
)
majorities <- rbind(majorities, df)

#Värmland
df <- tribble(
        ~municipality,  ~rule,          ~num_mandates,
        "Arvika",	"S+V+MP",	"25 av 49",
        "Eda",  	"C+HEL+V+KD",	"16 av 35 (minoritet)",
        "Filipstad",	"S+V",	        "19 av 37",
        "Forshaga",	"S+C",  	"22 av 41",
        "Grums",	"S",    	"17 av 31",
        "Hagfors",	"OR",   	"13 av 35 (minoritet)",
        "Hammarö",	"M+L+KD+C+MP",	"15 av 31 (minoritet)",
        "Karlstad",	"M+C+MP+L+KD",	"28 av 61 (minoritet)",
        "Kil",  	"S+C",  	"22 av 41",
        "Kristinehamn",	"M+C+L+KD+MP",	"21 av 41",
        "Munkfors",	"S",    	"14 av 21",
        "Storfors",	"S+V",  	"15 av 27",
        "Sunne",	"C+S",  	"21 av 41",
        "Säffle",	"C+M+SIV+KD+L",	"24 av 41",
        "Torsby",	"S+C+L",	"18 av 31",
        "Årjäng",	"S+C+MP",	"19 av 35"
        
)
majorities <- rbind(majorities, df)
majorities <- majorities %>% filter(municipality != "Region") %>%
        mutate(municipality = if_else(municipality == "Upplands Bro", "Upplands-Bro",municipality)) %>%
        mutate(municipality = if_else(municipality == "Region Gotland", "Gotland",municipality))


`%notin%` <- Negate(`%in%`)
missing <- ID %>% filter(municipality %notin%  majorities$municipality)
missing <- majorities %>% filter(municipality %notin%  ID$municipality)

#"date", "nParties"  
# visualise -------------------------------------------------------------------------------------
sd <- sweden %>% group_by(date) %>% 
        filter(party == "SD" & seats_won > 0) %>%  ""
        summarise(SD = n())

ggplot(sd,aes(x= date, y= SD)) + 
        geom_bar(stat = "identity") +
        ggtitle("Number of muncipalities where Sweden Democrats had atleast one seat") +
        geom_text(aes(label = SD),nudge_y = 4)

riksdagspartier <- c("V", "S", "MP", "C", "L", "M", "KD", "SD")
parties <- sweden %>% group_by(date) %>% 
        filter(party %in% riksdagspartier & seats_won > 0) %>% 
        count(party) 
parties$party <- factor(parties$party, levels = riksdagspartier)
ggplot(parties, aes(x=date, y=n, group=party)) +
        geom_line(aes(color=party))+
        geom_point(aes(color=party))+
        ggtitle("Number of muncipalities where parties had atleast one seat")

mandates <- sweden %>% group_by(date, party) %>% 
        filter(party %in% riksdagspartier & seats_won > 0) %>% 
        summarise(total_seats = sum(seats_won))
mandates$party <- factor(mandates$party, levels = riksdagspartier)
ggplot(mandates, aes(x=date, y=total_seats, group=party)) +
        geom_line(aes(color=party))+
        geom_point(aes(color=party))+
        ggtitle("Total municipal seats won")

