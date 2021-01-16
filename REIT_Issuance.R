library(tidyverse)
library(lubridate)
library(ggthemes)
library(forcats)
library(XLConnect)

# Import NAREIT dataset
reit <- read_csv("EquityOfferings.csv")

# Fix date, price, total
reit$Date <- dmy(reit$Date)

reit$Price <- as.numeric(gsub("[$,]", "", reit$Price))

reit$Total <- as.numeric(gsub("[$,]", "", reit$Total))

# Make sector a factor
reit$'Property Sector' <- factor(reit$'Property Sector')  

# Check dataframe
head(reit)

# Date range
range(reit$Date, na.rm = TRUE)

# Fix names to be more dataframe friendly
names(reit) <- c("name", "type", "sector", "subsector", "date", "price", "shares", "overallotment", 
                 "total", "under1", "under2", "under3", "under4", "under5", "under6", "description", "x17")

reit %>% group_by(sector) %>% summarize(count = n()) %>% arrange(desc(count))

# Fix duplicate sectors
reit$sector <- reit$sector %>% fct_collapse(Mortgage = c("Mortgage", "Mortgage Backed"))
reit$sector <- reit$sector %>% fct_collapse(Lodging = c("Lodging", "Lodging/Resorts", "Lodgjng/Resorts"))
reit$sector <- reit$sector %>% fct_collapse(Industrial/Office = c("Industrial/Office", "Industrial/ Office"))

# Fix other bad sector names
reit %>% filter(sector == "Industrial/Office") %>% group_by(subsector) %>% summarize(total = n())
reit$sector <- if_else(as.character(reit$sector) == "Industrial/Office" & reit$subsector == "Office", "Office", as.character(reit$sector))
reit$sector <- if_else(as.character(reit$sector) == "Industrial/Office" & reit$subsector == "Industrial", "Industrial", as.character(reit$sector))

# Clean messy data
reit %>% group_by(sector, subsector) %>% summarize(count = n()) %>% arrange(desc(count)) %>% head(20)
reit %>% filter(sector == "Specialty") %>% group_by(subsector) %>% summarize(count = n())
reit %>% filter(subsector == "Hotels")

reit %>% filter(sector == "Specialty") %>% group_by(subsector) %>% summarize(count = n())

# Factoring sector is making things difficult
reit$sector <- as.character(reit$sector)
reit$sector[reit$subsector == "Hotels"] <- "Lodging"

reit %>% group_by(sector) %>% summarize(count = n()) %>% arrange(desc(count)) %>% tail(20)

# Clean up the last few
reit %>% filter(is.na(sector)) %>% group_by(name) %>% summarize(total = n()) %>% arrange(desc(total))

# Clean up the NAs
reit$sector[reit$name == "City Office REIT, Inc."] <- "Office"
reit$sector[reit$name == "Arbor Realty Trust, Inc."] <- "Mortgage"
reit$sector[reit$name == "Capital Lease Funding, Inc."] <- "Mortgage"
reit$sector[reit$name == "BRT Realty Trust"] <- "Residential"

#Clean up some of the misc. "Specialty"
specialty <- reit %>% filter(sector == "Specialty") %>% select(name, subsector)
specialty

reit$sector[reit$name == "Digital Realty Trust, Inc."] <- "Data Centers"
reit$sector[reit$name == "DuPont Fabros Technology, Inc."] <- "Data Centers"
reit$sector[reit$name == "Gaming and Leisure Properties, Inc."] <- "Lodging"
reit$sector[reit$name == "Plum Creek Timber Company, L.P."] <- "Timber"


reit$sector <- factor(reit$sector)




# Number of offerings by sector, all time
reit %>% 
  group_by(sector) %>%
  summarize(total_issuance = sum(total, na.rm = TRUE), number = n(), average = total_issuance / number) %>%
  arrange(desc(number))

# Plot it
reit %>% 
  group_by(sector) %>%
  summarize(total_issuance = sum(total, na.rm = TRUE), number = n(), average = total_issuance / number) %>%
  filter(total_issuance >= 10000) %>%
  ggplot(aes(x = number, y = average, color = sector)) +
  geom_point(aes(size = total_issuance)) 


# By average issuance during last recession
reit %>% 
  filter(date >= "2007-12-31" & date <= "2009-12-31") %>%
  group_by(sector) %>%
  summarize(total_issuance = sum(total, na.rm = TRUE), number = n(), average = total_issuance / number) %>%
  arrange(desc(average))

# Total issues by sector during last recession
reit %>% 
  filter(date > "2007-12-31" & date <= "2009-12-31") %>%
  group_by(sector) %>%
  summarize(total_issuance = sum(total, na.rm = TRUE), number = n(), average = total_issuance / number) %>%
  arrange(desc(number)) %>%
  filter(number > 5) %>%
  ggplot(aes(x = number, y = average, color = sector)) +
  geom_point(aes(size = total_issuance)) +
  xlab("Number of Offerings") +
  ylab("Average Size of Offering, in $MM") +
  ggtitle("Equity Offerings", "By Sector, 2008 - 2009") +
  geom_text(aes(label = sector), nudge_x = 1, nudge_y = 10, check_overlap = TRUE)


# Table for above
reit %>% 
  filter(date > "2007-12-31" & date <= "2009-12-31") %>%
  group_by(sector) %>%
  summarize(total_issuance = sum(total, na.rm = TRUE), number = n(), average = total_issuance / number) %>%
  arrange(desc(number)) %>%
  filter(number > 5)
  
# By recent number of offerings per sector
reit %>% 
  filter(date >= "2014-01-01") %>%
  group_by(sector) %>%
  summarize(total_issuance = sum(total, na.rm = TRUE), number = n(), average = total_issuance / number) %>%
  arrange(desc(number))

# Plot number of issues by sector from 2015 through 2019
reit %>% 
  filter(date >= "2014-01-01") %>%
  group_by(sector) %>%
  summarize(total_issuance = sum(total, na.rm = TRUE), number = n(), average = total_issuance / number) %>%
  filter(number > 20) %>%
  ggplot(aes(x = number, y = average, label = sector)) + 
  geom_point(aes(color = sector, size = total_issuance)) + 
  xlab("Number of Offerings") + ylab("Average Issuance, in $MM") +
  ggtitle("Number of Secondary Offerings", "By Sector, 2014 - 2019") +
  geom_text(aes(label = sector), nudge_x = 5, nudge_y = 21, check_overlap = TRUE)


# Table for above
reit %>% group_by(sector) %>%
  filter(date >= "2014-01-01") %>%
  summarize(total_issuance = sum(total, na.rm = TRUE), number = n(), average = total_issuance / number) %>%
  filter(number > 10) %>%
  arrange(desc(total_issuance))

# Recent offerings by subsector
reit %>% group_by(sector, subsector) %>% 
  filter(date >= "2015-01-01") %>%
  summarize(total_issuance = sum(total, na.rm = TRUE), issues = n(), average = total_issuance / issues) %>%
  arrange(desc(issues))

# Recent offerings by company
reit %>% filter(date >= "2015-01-01") %>%
  group_by(name, sector) %>%
  summarize(total_issuance = sum(total, na.rm = TRUE), issues = n(), average = total_issuance / issues) %>%
  arrange(desc(issues))

# Recent offerings by mortgage reits
reit %>%
  filter(date >= "2015-01-01", sector == "Mortgage") %>%
  group_by(name, subsector) %>%
  summarize(total_issuance = sum(total, na.rm = TRUE), offerings = n(), average = total_issuance / offerings) %>%
  arrange(desc(offerings)) %>% head(n = 15)

### Look for offerings underwriten by non-white shoe firms
reit %>% 
  filter(date >= "2015-01-01") %>%
  select(under1, under2, under3, under4, under5, under6) %>% 
  distinct(under2)

# Extact all underwriter names
underwriters <- NULL

reit.2014 <- reit %>% filter(date >= "2014-01-01")

for (i in 10:15){
  underwriters <- rbind(underwriters, reit.2014[ , i])
}

underwriters <- distinct(underwriters)

dim(underwriters)
class(underwriters)

names(underwriters) <- "underwriters"

head(underwriters, 20)

# Exclude "White shoe" firms
underwriters <- underwriters[-c(2, 3, 4, 6, 8, 9, 10, 11, 12, 15, 16, 31, 34), ]

head(underwriters)
tail(underwriters)

# Filter reits for underwriters in second or third book runner position
reit %>% 
  filter(date >= "2015-01-01") %>%
  filter(under1 %in% as.matrix(underwriters) | under2 %in% as.matrix(underwriters) | under3 %in% as.matrix(underwriters) | under4 %in% as.matrix(underwriters) | under5 %in% as.matrix(underwriters) | under6 %in% as.matrix(underwriters)) %>%
  filter(!is.na(under2)) %>%
  group_by(sector) %>%
  summarize(offerings = n(), total_issuance = sum(total, na.rm = TRUE), average = total_issuance / offerings) %>%
  arrange(desc(average)) %>%
  filter(offerings >= 5) %>%
  ggplot(aes(x = offerings, y = average)) + 
  geom_point(aes(size = total_issuance, color = sector)) +
  ylab("Average Offering Size $MM") +
  xlab("Number of Secondary Offerings") +
  ggtitle("REIT Secondary Equity Offerings", "By Sector, Since 2015") +
  geom_text(aes(label = sector), size = 2.5, nudge_x = 4.5, nudge_y = -15, check_overlap = TRUE) 

  
# Table for above, ranked by average offering size
reit %>% 
  filter(date >= "2015-01-01") %>%
  filter(under1 %in% as.matrix(underwriters) | under2 %in% as.matrix(underwriters) | under3 %in% as.matrix(underwriters) | under4 %in% as.matrix(underwriters) | under5 %in% as.matrix(underwriters) | under6 %in% as.matrix(underwriters)) %>%
  filter(!is.na(under2)) %>%
  group_by(sector) %>%
  summarize(offerings = n(), total_issuance = sum(total, na.rm = TRUE), average = total_issuance / offerings) %>%
  arrange(desc(average))

# Table ranked by total offerings
reit %>% 
  filter(date >= "2015-01-01") %>%
  filter(under1 %in% as.matrix(underwriters) | under2 %in% as.matrix(underwriters) | under3 %in% as.matrix(underwriters) | under4 %in% as.matrix(underwriters) | under5 %in% as.matrix(underwriters) | under6 %in% as.matrix(underwriters)) %>%
  filter(!is.na(under2)) %>%
  group_by(sector) %>%
  summarize(offerings = n(), total_issuance = sum(total, na.rm = TRUE), average = total_issuance / offerings) %>%
  arrange(desc(offerings))

# Table ranked by total total issuance
reit %>% 
  filter(date >= "2015-01-01") %>%
  filter(under1 %in% as.matrix(underwriters) | under2 %in% as.matrix(underwriters) | under3 %in% as.matrix(underwriters) | under4 %in% as.matrix(underwriters) | under5 %in% as.matrix(underwriters) | under6 %in% as.matrix(underwriters)) %>%
  filter(!is.na(under2)) %>%
  group_by(sector) %>%
  summarize(offerings = n(), total_issuance = sum(total, na.rm = TRUE), average = total_issuance / offerings) %>%
  arrange(desc(total_issuance))

# List of names of companies offering througn non-white shoe co-bookrunners since 2017
reit %>% 
  filter(date >= "2015-01-01") %>%
  filter(under1 %in% as.matrix(underwriters) | under2 %in% as.matrix(underwriters) | under3 %in% as.matrix(underwriters) | under4 %in% as.matrix(underwriters) | under5 %in% as.matrix(underwriters) | under6 %in% as.matrix(underwriters)) %>%
  filter(!is.na(under2)) %>%
  filter(sector == "Infrastructure" | sector == "Mortgage") %>%
  group_by(name) %>% summarize(total_offerings = n()) %>%
  arrange(desc(total_offerings)) %>%
  head(15)


# Plot isuance over time
reit %>%
  filter(date >= "2014-01-01", sector == "Infrastructure" | sector == "Mortgage") %>%
  mutate(year = year(date)) %>%
  group_by(year, sector) %>%
  summarize(total_issuance = sum(total)) %>%
  ggplot(aes(x = year, y = total_issuance, fill = sector)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  ylab("Total Equity Issuance, $MM") + xlab("Year") +
  ggtitle("Secondary Equity Offerings", "By Sector, 2014 - 2019")



# Infrastructure REITs
infrastructure_reits <- reit %>% 
  filter(date >= "2015-01-01") %>%
  filter(under1 %in% as.matrix(underwriters) | under2 %in% as.matrix(underwriters) | under3 %in% as.matrix(underwriters) | under4 %in% as.matrix(underwriters) | under5 %in% as.matrix(underwriters) | under6 %in% as.matrix(underwriters)) %>%
  filter(!is.na(under2)) %>%
  filter(sector == "Infrastructure") %>%
  group_by(name) %>%
  summarize(offerings = n(), total_issuance = sum(total, na.rm = TRUE), average = total_issuance / offerings) %>%
  select(name, offerings, total_issuance, average) %>%
  arrange(desc(offerings))

# Mortgage REITs
mortgage_reits <- reit %>% 
  filter(date >= "2015-01-01") %>%
  filter(under1 %in% as.matrix(underwriters) | under2 %in% as.matrix(underwriters) | under3 %in% as.matrix(underwriters) | under4 %in% as.matrix(underwriters) | under5 %in% as.matrix(underwriters) | under6 %in% as.matrix(underwriters)) %>%
  filter(!is.na(under2)) %>%
  filter(sector == "Mortgage") %>%
  group_by(name) %>%
  summarize(offerings = n(), total_issuance = sum(total, na.rm = TRUE), average = total_issuance / offerings) %>%
  select(name, offerings, total_issuance, average) %>%
  arrange(desc(offerings)) %>%
  head(15)

# Write to Excel
wb <- loadWorkbook("REIT_offerings.xlsx", create = FALSE)

createSheet(wb, name = "Mortgage")
createSheet(wb, name = "Infrastructure")
createSheet(wb, name = "REITs2020")
createSheet(wb, name = "Mortgage2020")

writeWorksheet(wb, mortgage_reits, "Mortgage")
writeWorksheet(wb, infrastructure_reits, "Infrastructure")
writeWorksheet(wb, reits_recent, "REITs2020")
writeWorksheet(wb, mortgage_2020, "Mortgage2020")

saveWorkbook(wb)



# Individual names, Infrastructure
reit %>%
  filter(name == "Hannon Armstrong Sustainable Infrastructure Capital, Inc.") %>%
  select(date, under3, under4, under5, under6) 

reit %>%
  filter(name == "Crown Castle International Corp. (REIT)") %>%
  select(date, under3, under4, under5, under6) 

reit %>%
  filter(name == "CorEnergy Infrastructure Trust, Inc.") %>%
  select(date, under3, under4, under5, under6) 

# Individual names, Mortgage
reit %>%
  filter(name == "New York Mortgage Trust, Inc.") %>%
  select(date, under1, under2, under3, under4, under5, under6) 

reit %>%
  filter(name == "Annaly Capital Management, Inc.") %>%
  select(date, under1, under2, under3, under4, under5, under6) 

reit %>%
  filter(name == "New Residential Investment Corp.") %>%
  select(date, under1, under2, under3, under4, under5, under6)

reit %>%
  filter(name == "Apollo Commercial Real Estate Finance, Inc.") %>%
  select(date, under1, under2, under3, under4, under5, under6)
