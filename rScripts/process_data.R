# Purpose: The process.R script loads the previously extracted raw data into 
# memory and processes it. Its major task is to construct tiny data frames which
# will be saved as .csv files. Those .csv files will later be consumed by the
# UI layer to visualize the data.
#
# process.data.R consists of four mayor parts:
#  1. Definition of utitility functions
#  2. Process OpenAir and LabCase raw data
#  3. Create and save .csv files
#
# Google's R Style Guide (http://bit.ly/12ZBd1J) was applied while writing 
# the code below.

# if statement added because the rJava package referenced by the xlsx package 
# does not work when JAVA_HOME is set on Windows Server 64 Bit 
# see http://bit.ly/10OGlQx (stackoverflow)
if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")


#-------------------------------------------------------------------------------
# Definition of utility functions

mapToAcronym <- function(names){
  # Maps the complete methodology names to an abbreviated version of the name
  #
  # Args:
  #   names: character vector containing the complete methodology names 
  #         suffixes
  # 
  # Returns:
  #   Vector with abbreviated methodology names
  output <- rep(NA, length(names))
  for(i in 1:length(names))
    output[i] <- ifelse(names[i] == 'Process Improvement Methodology', 'General',
          ifelse(names[i] == 'Governance Risk and Compliance', 'GRC',
          ifelse(names[i] == 'Project Management', 'Project Mgm',
          ifelse(names[i] == 'Cockpit and Prime-to-Go', 'Ckpt and PtG',
          ifelse(names[i] == 'Project Governance', 'Project Gov',
          ifelse(names[i] == 'Master Data Management', 'MDM',
          ifelse(names[i] == 'Enterprise Integration', 'EI',
          ifelse(names[i] == 'Process Intelligence', 'PI',
          ifelse(names[i] == 'Process-driven SAP Management', 'PDSAP',
          ifelse(names[i] == 'Process-Driven SAP Management', 'PDSAP',
          ifelse(names[i] == 'Training Management', 'Trng Mgm',
          ifelse(names[i] == 'Business Process Management', 'BPM',
          ifelse(names[i] == 'Business Process Management ', 'BPM',
          ifelse(names[i] == 'Business Process Analysis', 'BPA',
          ifelse(names[i] == 'Enterprise Architecture', 'EA',
          names[i])))))))))))))))
  return (output)
}

#-------------------------------------------------------------------------------
# Process OpenAir and LabCase raw data


# Process Open Air data

oa.voluntary.raw <- read.csv('./rawData/prime_voluntary_2013.csv', 
                             header=TRUE, encoding='UTF-8')
oa.billable.raw <- read.csv('./rawData/prime_bookable_2013.csv',
                            header=TRUE, encoding='UTF-8')
# Exclude numbers from the GCS Portfolio and Methodology team
oa.billable.raw <- subset(oa.billable.raw,
                          Phase != 'Prime - Process Improvement Methodology')

# Extract total billable and total voluntary hours spent
total.vol.hours <- sum(oa.voluntary.raw$Approved.hours)
total.bill.hours <- sum(oa.billable.raw$Approved.hours)

# Add cost_type column to data frames to separate billable from voluntary 
# work later (V: Voluntary, B: Billable)
oa.voluntary.raw$cost_type <- rep('V', dim(oa.voluntary.raw)[1])
oa.billable.raw$cost_type <- rep('B', dim(oa.billable.raw)[1])

# Merge billable and voluntary project
oa.processed <- rbind(oa.voluntary.raw, oa.billable.raw)

# Process merged OpenAir raw data
names(oa.processed) <- tolower(names(oa.processed))
names(oa.processed)[names(oa.processed) == 'phase'] <- c('methodology')
oa.processed$methodology <- gsub('Prime for |Prime |Prime - ', "",
                                 oa.processed$methodology)
oa.processed <- transform(oa.processed,
                          user = gsub(" ", "", user),
                          days.spent = approved.hours/8)


# Process LabCase data (employee list)

# Load employee-country-mapping
empl.country.map <- read.xlsx('./rawData/employeeList2013.xlsx', 
                            sheetIndex = 1, encoding = 'UTF-8')
names(empl.country.map)[1] <- 'user'

# Merge OpenAir report with employee/country excel file
oa.pro.mer <- merge(oa.processed, empl.country.map, by = c('user'), all.x=TRUE)


# Process LabCase data (project list)

lc.prime.tasks <- read.xlsx('./rawData/lcPrimeTasks.xls',
                            sheetIndex = 1, enconding = 'UTF-8')
names(lc.prime.tasks) <- tolower(names(lc.prime.tasks))
names(lc.prime.tasks)[names(lc.prime.tasks) == 'x..done'] <- c('done')
lc.prime.tasks$project <- gsub('Prime for |Prime |Prime - ', "",
                               lc.prime.tasks$project)
lc.prime.tasks <- transform(lc.prime.tasks, 
                            done = as.numeric(as.character(done)))


#-------------------------------------------------------------------------------
#Create and save .csv files
#
#Overview page
#

# Calculate total investment in person days per methodology
totalInvestByMethInPersonDays <- ddply(oa.pro.mer, c('methodology'), 
                                       summarize, 
                                       daysSpent = round(sum(days.spent),
                                                         digits = 1))
totalInvestByMethInPersonDays$methodology <- mapToAcronym(
  totalInvestByMethInPersonDays$methodology)
write.csv(totalInvestByMethInPersonDays, 
          file = './rOutput/totalInvestByMethInPersonDays.csv', 
          row.names = FALSE)


# Calculate total investment in K euros per methodology
totalInvestByMethInEuros <- ddply(oa.pro.mer, c('methodology'), 
                                  summarize, 
                                  eurosSpent = round(sum(approved.actual.cost..eur.)/1000,
                                                     digits = 1))
totalInvestByMethInEuros$methodology <- mapToAcronym(
  totalInvestByMethInEuros$methodology)
write.csv(totalInvestByMethInEuros, 
          file = './rOutput/totalInvestByMethInEuros.csv', row.names = FALSE)


# Calculate total investment in person days per methododoly per country
totalInvestByCountry <- ddply(oa.pro.mer, c('methodology','country'), 
                              summarize, daysSpent = round(sum(days.spent),
                                                           digits=1))
totalInvestByCountry$methodology <- mapToAcronym(
  totalInvestByCountry$methodology)
write.csv(totalInvestByCountry, 
          file = './rOutput/totalInvestByCountryInPersonDays.csv', 
          row.names = FALSE)


# Generate contributers by country table
daysSpentByContributor <- subset(oa.pro.mer, 
                                 select=c(user, methodology, country, 
                                          days.spent, cost_type))
daysSpentByContributor$methodology <- mapToAcronym(
  daysSpentByContributor$methodology)
# Transform long data to wide data
daysSpentByContributor <- dcast(daysSpentByContributor, 
                                user + methodology + country ~ cost_type, sum, 
                                value.var = "days.spent")
# Add total.days.spent column to data frame
daysSpentByContributor <- transform(daysSpentByContributor, total.days = B + V)

# Duplicate methodolgy column in order to have another filter option available 
# on the UI layer
daysSpentByContributor$additional_meth <- daysSpentByContributor$methodology
write.csv(daysSpentByContributor, 
          file = './rOutput/daysSpentByContributor.csv', row.names = FALSE)


# Generate total days table
totalVolDays = round(total.vol.hours/8, digits = 1)
totalBillDays = round(total.bill.hours/8, digits = 1)
totalDays <- as.data.frame(cbind(
  totalDays = totalVolDays + totalBillDays,
  totalVolDays, totalBillDays))
write.csv(totalDays, file = './rOutput/totalDays.csv', row.names = FALSE)


# Generate release progress by methodolgy table
#Exclude non-methodogoly entries
releaseProgressByMethodolgy <- subset(lc.prime.tasks,
                                      (!project %in% c('Cockpit and Prime-to-Go',
                                                    'Process Improvement Methodology', 
                                                    'Training Management')))
# Exclude work package trackers
releaseProgressByMethodolgy <- subset(releaseProgressByMethodolgy, 
                                      tracker != 'Work package')
releaseProgressByMethodolgy <- ddply(releaseProgressByMethodolgy, 
                                     c('project'), summarize, 
                                     achievementInPercent = mean(done))
releaseProgressByMethodolgy$project <- mapToAcronym(releaseProgressByMethodolgy$project) 
write.csv(releaseProgressByMethodolgy, 
          file='./rOutput/releaseProgressByMethodolgy.csv', row.names=FALSE)


#
#Generation of MashZone relevant CSV files
#
#Details page
#
#Todo (alem)
