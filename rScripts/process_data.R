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


# 1. Define utility functions---------------------------------------------------

cutNamePrefix <- function(names) {
  # Cuts the prefix of the OA phase name or the LC project name
  #
  # Args:
  #   names: character vector containing the complete methodology names
  #          including a prefix
  #        
  # 
  # Returns:
  #   Vector with full methodology names
  output <- gsub('^(Prime for |Prime |Prime - |Prime-)', "", names)
  return(output)
}


map_name_to_acronym <- function(names, mapping) {
  # Maps the complete methodology names to an abbreviated version of the name
  #
  # Args:
  #   names: character vector containing the complete methodology names
  #   mapping: character vector containing the mapping rules. Each entry has
  #            the following form "full name, acronym"
  #         
  # Returns:
  #   Vector with abbreviated methodology names
  
  full_names <- str_extract(mapping, '^.*,') %>%
    str_sub(1, str_length(.) - 1) %>%
    str_trim(side = "both")
  
  acronyms <- str_extract(mapping, ",.*") %>%
    str_sub(2, str_length(.)) %>%
    str_trim(side = "both")
  
  return(mapvalues(names, full_names, acronyms, warn_missing = FALSE))
  
}


mergeLcOaWorkPackageData <- function(oa.data.df, lc.data.df) {
  # Merges the LC and OA data on the work package level 
  #
  # Args:
  #   oa.data.df: Processed OA data
  #   lc.data.df: Processed LC data
  #         
  # Returns:
  #   A data frame containing the merged data and selected columns. In addition, 
  #   the column 'check' will show if a data problem exists in one of the source
  #   systems:
  #   0: no issue
  #   1: LC data issue 
  #   2: OA data issue
  
  oa.tmp <- group_by(oa.data.df, methodology, task, lc.issue.numb) %>%
    summarize(
      days.planned = round(sum(days.planned), digits = 2),
      days.spent = round(sum(days.spent), digits = 2)
    ) %>%
    rename(
      oa.days.planned = days.planned,
      oa.days.spent = days.spent
    )
  
  lc.tmp <- filter(lc.prime.tasks, tracker == 'Work package') %>%
    select(methodology, lc.issue.numb, subject, done)
  
  merged.df <- merge(lc.tmp, oa.tmp, by = 'lc.issue.numb', all = TRUE)
  # Add check column to highlight the source system in which a data problem exists
  # 0: no issue
  # 1: LC issue
  # 2: OA issue
  #m$check <- rep(0, dim(m)[1])
  merged.df$check <- ifelse(is.na(merged.df$subject), 1, 
                    ifelse(is.na(merged.df$task), 2 , 0
                    ))
  merged.df$methodology.x <- ifelse(is.na(merged.df$methodology.x), 
                                         as.character(merged.df$methodology.y),
                                         as.character(merged.df$methodology.x))
  merged.df$subject <- ifelse(is.na(merged.df$subject), 
                                   as.character(merged.df$task), 
                                   as.character(merged.df$subject))
  names(merged.df)[names(merged.df) == 'methodology.x'] <- 'methodology'
  
  merged.df <- select(merged.df, 
                      check, lc.issue.numb, methodology, subject, 
                      oa.days.planned, oa.days.spent, done)
  
  merged.df %<>% replace_na(list(
    oa.days.planned = 0,
    oa.days.spent = 0,
    done = 0
  ))
  
  return(merged.df)
}


get_issue_hierarchy  <- function(lc_data_df, issue_numbers) {
  # Identifies all LC report records belonging to a certain issue hierarchy 
  # given the issue numbers from the parent level.
  #
  #
  # Args:
  #   lc.data.df: LC issue list
  #   issue_number: issue numbers from the desisred parent level the search 
  #                 starts at
  #         
  # Returns:
  #   Vector containing issue numbers from the entire issue hierachy
  
  child_issues <- lc_data_df$lc.issue.numb[lc_data_df$parent.task %in% issue_numbers]
  
  if (length(child_issues) > 0) {
    tmp <- get_issue_hierarchy(lc_data_df, child_issues)
    issue_numbers <- c(issue_numbers, tmp)
  }
  return(issue_numbers)
}


merge_oa_project_timesheet_data <- function(project_df, timesheet_df) {
  #  Preprocesses and merges project-based and timesheet-based report data
  #  extracted from the same OA project. 
  #
  #  OA project-based reports return the correct number of planned hours per 
  #  Phase/Task/User drill-down which is stored in the 'All.assigned.hours' 
  #  column.
  #  Instead, the accurate information about time and Euros spent per 
  #  Phase/Task/User drill-down can only be captured by a timesheet-based 
  #  report which stores the information in the 'Approved.hours' and the 
  #  'Approved.actual.cost..EUR.' column.
  #
  # Args:
  #   project_df: OA project-based report
  #   timesheet_df: OA timesheet-based report
  #         
  # Returns:
  #   Dataframe containing merged report data
  
  timesheet_df %<>%
    setNames(tolower(names(.))) %>%
    select(task, user, 
           approved_hours = approved.hours, 
           approved_costs_EUR = approved.actual.cost..eur.) %>%
    unite(task_user, task, user) 
  
  project_df %<>%
    setNames(tolower(names(.))) %>%
    unite(task_user, task, user, remove = FALSE) %>%
    left_join(timesheet_df, by = "task_user") %>%
    rename(
      planned_hours = all.assigned.hours,
      methodology = phase
    ) %>%
    select(-date, -project, -task_user, -task.planned.hours) 
  
  return(project_df)
}


process_merged_oa_data <- function(oa_df, exclude_vec) {
  #  Processes a OA data report orginally created by merging a project-based and
  #  timesheet-based report.
  #
  # Args:
  #   oa_df: Merged OA report
  #   exclude_vec: Character vector of OA phase names to exclude
  #         
  # Returns:
  #   Processed merged OA report data
  
  oa_df %<>% filter(!methodology %in% exclude_vec) %>% 
    replace_na(list(
      approved_hours = 0,
      approved_costs_EUR = 0
    )) %>%
    mutate(
      methodology = cutNamePrefix(methodology),
      methodology = map_name_to_acronym(methodology, config$mapping),
      task = as.character(task),
      user = gsub(" ", "", user),
      days.planned = planned_hours / 8,
      days.spent = approved_hours / 8,
      cost_type = rep('B', dim(.)[1]),
      # Extract LC issue number and store result in separate column
      lc.issue.numb = as.numeric(sapply(regmatches(task, 
                                                   regexec('^([0-9]+)', task)),
                                        function(x)x[2])),
      # Add dummy issue number to OA tasks without LC representation
      index = group_indices_(., .dots = c("methodology", "task")),
      lc.issue.numb = ifelse(is.na(lc.issue.numb), index, lc.issue.numb)
    ) %>%
    select(-index)
  
  
  return(oa_df)
}


# 2. Process OpenAir and LabCase raw data --------------------------------------

# Process Open Air data

oa.proj.billable.df <- read.csv('./rawData/oa_proj_billable.csv',
                                 header = TRUE, encoding = 'UTF-8')
oa.timesheet.billable.df <- read.csv('./rawData/oa_timesheet_billable.csv',
                            header = TRUE, encoding = 'UTF-8')

oa.processed.df <- merge_oa_project_timesheet_data(oa.proj.billable.df,
                                         oa.timesheet.billable.df) %>% 
  process_merged_oa_data(config$exclude)

# Extract total billable and total voluntary hours spent
total.vol.hours <- sum(oa.processed.df$approved_hours[oa.processed.df$cost_type == "V"])
total.bill.hours <- sum(oa.processed.df$approved_hours[oa.processed.df$cost_type == "B"])



# Process LabCase data (employee list)

# Load employee-country-mapping
empl.country.map <- read_excel('./rawData/employeeCountryMapping.xlsx')
names(empl.country.map)[1] <- 'user'

# Merge OpenAir report with employee/country excel file
oa.pro.mer <- merge(oa.processed.df, empl.country.map, by = c('user'), 
                    all.x = TRUE)



# Process LabCase data (task list)

lc.prime.tasks <- read.csv('./rawData/lc_tasks.csv', 
                           header = TRUE, encoding = 'UTF-8') %>%
  mutate(
    methodology = cutNamePrefix(methodology),
    methodology = map_name_to_acronym(methodology, config$mapping)
  ) %>% filter(methodology != "Platform Rapid Innovation Methodology")


template_issues <- lc.prime.tasks %>%
  filter(subject == "[Template - Copy me and enter service package name]") %$%
  lc.issue.numb %>%
  get_issue_hierarchy(lc.prime.tasks, .)

lc.prime.tasks %<>% filter(!lc.issue.numb %in% template_issues)


                                  
# 3.1 Create and save .csv files (Overview page) -------------------------------

# Calculate total investment in person days per methodology
totalInvestByMethInPersonDays <- oa.pro.mer %>%
                                   group_by(methodology) %>%
                                   summarize(
                                     daysSpent = round(sum(days.spent), 
                                                       digits = 1)
                                     )
write.csv(totalInvestByMethInPersonDays, 
          file = './rOutput/totalInvestByMethInPersonDays.csv', 
          row.names = FALSE)



# Calculate total investment in K euros per methodology
totalInvestByMethInEuros <- oa.pro.mer %>%
                              group_by(methodology) %>%
                              summarize(
                                eurosSpent = round(sum(approved_costs_EUR)/1000,
                                                   digits = 2)
                                )
write.csv(totalInvestByMethInEuros, 
          file = './rOutput/totalInvestByMethInEuros.csv', row.names = FALSE)



# Calculate total investment in person days per methododoly per country
totalInvestByCountry <- oa.pro.mer %>%
                          group_by(methodology, country) %>%
                          summarize(
                            daysSpent = round(sum(days.spent),
                                              digits = 1)
                            )
write.csv(totalInvestByCountry, 
          file = './rOutput/totalInvestByCountryInPersonDays.csv', 
          row.names = FALSE)



# Create contributers by country table
daysSpentByContributor <- oa.pro.mer %>%
                            select(user, methodology, country, days.spent, 
                                   cost_type)
# Transform long data to wide data
daysSpentByContributor <- dcast(daysSpentByContributor, 
                                user + methodology + country ~ cost_type, sum, 
                                value.var = "days.spent")
# Add total.days.spent column to data frame
# Check if volunatary work exists
if ("V" %in% names(daysSpentByContributor)) {
  daysSpentByContributor <- mutate(daysSpentByContributor, total.days = B + V)
} else {
  daysSpentByContributor$V <- rep(0, nrow(daysSpentByContributor))
  daysSpentByContributor <- mutate(daysSpentByContributor, total.days = B)
}
# Duplicate methodolgy column in order to have another filter option available 
# on the UI layer
daysSpentByContributor$additional_meth <- daysSpentByContributor$methodology
write.csv(daysSpentByContributor, 
          file = './rOutput/daysSpentByContributor.csv', row.names = FALSE)



# Create total days table
totalVolDays = round(total.vol.hours/8, digits = 1)
totalBillDays = round(total.bill.hours/8, digits = 1)
totalDays <- as.data.frame(cbind(totalDays = totalVolDays + totalBillDays,
                                 totalVolDays, totalBillDays))
write.csv(totalDays, file = './rOutput/totalDays.csv', row.names = FALSE)



# Create release progress by methodology table and calculate overall 
# methodology achievement in percent
# Only include trackers which are tree leaves to ensure that every methodology
# is treated equally regardless if a time estimate was given or not. If a 
# certain issue ID is not present in the parent ID vector than the respective
# issue must be a leaf

lc_id <- lc.prime.tasks$lc.issue.numb
parent_id <- lc.prime.tasks$parent.task
leaf_id <- lc_id[!lc_id %in% parent_id]

releaseProgressByMethodology <- lc.prime.tasks %>% 
  filter(lc.issue.numb %in% leaf_id) %>%
  group_by(methodology) %>%
  summarize(
    achievementInPercent = sum(done) / n()
  ) 




write.csv(releaseProgressByMethodology, 
          file = './rOutput/releaseProgressByMethodolgy.csv', row.names = FALSE)



# 3.2 Create and save .csv files (Details page) --------------------------------

# Create merged work package status table
mergedOaLcWorkPackageStatus <- mergeLcOaWorkPackageData(oa.pro.mer, lc.prime.tasks)

write.csv(mergedOaLcWorkPackageStatus,
          file = './rOutput/megedOaLCWorkPackageStatus.csv', row.names = FALSE)



# Create spent days by contributor table 
oaDaysSpentByContributor <- group_by(oa.pro.mer, methodology, lc.issue.numb, 
                                     user) %>%
  summarize(
    days.planned = round(sum(days.planned), digits = 2),
    days.spent = round(sum(days.spent), digits = 2) 
  )

write.csv(oaDaysSpentByContributor, 
          file = './rOutput/oaDaysSpentByContributor.csv', row.names = FALSE)


# Create readiness/progress per work package table

readinessByServicePackage <- lc.prime.tasks %>% 
  filter(!is.na(parent.task), subject %in% c('General readiness', 
                                             'Consulting Readiness', 
                                             'Sales Readiness')) %>% 
  select(parent.task, subject, done) %>% 
  spread(subject, done) %>%
  left_join(lc.prime.tasks,. , by = c("lc.issue.numb" = "parent.task")) %>%
  filter(tracker == "Work package") %>%
  select(-c(tracker, subject, parent.task))

write.csv(readinessByServicePackage,
          file = "./rOutput/readinessByServicePackage.csv", row.names = FALSE)



readinessByPlatform <- lc.prime.tasks %>% 
  filter(subject %in% c("General readiness", "Sales Readiness",
                        "Consulting Readiness")) %>%
  group_by(methodology, subject) %>%
  summarize(
    done = sum(done)/n()
  ) %>%
  spread(subject, done)

write.csv(readinessByPlatform,
          file = "./rOutput/readinessByPlatform.csv", row.names = FALSE)
  
  


