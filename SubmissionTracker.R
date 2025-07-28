
library(tidyverse)
library(googledrive)
library(googlesheets4)
library(httr)

setwd("C:/Users/mab3351/OneDrive - Northwestern University/Devidence/Assignment_App_Public")
#setwd("/Users/makaylabarker/Library/CloudStorage/OneDrive-NorthwesternUniversity/Devidence/DevidenceApp2.0")

################################################################################
# 1. Authorize Google Drive
################################################################################

gs4_auth()

dev_papers_dir = "Study Assignment/paper_assign_publicTemplate/Papers"
paper_assign_filename <- "Study Assignment/paper_assign_publicTemplate/paper_assign_template"

################################################################################
# 2. Load SurveyCTO Credentials
################################################################################
# Load SurveyCTO Credentials doc from google drive

credentials_filepath <-"Study Assignment/paper_assign_publicTemplate/SurveyCTO_Credentials"

credentials_file <- drive_get(credentials_filepath)$id
cto_credentials <- read_sheet(credentials_file)

# Store username and password
username <- cto_credentials$username
password <- cto_credentials$password

################################################################################
# 3. Load in Data from SURVEYCTO Server
################################################################################


SurveyNames <- c("Stage1", "Stage1_Check", "Stage2a", "Stage2b", "Appending")

SurveyForms <- c("https://gprl.surveycto.com/api/v1/forms/data/csv/stage1_v3",
                "https://gprl.surveycto.com/api/v1/forms/data/csv/stage1check_v3",
                 "https://gprl.surveycto.com/api/v1/forms/data/csv/stage2a_v3",
                 "https://gprl.surveycto.com/api/v1/forms/data/csv/stage2b_v3",
                 "https://gprl.surveycto.com/api/v1/forms/data/csv/append_v3")

new_data <- map(SurveyForms, ~GET(.x, authenticate(username, password)) %>% httr::content("text"))
names(new_data) <- SurveyNames

new_data <- imap(new_data, ~{
              
         df <-  read.csv(textConnection(.x)) 
         
         if ("X_enum" %in% names(df)){
          df <- rename(df, enum = X_enum)
         }
         
         if ("G_studyID.studyID" %in% names(df)){
           df <- rename(df, studyID = G_studyID.studyID)
         }
         
         df %>%
            select(SubmissionDate, matches("enum$", ignore.case = FALSE), studyID) %>%
            mutate(
            enum = gsub("\\s*\\(.*?\\)", "", enum),
            SubmissionDate = as.Date(SubmissionDate, format = "%B %d, %Y %I:%M:%S %p")) %>%
           rename(!!.y := SubmissionDate)
  })

new_data <- reduce(new_data, full_join) %>%
            filter(!studyID == "") %>%
            select(studyID, enum, everything())

################################################################################
# 4. Load in study tracker IDs
################################################################################


coders_file <- drive_get(paper_assign_filename)$id

current_studyIDs <- read_sheet(coders_file, sheet = "coding") %>%
                    mutate(across(c("studyID", "Supervisors", "Coders"), as.character)) %>%
                    mutate(across(c("Stage1", "Stage1_Check", "Stage2a", "Stage2b", "Appending"), as.Date))
                      

################################################################################
# 5.  Reshape submission data
################################################################################


stage_one <- filter(new_data, !is.na(Stage1) | !is.na(Stage1_Check)) %>%
             select(studyID, enum, Stage1, Stage1_Check) %>%
             rename(Supervisors = enum) %>% 
             group_by(studyID) %>%
             mutate(index = row_number())

stage_two <- filter(new_data, !is.na(Stage2a) | !is.na(Stage2b) | !is.na(Appending)) %>%
             select(studyID, enum, Stage2a, Stage2b, Appending) %>%
             rename(Coders = enum) %>%
            group_by(studyID) %>%
            mutate(index = row_number())

################################################################################
# 6. Join Submission Data
################################################################################


all_submissions <- full_join(stage_one, stage_two, by = c("studyID", "index")) %>%
        select(-index)

all_submissions <- full_join(current_studyIDs, all_submissions)

write_sheet(all_submissions, ss = coders_file, sheet = "coding")


     


