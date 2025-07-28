# DEVIDENCE_paper_assign_Public

Paper_assignment Steps:

Set up a folder structure in Google Drive
 - Study_assignment
 - Papers (empty folder)
 - Paper_assign_template (template file)

Open paper_assign_template
 - Navigate to the “staff” tab 
 - Add names of staff members and their positions. Optional positions include “Supervisor”, “Sub-Supervisor”, and “coder” 

Authorize Google account (Section 3)
 - Make sure that the authorized account has edit (read & write) access to the Study_assignment folder above
 - Set dev_papers_dir path to empty “Papers” folder
 - Set paper_assign_filename to “Paper_assign_template” file
 
Define Intervention types in section 5. 
 - Change “Intervention 1, Intervention 2, Intervention 3, etc.” 
 - If desired, define extra screening criteria. Remember to change column headings in paper_assign_template and in section 4a/4b



Submission Tracker Steps: 

 Set the working directory
 
Authorize Google account (step 1)
 - Make sure that the authorized account has edit (read & write) access to the Study_assignment folder above
 - Set dev_papers_dir path to empty “Papers” folder
 - Set paper_assign_filename to “Paper_assign_template” file
 
Set up SurveyCTO credentials file (step 2)
 - Store SurveyCTO Credentials file in same google drive folder as paper_assign_template
 - Add username and password to SurveyCTO Credentials file in drive
 - Change filepath accordingly in the SubmissionTracker script (step2)
 
Edit Survey names and form urls (step 4)
 - In the SurveyNames vector, add survey names of your choosing
 - In the SurveyForms vector, change “server_name” to your server name in each url, and “form_name” to each form’s ID. 
 
Edit Column names in paper_assign_template 
 - In the coding tab of paper_assign_template, update columns “stage 1”, “stage 1 check”, “stage 2a”, and “stage 2b” to match your survey names
 - Replace these names in steps 4,5, & 6 of the SubmissionTracker script.
