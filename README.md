################################################################################
#
# Code overview and how to run it - Weekly Aging Report
#
################################################################################

Each week, the service desk runs an extract of the Cherwell data base to collect
all open tickets. This file is then run through the program titled:

     3-weekly-aging-report.Rmd
     
to generate the weekly management report (3-weekly-aging-report.html). This 
README file explains the process for running the report.

Process steps

(1) The service desk follows these steps to do the data extract:
     - In Cherwell blue client:
          -Tools > Reports > Report Manager
          -Set Association drop-down to Incident
          -Select Global folder at left
          -On the right, right-click "A_Open Tickets by Team"
          -Select Export To > CSV
          -Wait a moment for it to load, then give the file a name and save it
          -Close Report Manager
          -Email the CSV as an attachment
     - The file extract should always be on a Friday, late in the day

(2) Upload the file as a Google Sheet in your top level Google Drive
     - This is important; it cannot be an xls or csv
     - Do not make any changes to the file

(3) Ensure the file is named in this format: CAR-input-YY-MM-DD
     - For example: CAR-input-19-09-20
     
(4) Load the program 3-weekly-aging-report.Rmd into RStudio

(5) Ensure the file name on line 56 has the correct name
     - The name must match the Google Sheet you created from the extract

(6) Click the "knit" button to generate the weekly report
     - Once the output appears, click on "Open in browser" near the top
     - Once open in the browser, right click to print as a PDF
     - Upload the PDF into the reports directory - the link is:
     
     https://drive.google.com/drive/u/1/folders/1EXZYWojMctNfcIpyL_YuaM-B0l-zqa4x
     
(7) Send the completed report to the appropriate managers

