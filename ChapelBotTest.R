#' ---
#' title: "ChapelBotR"
#' output: null_document
#' date: "`r Sys.Date()`"
#' ---
#' 
## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------
if(!require(dplyr)) {install.packages("dplyr"); library(dplyr)}
if(!require(tidyverse)) {install.packages("tidyverse"); library(tidyverse)}
if(!require(rvest)) {install.packages("rvest"); library(rvest)}
if(!require(RSelenium)) {install.packages("RSelenium"); library(RSelenium)}
if(!require(wdman)) {install.packages("wdman"); library(wdman)}
if(!require(netstat)) {install.packages("netstat"); library(netstat)}
if(!require(xml2)) {install.packages("xml2"); library(xml2)}
if(!require(webdriver)) {install.packages("webdriver"); library(webdriver)}
if(!require(purrr)) {install.packages("purrr"); library(purrr)}
if(!require(readr)) {install.packages("readr"); library(readr)}
if (!require(usethis)) {install.packages("usethis"); library("usethis")}
if (!require(dotenv)) {install.packages("dotenv"); library("dotenv")}
if (!require(here)) {install.packages("here"); library("here")}
if (!require(gmailr)) {install.packages("gmailr"); library(gmailr)}
if(!require(httr)) {install.packages("httr"); library(httr)}
if(!require(lubridate)) {install.packages("lubridate"); library(lubridate)}
if(!require(jsonlite)) {install.packages("jsonlite"); library(jsonlite)}
if(!require(base64enc)) {install.packages("base64enc"); library(base64enc)}
setwd(here())
# Define .env content
env_content <- ""
# Write to .env file in the current working directory
if (nchar(env_content) > 0) {
  cat(env_content, file = ".env", append = TRUE, sep = "\n")
}
dotenv::load_dot_env(".env")
#Delete .env content
#writeLines("", ".env")
# Read the contents of the .env file
print(readLines(".env"))

#' 
## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Set TRUE to automatically download weekly updated on-campus roster from email
downloadUpdatedRoster <- TRUE
if (downloadUpdatedRoster == TRUE) {
  # Step 1: Set up authentication
  tryCatch({
      gm_auth_configure(path = "[CTE]GmailCredentials.json")
      gm_auth(email = TRUE, cache = ".secret")
  }, error = function(e) {
      print("Authentication failed: Could not fetch updated on-campus roster data.")
  })
  
  # Step 2: Search for messages from the last seven days with a specific subject
  subject_filter <- "StarRez Auto Report - Chapel Thrill Roster"
  since_date <- format(Sys.Date() - 5, "%Y-%m-%d")  # Gmail uses YYYY-MM-DD format
  
  # Construct the search query
  query <- sprintf('subject:"%s" after:%s', subject_filter, since_date)
  tryCatch({
    # Retrieve messages matching the query
    messages <- gm_messages(search = query, num_results = 1)  # Adjust num_results as needed
    
    # Step 3: Check for the attachment and save it
    if (length(messages) > 0) {
      # Assuming the first message is the correct one
      message_id <- messages[[1]]$messages[[1]]$id
      msg <- gm_message(message_id)
      # Check if there are attachments in the message
      if (!is.null(msg$payload$parts)) {
        for (part in msg$payload$parts) {
          if (part$filename != "" && grepl("\\.csv$", part$filename)) {
            # Define the path for saving the file
            save_path <- file.path(getwd(), "data", "Chapel Thrill Roster.csv")
            
            # Save the attachment
            attachment_data <- gm_attachment(part$body$attachmentId, message_id, user_id = "admin@chapelthrillescapes.com")
            raw_data <- base64decode(attachment_data$data)
            writeBin(raw_data, save_path)
            print(sprintf("Attachment saved as: %s", save_path))
          }
        }
      }
    } else {
      print("No messages found with subject 'StarRez Auto Report - Chapel Thrill Roster.'")
    }
  }, error = function(e) {
      print("Message retrieval failed: Could not fetch updated on-campus roster data.")
  })
}
PID_data <- read.csv("./data/Chapel Thrill Roster.csv")

# Bookeo API credentials
api_key <- Sys.getenv("api_key")
secret_key <- Sys.getenv("secret_key")
api_endpoint_bookings <- "https://api.bookeo.com/v2/bookings"
api_endpoint_customers <- "https://api.bookeo.com/v2/customers"
user_agent <- "ChapelBot"

start <- as.Date(format(Sys.Date(), "%Y-%m-%d"))  %m+% days(1)
end <- as.Date(format(Sys.Date(), "%Y-%m-01")) %m+% months(1)  # Last day of the next month

bookingResponse <- GET(
    url = api_endpoint_bookings,
    query = list(
      startTime = format(start, "%Y-%m-%dT%H:%M:%SZ"),
      endTime = format(end, "%Y-%m-%dT%H:%M:%SZ"),
      secretKey = secret_key,
      apiKey = api_key,
      expandParticipants = "true",
      itemsPerPage = 100
    ),
    add_headers(`User-Agent` = user_agent)
)

#Make the GET request 
#I didn't end up using the customer data but this could be helpful for other projects.
customerResponse <- GET(
  url = api_endpoint_customers,
  query = list(
    apiKey = api_key,
    secretKey = secret_key,
    createdSince = format(start, "%Y-%m-%dT%H:%M:%SZ"),
    expandParticipants = "true",
    itemsPerPage = 100
  ),
  add_headers(`User-Agent` = user_agent)
)

# MPJWRE is On-Campus Student ($14)
# UPEMYF is UNC Faculty/Staff ($20)
# JKCAFP is Student ($16)
# UYXFLE is Adult ($26)
# WYWRML is Child <15 ($20)

# Check the response status
if (http_status(bookingResponse)$category == "Success" & http_status(customerResponse)$category == "Success") {
  # Parse the JSON response
  bookingdata <- fromJSON(rawToChar(bookingResponse$content))$data
  customerdata <- fromJSON(rawToChar(customerResponse$content))$data
  cat(sprintf("Fetched %d booking(s) from Bookeo\n", nrow(bookingdata)))
  notoncampus <- data.frame(BookingID = character(), Name = character(), PID = character(), Name_match = logical(), PID_match = logical(), stringsAsFactors = FALSE)
  for (booking in 1:nrow(bookingdata)) {
    personinfo <- bookingdata[booking,]$participants$details[[1]]
    for (i in 1:nrow(personinfo)){
      if (personinfo[[2]][[i]] == "MPJWRE") {
        Name <- paste0(personinfo[[4]]$firstName[[i]], " ", personinfo[[4]]$lastName[[i]])
        PID <- personinfo[[4]]$customFields[[i]]$value
        PID_match <- any(PID_data$PID == PID)
        Name_match <- any(paste0(PID_data$Name.First[which(PID_data$PID == PID)], " ", PID_data$Name.Last[which(PID_data$PID == PID)]) == Name)
        match <- PID_match & Name_match
        if (!match) {
        # Handle the case where no match was found
        BookingID <- bookingdata[booking,]$bookingNumber
        print(paste("No match found for PID:", PID, "and Name:", Name))
        notoncampus <- rbind(notoncampus, data.frame(BookingID = BookingID, Name = Name, PID = PID, Name_match = Name_match, PID_match = PID_match, stringsAsFactors = FALSE))
        } else {
        # Handle the case where a match was found
        print(paste("Match found for PID:", PID, "and Name:", Name))
        }
      }
      else {
        next
      }
    }
  }
  prevnotoncampus <- read_csv("./data/notoncampus.csv", show_col_types = FALSE)
  if(length(unique(notoncampus$BookingID)) > 0) {
    for (i in 1:length(unique(notoncampus$BookingID))){
      cancelID <- unique(notoncampus$BookingID)[i]
      PIDs <- paste(notoncampus[notoncampus$BookingID == cancelID, ]$PID, collapse = ", ")
      Names <- paste(notoncampus[notoncampus$BookingID == cancelID, ]$Name, collapse = ", ")
      #reason <- paste0('ChapelBot has cancelled your booking due to no on-campus UNC student match being found for respective PID(s): ', PIDs, ' and Name(s): ', Names, '. You can rebook at chapelthrillescapes.com/book with a PID matching the names of the on-campus student(s) exactly as they appear on their Student ID or for off-campus students select "Student ($16)." If you think this cancellation is incorrect or have any questions, please send our team an email at admin@chapelthrillescapes.com.')
     
      notification_body <- paste0(
        'ChapelBot found a potential issue with booking ID: ', cancelID, 
        '. No on-campus UNC student match was found for respective PID(s): ', PIDs, 
        ' and Name(s): ', Names, 
        '. The booking HAS NOT been cancelled. Please review it manually.'
      )
      
      # Log the message to the console/log file
      cat("!!! WARNING - MANUAL REVIEW NEEDED !!!\n")
      cat(notification_body, "\n\n")
      
      # Option to send an email notification (Requires gmailr setup)
      # Assuming you want the email to go to 'admin@chapelthrillescapes.com'
      email_subject <- paste0("ACTION REQUIRED: UNC Student Match Issue for Booking ", cancelID)
      
      gm_send_message(
        mime(
          to = "admin@chapelthrillescapes.com", 
          subject = email_subject,
          body = notification_body
        )
      )
    }
      #response <- DELETE(
       # url = paste0(api_endpoint_bookings, "/", cancelID),
        #query = list(
         # notifyUsers = "true",
          #notifyCustomer = "true",
          #applyCancellationPolicy = "false",
          #secretKey = secret_key,
          #apiKey = api_key,
          #trackInCustomerHistory = "true",
          #reason = reason
        #),
        add_headers(`User-Agent` = user_agent)
       cat("Manual review notification sent — no cancellation performed.\n")
    }
    combined_notoncampus <- rbind(notoncampus, prevnotoncampus)
    write.csv(combined_notoncampus, "./data/notoncampus.csv", row.names = FALSE)
  } else {
      cat("No incorrect on-campus students to process.")
  }
} else {
    if (http_status(bookingResponse)$category == "Success" & http_status(customerResponse)$category != "Success") {
      cat("Error fetching customers: ", http_status(customerResponse)$message)
    } else if (http_status(bookingResponse)$category != "Success" & http_status(customerResponse)$category == "Success") {
      cat("Error fetching bookings: ", http_status(bookingResponse)$message)
    } else {
      cat("Error fetching bookings: ", http_status(bookingResponse)$message, "\n")
      cat("Error fetching customers: ", http_status(customerResponse)$message)
    }
}
