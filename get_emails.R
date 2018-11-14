# SUMMARY
# the code contains 2 functions. The first (get_names) loops through the 200+ pages
# of search results and returns a data frame with the following information:
# name:         Each person's name with any alternate names removed.
# url:          The URL for each person's secondary page of information.
# isCGA:        A logical value of whether or not that person has a CGA license.
# isActiveCGA:  A logical value of whether or not the CGA license is active. This column defaults
#               to FALSE for those who do not have a CGA license.
#
# The second function (get_CGA_emails) opens the secondary pages for only those people with an
# active CGA license. It takes as input the output from the first function and returns a data
# frame with the same information as above PLUS:
# email:        The email associated with the active CGA license. A value of NA is present when
#               no email is associated with the active CGA license.

# INSTRUCTIONS
# 1) Install the "rvest" package
# 2) Click "Source" in the upper right hand corner of this window
# 3) Move to the "Console" window below and run the first function:
#    "all_names_df <- get_names()" (THIS MAY TAKE UP TO 10 MINUTES)
# 4) [OPTIONAL] If, you want to save the all_names_df to a text file on your computer, type:
#    "write.table(all_names_df, file = "all_names_df.txt", quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)"
# 5) Now run the second function that gets the emails for only the active CGAs:
#    "activeCGA_df <- get_CGA_emails(all_names_df)" (THIS MAY TAKE UP TO 35 MINUTES)
# 6) To save this final data frame with the emails to your computer, type:
#    "write.table(activeCGA_df, file = "activeCGA_df.txt", quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)"

#=========================================================================================

#Packages needed
library(rvest)

#***THE FIRST FUNCTION***=================================================================
get_names <- function(){
  
  #Reading the URL you gave me
  page1 <- read_html("https://www.talcb.texas.gov/apps/license-holder-search/?lic_name=&industry=Appraisers&email=&city=&county=&zip=&display_status=active&lic_hp=&license_search=Search")
  
  #Getting the results information
  results_string <- page1 %>% 
    html_nodes(".paginator-description") %>%
    html_text()
  
  #Getting the number of pages
  num_pages <- sub(".*of ", "", results_string[1])
  num_pages <- sub(" \\(.*", "", num_pages) %>% as.integer()
  
  #Getting the number of results
  num_results <- sub(".*\\(", "", results_string[1])
  num_results <- sub(" license.*", "", num_results) %>% as.integer()
  
  #Setting up empty vectors to store data
  names_vector <- c()
  href_vector <- c()
  isCGA <- c()
  isActiveCGA <- c()
  
  #Looping through every page
  for(i in 1:num_pages){
    
    #There are 25 names on each page (nodes 2 - 26) of 33 nodes that are returned...
    #Unless it is the last page when there may be less
    
    #Read a page
    page <- read_html(paste0("https://www.talcb.texas.gov/apps/license-holder-search/?lic_name=&industry=Appraisers&email=&city=&county=&zip=&display_status=active&lic_hp=&license_search=Search&showpage=", i))
    
    #Get the h5 html nodes (the names are in here somewhere)
    h5_nodes <- html_nodes(page, "h5")
    
    #Narrow down to only those html nodes that contain names
    #This also accounts for the last page that may have less than 25 results
    if(length(h5_nodes)==33){
      name_nodes <- h5_nodes[2:26]
    } else{
      name_nodes <- h5_nodes[2:(1+(num_results%%25))]
    }
    
    #Collect the names
    names_vector <- c(names_vector, html_text(name_nodes))
    
    #Remove any alternate names from the names vector
    names_vector <- sub(" \\(Alternate Name.*", "", names_vector)

    #Collect the ids for pages that contain the emails
    href_vector <- c(href_vector, html_attr(html_nodes(name_nodes, "a"), "href"))
    
    #Construct the unique urls
    url_vector <- paste0("https://www.talcb.texas.gov/apps/license-holder-search/", href_vector)
    
    #Get the basic information for each name
    record_fluid_all <- html_nodes(page, "[class=record-fluid]")
    
    #Loop through each person on each page
    for(j in 1:length(record_fluid_all)){
      
      #Get the rows for each person
      rows_per_person <- html_nodes(record_fluid_all[j], "[class=row]")
      
      #Set up an empty vector that will tell me which rows for each person is a CGA row
      CGA_rows <- c()
      
      #Fill the CGA_rows vector
      for (k in 1:length(rows_per_person)){
        if(grepl("Certified General Appraiser", html_text(rows_per_person[k]))==TRUE){
          CGA_rows <- c(CGA_rows, TRUE)
        }else{
          CGA_rows <- c(CGA_rows, FALSE)
        }
      }
      
      #If any of the elements in CGA_rows==TRUE, then that person is a CGA
      if(length(which(CGA_rows))==0){
        isCGA <- c(isCGA, FALSE)
        isActiveCGA <- c(isActiveCGA, FALSE)
      } else{
        isCGA <- c(isCGA, TRUE)
        isActiveCGA <- c(isActiveCGA, grepl("Active", html_text(rows_per_person[which(CGA_rows)])))
      }
    }
  }
  
  #Add all info to a data frame
  all_names_df <- data.frame(names_vector, url_vector, isCGA, isActiveCGA, stringsAsFactors = FALSE)
  
  #Rename the columns
  colnames(all_names_df) <- c("name", "url", "isCGA", "isActiveCGA")
  
  #Return the data frame to the user
  return(all_names_df)
}

#***THE SECOND FUNCTION***================================================================
get_CGA_emails <- function(all_names_df){
  
  #Make a new data frame with only the active CGAs
  activeCGA_df <- subset(all_names_df, (all_names_df$isCGA==TRUE)&(isActiveCGA==TRUE))
  
  #On each secondary page, I need to look for the <div class="record-fluid">
  #with "Certified General Appraiser, License #"
  
  #Set up an empty vector to hold the emails
  email <- c()
  
  #Loop through every active CGA's secondary page
  for(i in 1:dim(activeCGA_df)[1]){
    
    #Load each person's secondary page
    sec_page <- read_html(activeCGA_df$url[i])
    
    #Get the panels for each person
    sec_record_fluid_all <- html_nodes(sec_page, "[class=record-fluid]")
    
    #Create an empty vector that will tell me which panel holds the CGA information
    CGA_panels <- c()
    
    #Fill the CGA_panels vector so that I will know which panel holds the CGA information
    for (j in 1:length(sec_record_fluid_all)){
      if(grepl("Certified General Appraiser, License #", html_text(sec_record_fluid_all[j]))==TRUE){
        CGA_panels <- c(CGA_panels, TRUE)
      }else{
        CGA_panels <- c(CGA_panels, FALSE)
      }
    }
    
    #The index that is TRUE in CGA_panels is the panel with CGA information
    final_CGA_panel <- sec_record_fluid_all[which(CGA_panels==TRUE)]
    
    #Find where the CGA email lives on the page
    email_address_index <- which(html_text(html_nodes(final_CGA_panel, "div"))=="Email Address")
    email_attr <- html_attrs(html_nodes(final_CGA_panel, "div")[email_address_index+1])[[1]][[2]]
    
    #Now that I know where the email lives, extract it
    email_block <- html_nodes(final_CGA_panel, paste0("[id=", email_attr, "]"))
    removal_nodes <- email_block %>% html_nodes("span")
    xml_remove(removal_nodes)
    
    #Add the extracted email to the email vector
    email <- c(email, html_text(email_block))
  }
  
  #Add the emails to the activeCGA_df
  activeCGA_df$email <- email
  
  #If no email was found, make NA
  activeCGA_df$email[activeCGA_df$email==""] <- NA
  
  #Return the data frame to the user
  return(activeCGA_df)
}
