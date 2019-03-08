# SUMMARY
# I've strucutured the code into 3 functions: 
#
# 1) The first (Get_PDF_File_Names) loops through a directory and any subdirectories
# and returns a list of the PDF files present.
# 2) The second function (Extract_PDF_Text) extracts the text from a PDF and if a blank 
# string is produced it will then OCR the PDF before re-extracting the text. This function
# then assesses if the PDF text contains keywords. The function will then return TRUE/FALSE,
# indicating if the PDF does contain\does not contain the keywords.
# 3) The third function (Create_DF) returns a dataframe of the PDF paths, names, keywords, and 
# the results of the second function (TRUE) for PDFS that do contain the matching keywords. 
#
# INSTRUCTIONS
# 1) Install the "tesseract" and "pdftools" packages
# 2) Change directory/keywords under "Run the Functions" Section
# 3) Click "Source" in the upper right hand corner of this window
#
#=========================================================================================

# Import Packages
library(tesseract)
library(pdftools)
library(tcltk)


#***Allow User to Select Directory***=====================================================
tk_choose.dir(default= "", caption = "Select Directory")


#***THE FIRST FUNCTION***=================================================================
Get_PDF_File_Names <- function(directory){
  ### This function loops over a directory and any subdirectories and stores PDF names into a vector (pdf_files): 
  ###     1. The name of the PDF 
  ###     2. The complete path of the PDF
  ###     3. The keywords 
  ###     4. Whether or not the keywords are found in the PDF Text 
  ###     (Results from Extract_PDF_Text Funciton
  
  # Get the names of pdfs in a directory and subdirectories 
  #directory <- gsub('\\\\', '//', directory)
  pdf_files <- list.files(directory, pattern="\\.pdf$", full.names=TRUE, recursive = TRUE)
  
  # Return pdf file names
  return(pdf_files)
}

#***THE SECOND FUNCTION***=================================================================
Extract_PDF_Text <- function(pdf_file, keywords){
  ### This function will extract the text from a PDF whether or not it is OCRed already. 
  ### The function also searches the extracted PDF text and determines if a set of 
  ### Keywords can be found in the text.
  ### Arguments: pdf_file - the file path & name of the PDF used to extract text.
  ### Returns: Results - A TRUE/False field that indicates wheter or not the the keywords 
  ###                    exist in the PDF.
  
  # Try to extract text from PDF (as if it was already OCRed)
  # Obtain the number of pages in the PDF
  pdf_length <- pdf_info(pdf_file)$pages
  
  # Convert all of the PDF pages to text
  text <- pdftools::pdf_text(pdf_file)[1:pdf_length]
  
  # Create a single vector with text from every page
  text <- paste(text[c(1:pdf_length)], collapse="")

  
  # If a NULL text string is produced, then the PDF will be OCRed and the text will be extracted
  if (text == ""){
    # Covert the PDF pages to images
    img_file <- pdftools::pdf_convert(pdf_file, format='tiff', pages=1:pdf_length, dpi=400)
    
    # Extract the text from each page's image
    text <- ocr(img_file)
    
    # Delete the image fils created
    unlink(img_file)
    
    # Create a single vector with text from every page 
    text <- paste(text[c(1:length(text))], collapse="")
  }

  # Create an empty vector to hold the results (TRUE/FALSE if keywords exist in the PDF)
  results1 <- c()
  results2 <-c()
  
  ## Check to see if keywords are found in the extracted text
  #for (k in 1:length(keywords)){
  #  if (grepl(keywords[k], text, ignore.case = True))
  #  results[k] <- (grepl(keywords[k],text,ignore.case=TRUE))
  #}
  #results <- grepl(paste(keywords, collapse="|"),text, ignore.case=TRUE)
  for (k in 1:length(keywords)){
    results1[k] <- (grepl(keywords[k],text,ignore.case=TRUE))
  }
  
  results2 <- paste(results1, collapse=" ")
  
  # Return Results
  return(results2)
}

#***THE THIRD FUNCTION***==================================================================
Create_DF <- function(pdf_files, keywords, results){
  ### This function creates a dataframe with 4 fields: 
  ###     1. The name of the PDF 
  ###     2. The complete path of the PDF
  ###     3. The keywords 
  ###     4. Whether or not the keywords are found in the PDF Text 
  ###     (Results from Extract_PDF_Text Funciton
  
  # Create vectors to include in  Dataframe
  pdf_paths <- pdf_files
  pdf_names <- basename(pdf_files) 
  keywords <- paste(keywords[c(1:length(keywords))], collapse="  ")
  
  # Create a dataframe of the PDF name & path, keywords, and results
  data_df <- data.frame(pdf_paths, pdf_names, keywords, results, stringsAsFactors = FALSE)

  # Rename the colums of the dataframe
  colnames(data_df) <- c("PDFPath", "PDFName", "Keyword", "IsPresent")
  
  # Remove any rows that have FALSE results
  ###############################data_df <- data_df[results != FALSE]
  #data_df_sub <- subset(data_df, IsPresent!=FALSE)
  #data_df <- data_df[results != '*FALSE*']
  data_df <- data_df[!grepl("FALSE", data_df$IsPresent),]
  
  # Export the dataframe to a CSV file and save to the local directory
  write.table(data_df, file = "PDF_text.csv", quote = FALSE, sep=",", row.names = FALSE, col.names = TRUE)
  
  # Return data frame
  return(data_df)
}


#***RUN THE Functions***===================================================================

## Define Variables (PDF and Keywords)
#directory <- "S:/RE/Melinda/RE_GIS/PDF_extract"
pdf_files <- Get_PDF_File_Names(directory)
keywords <- c("Allen", "encroachment")

## Loop the Functions
res <- c()
for (f in 1:length(pdf_files)){
  res[f] <- Extract_PDF_Text(pdf_files[f], keywords)
}
csv <- Create_DF(pdf_files,keywords,res)
