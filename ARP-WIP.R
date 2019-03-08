

# To run this script:
# 	source ('exrtact.donors.and.addressess.R')


# First remove all old objects in memory
rm(list=ls(all=TRUE))

#install.packages("XML")
#install.packages("RCurl")
library(XML)
library(RCurl)
library('rjson')
library(stringdist)

# Install the Biobase library. See https://bioconductor.org/packages/release/bioc/html/Biobase.html
# To install use: 
# 	source("https://bioconductor.org/biocLite.R")
# 	biocLite("Biobase")
# We need Biobase use the the function subListExtract, which is needed below
library('Biobase')

getGoogleURL <- function(search.term, domain = '.com.au', quotes=TRUE) 
{
  search.term <- gsub(' ', '%20', search.term)
  if(quotes) search.term <- paste('%22', search.term, '%22', sep='') 
  getGoogleURL <- paste('http://www.google', domain, '/search?q=',
                        search.term, sep='')
}

getGoogleLinks <- function(google.url) {
  doc <- getURL(google.url, httpheader = c("User-Agent" = "R
                                           (2.10.0)"))
  html <- htmlTreeParse(doc, useInternalNodes = TRUE, error=function
                        (...){})
  nodes <- getNodeSet(html, "//h3[@class='r']//a")
  return(sapply(nodes, function(x) x <- xmlAttrs(x)[["href"]]))
}

setwd("C:/Users/Wel/Downloads/ARP")

# read in the data from the JSON file
x <- fromJSON( file='all_data.json' )

no.entries <- length(x$receipts)  # tot no is 40892

# get the list of parties
party.names <- x$parties



# extract the list of donor names by getting the sublist "Name" from every element of the list
donor.name.list <- subListExtract(x$entities, 'Name')

# convert the list to a vector
donor.name.vector <- unlist(donor.name.list)
#donor.name.vector <- tolower(donor.name.vector)
#donor.name.vector <- stri_trans_general(donor.name.vector,"Latin-ASCII")
donor.name<- as.data.frame(donor.name.vector)
donor.name$ID <- c(1:18073)

donor.name.vector <- donor.name.vector[11900:12200]
cat('\nExtracted', length(donor.name.vector), 'donor names')


# extract the list of donor address by getting the sublist "Name" from every element of the list
donor.address.list <- subListExtract(x$entities, 'Address')
donor.address.list[sapply(donor.address.list, is.null)] <- "No.address"

# convert the list to a vector
donor.address.vector <- unlist(donor.address.list)
donor.address.vector <- tolower(donor.address.vector)
donor.address <- as.vector(as.factor(donor.address.vector))
donor.address <- as.data.frame(donor.address)
donor.address$ID <- c(1:18073)

List.Donor <- merge(donor.name,donor.address,by="ID")

cat('\nExtracted', length(donor.address.vector), 'donor addresses\n')


# base function to build from for comparing the name strings
compare.strings <- function(x, current.name) {
  #cat('\n\nname1:', current.name, '\nname2:', x)
  string1 <- tolower(as.character(x))
  string2 <- tolower(as.character(current.name))
  
 dist.name<-stringdist(string1,string2, method = "soundex")# 0 means match
  dist.name2 <- stringdist(string1,string2,method = "jw")
  
  status <- "diff"
  
  
 
  
 if (dist.name2 <0.11 & dist.name == 0){
   cat('\n\nname1:', current.name, '\nname2:', x)
   
  cat('\ndist1:', dist.name)
   cat('\ndist2:', dist.name2)
   
   status <- "same"
    #return("same")
   
 }else{
   if(dist.name2 < .12 ){
     cat('\n\nname1:', current.name, '\nname2:', x)
     
    cat('\ndist1:', dist.name)
     cat('\ndist2:', dist.name2)
     
     status <- "check"
     #return("check") 
     }
 }
  
  if(status=='same' | status=='check') {
   
    cat(current.name, ',', x, ',', status,',',dist.name,',',dist.name2, '\n', file="output.csv", sep="", append=TRUE )
  
  }
  
  #search.url1 <- getGoogleURL(search.term=string1, quotes=FALSE)
  #search.url2 <- getGoogleURL(search.term=string2, quotes=FALSE)
  
  #links1 <- getGoogleLinks(search.url1)
  #links2 <- getGoogleLinks(search.url2)
  
 #browser()
    
    #if ( x == current.name )  {
        #cat('Same length names: current name', current.name, 'name to compare to:', x,  '\n')
      #  return( 1 ) 
   # } else{
   #     return( 0 )
   # }

}#


cat('donor1, donor2, status,soundex,jw \n', file="output.csv", sep="", append=FALSE)

# loop through each donor and compare to all others

for( cur.donor.index in 1:length(donor.name.vector) ) {
#for( cur.donor.index in 1:5) {

    #cat('\nDonor "', donor.name.vector[cur.donor.index], '"', sep='' ) 

    # Note: donor.name.vector[-cur.donor.index] is the vector of donor names with the current one excluded
    result <- sapply(donor.name.vector[-cur.donor.index], compare.strings, current.name=donor.name.vector[cur.donor.index], simplify=TRUE )
    #donor.name.vector[cur.donor.index+1:]
  
    #result <- sapply(, compare.strings, current.name=donor.name.vector[cur.donor.index], simplify=TRUE )
    
    # for testing
    #result <- sapply(donor.name.vector[1:110], compare.strings, current.name=donor.name.vector[cur.donor.index], simplify=TRUE )

    # note result is a vector of zeros and ones, where the ones show which names had the same length

    #cat(' there are', sum(result), 'other names of the same length')

}

cat('\n')





