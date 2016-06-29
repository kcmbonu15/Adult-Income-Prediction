
setwd("C:/Users/kaelo/Downloads/Kingsley")
##################################################################################

library(XML)
library(RCurl)
library(rjson)
library(httr)
library(RJSONIO)
library(XML2R)
library(jpeg)

##################################################################################

xmlfile=c(1:5)
review <- c("Fiction superhero ",)


xmlfile[1] <- "http://www.omdbapi.com/?t=cyborg&r=XML&tomatoes=TRUE"
xmlfile[2] <- "http://www.omdbapi.com/?t=Hero&r=XML&tomatoes=TRUE"
xmlfile[3] <- "http://www.omdbapi.com/?t=Kiss of the dragon&r=XML&tomatoes=TRUE"
xmlfile[4] <- "http://www.omdbapi.com/?t=Fearless&r=XML&tomatoes=TRUE"
xmlfile[5] <- "http://www.omdbapi.com/?t=unleashed&r=XML&tomatoes=TRUE"



##################################################################################

library(rtf)
file <- RTF("C:/Users/kaelo/Downloads/Kingsley")
addHeader(file,title="Mymovie Review",font.size=24)
addParagraph(file,"\nMy Rexam\n")
addParagraph(file,"Kingsley Mbonu\n")
addParagraph(file,paste("\nCreated On",Sys.Date()))

##################################################################################

for(i in 1:5){
        addPageBreak(file)
        xml.url <- xmlTreeParse(xmlfile[i])
        xml.url2 <- docsToNodes(xml.url)
        mymovies <- mydat$doc$children$root$children$movie$attributes
        addPng(file,mymovies[[14]],height=2,width=2)
        addParagraph(file,"Title:",mymovies[[1]])
        addNewLine(file)
        addParagraph(file,"Released Date:",mymovies[[4]])
        addNewLine(file)
        addParagraph(file,"Director:",mymovies[[7]])
        addNewLine(file)
        addParagraph(file,"Actors:",mymovies[[9]])
        addNewLine(file)
        addParagraph(file,"Genre:",mymovies[[6]])
        addNewLine(file)
        addParagraph(file,"Tomatoes Rating:",mymovies[[22]])
        addNewLine(file)
        addParagraph(file,"Tomato Consensus:",mymovies[[26]])
        addNewLine(file)
        addParagraph(file,"Plot:",mymovies[[10]])
        addNewLine(file)
        addParagraph(file,"Box Office dollars:",mymovies[[31]])
        addNewLine(file)
        addParagraph(file,"Review:",review[i])
        addNewLine(file)
}

done(file)
