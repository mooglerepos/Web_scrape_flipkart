library(RSelenium)
library(basictabler)
library(openxlsx)
library(shinydashboard)
library(shiny)

tbl <- BasicTable$new()

driver <- rsDriver(browser=c("chrome"), chromever="98.0.4758.102",port = 4444L)
remote <- driver[["client"]]

remote$open()
remote$navigate("https://www.flipkart.com")
Sys.sleep(5)
remote$click(1)

print("which product wanna scrape for:")
nm <- readline()

inp <- remote$findElement(using = 'css', "[class='_3704LK']")
inp$sendKeysToElement(list(nm, "\uE007"))

tit <- remote$findElements(using="css selector" ,"a.s1Q9rs")
tit_li <- unlist(lapply(tit, function(x){x$getElementText()}))
tit_li

pri <- remote$findElements(using= "css selector","div._30jeq3")
pri_li <- unlist(lapply(pri,function(x){x$getElementText()}))
fi_li <- substr(pri_li,2,nchar(pri_li))

lil <- remote$findElements(using = "css selector","a._2rpwqI")
lil_li <- unlist(lapply(rat, function(x){x$getElementAttribute("href")}))
lil_li

rat <- remote$findElements(using = "css selector","div._3LWZlK")
rat_li <- unlist(lapply(cur, function(x){x$getElementText()}))
rat_li


tbl$addData(data.frame(tit_li[1:40],fi_li[1:40],rat_li[1:40],lil_li[1:40]), 
            firstColumnAsRowHeaders=TRUE,
            explicitColumnHeaders=c(nm, "INR", "Rating","details"),
            columnFormats=list(NULL, NULL, NULL,NULL))

out <- tbl$renderTable(styleNamePrefix="t10")
out

wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
addWorksheet(wb, "Data")
tbl$writeToExcelWorksheet(wb=wb, wsName="Data", 
                          topRowNumber=1, leftMostColumnNumber=1,applyStyles=TRUE, mapStylesFromCSS=TRUE)

saveWorkbook(wb, file="E:\\COM\\R\\scrape\\scrape_file.xlsx", overwrite = TRUE)

img <- remote$findElement(using = "css selector","img._396cs4")
ima <- img$getElementAttribute("src")
print(ima)
remote$navigate(ima)

for (i in gig){
  if (i < "50,000"){
    print(i)}
}

fi_li
typeof(fi_li1)
fi_li1<- gsub(",","",fi_li)
fi_li1 <- as.numeric(fi_li1)
typeof(rat_li1)


ui <- dashboardPage(
  dashboardHeader(title="scrape data"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(box(title="rating",background = "maroon",sloidHeader=TRUE,collapsible = TRUE,plotOutput("plot1",height=250)),
             box(title="price",background = "maroon",solidHeader = TRUE,plotOutput("plot2",height = 250))),
    fluidRow(box(title = "histogram about",width =NULL,solidHeader = TRUE,status = "primary",background = "aqua",
                 "above two garah about which rating is purchased high and which price is purchased high"),
             box(title = "link for filter products",width = NULL,solidHeader = TRUE,status = "primary",tt))
    ))

server <- function(input, output) {
  output$plot1 <- renderPlot({ hist(rat_li1,col = "lightblue")})
  output$plot2 <- renderPlot({plot(fi_li1,type = "o",col = "red",pch=18)})
  #lines(ref_lin,type = "o",col='blue')
  }

shinyApp(ui, server)

remote$close()

out1 <- data.frame(tit=tit_li[1:40],pri=fi_li1[1:40],rat=rat_li1[1:40],lin=lil_li[1:40])
out1$pri > 50000
tt<- out1[out1$pri < 50000 & out1$rat > 4.5 ,c("lin")]
length(tt)
tt


