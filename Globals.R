
# Libraries ----------------------------------------------------------------

library(plyr)
library(dplyr)
library(ggvis)
library(googleVis)
library(devtools)
library(RSQLite)
library(data.table)
library(reshape2)
library(ggplot2)

options(strings.as.factors=FALSE)

if (!requireNamespace('htmlwidgets') || packageVersion('htmlwidgets') <= '0.3.2')
 devtools::install_github('ramnathv/htmlwidgets')
# install DT
if (!require("DT")) devtools::install_github("rstudio/DT")
sessionInfo()

# Data Preparation ------------------------------------------------------------------------------------------------------

fileLoc <- "https://raw.github.com/miles2know/DataContainer/master/Ldata.csv"
exData <- read.table(fileLoc,sep = ",",header = TRUE,stringsAsFactors = FALSE)
head(exData)
sapply(exData,class)

regionData <- exData %>% group_by(Scenario,Year) %>% summarise(ghg=sum(value)) 
regionData$ID <- seq_len(nrow(regionData))

guideLine <- data.frame(x=c(2013,2013,2013,2013),y=c(0,50,100,225))

ghgGoal <- read.excel()
goalLine <- data.frame(Year=2001:2030,goal=ghgGoal)
names(goalLine) <- c("year","goal")

# Data -------------------------------------------------------------------------------------------------------------------

proj <- read.excel() %>% melt()
proj[,4] <- sapply(proj[,4],as.character)
proj$Year <- as.numeric(unlist(lapply(strsplit(proj$variable,"X"),"[",2)))

projNghg <- read.excel() %>% melt()
projNghg[,3] <- sapply(projNghg[,3],as.character)
projNghg$Year <- as.numeric(unlist(lapply(strsplit(projNghg$variable,"X"),"[",2)))

hist <- read.excel() %>% melt()
hist[,4] <- sapply(hist[,4],as.character)
hist$Year <- as.numeric(unlist(lapply(strsplit(hist$variable,"X"),"[",2)))

# merge proj with projNghg

projM <- proj %>% group_by(Scenario,Year) %>% summarise(ghg=sum(value))
projNghgM <- projNghg %>% group_by(Year) %>% summarise(ghgN=sum(value))

projF <- left_join(projM,projNghgM,by="Year") %>% mutate(ghgT=ghg+ghgN) %>% select(Scenario,Year,ghgT) %>% 
          arrange(Scenario,Year) %>% ungroup()

projFCheck <- left_join(projM,projNghgM,by="Year") %>% filter(Scenario=="Reference") %>% mutate(ghgT=ghg+ghgN) %>% 
left_join(.,goalLine[-1,],by="Year") %>% ungroup() %>% select(Year,ghg,ghgT,goal) %>% melt(id="Year")

# re-create regionData

histF <- hist %>% group_by(Scenario,Year) %>% mutate(ghgT=sum(value)) %>% select(Scenario,Year,ghgT) %>% 
         arrange(Scenario,Year) %>% ungroup()

regionData <- rbind(projF,histF) %>% arrange(Scenario,Year)
regionData$ID <- seq_len(nrow(regionData))

str(regionData)

allData <- rbind(proj,hist) %>% select(Scenario,State,Sector,Year,value) %>% arrange(Scenario,Year)
allData[1:50,]
names(allData)
sapply(allData,class)

regionData <- allData %>% group_by(Scenario,Year) %>% summarise(ghg=sum(value)) 
regionData$ID <- seq(1:nrow(regionData))
names(regionData)
unique(regionData$Scenario)

regionData[regionData$Scenario=="Reference",]
nrow(regionData)

regionData1 <- allData %>% group_by(Scenario,Year) %>% summarise(ghg=sum(value)) %>% mutate(ID=seq_along(nrow(.)))
nrow(regionData1)

regionData %>% ggvis(~Year,~ghg,stroke=~Scenario) %>% layer_lines(strokeWidth:=2.5,strokeWidth.hover:=5) %>% 
               scale_numeric("y",domain=c(5,125)) %>% add_axis("x",format="d",title="") %>% 
               layer_paths(x = ~x, y = ~y, stroke := "black",strokeWidth := 2.5, opacity := .5, data = guideLine)     

write.table(allData,"C:/Data Container/allData.csv",sep=",",col.names=TRUE)

guideLine <- data.frame(x=c(2013,2013,2013,2013),y=c(0,50,75,125))


# Data Preparation for Wedges ------------------------------------------------------------------------------------------

ghg_db <- dbConnect(SQLite(), dbname="C:/Answerv6/Scenario Development/Project Groups/NEG-ECP Work/R Work/DataAutomation/ghgOut.sqlite")
query <- dbSendQuery(conn = ghg_db,"SELECT * FROM wedge2") 
summaryGhg <- fetch(query,n=-1)
head(summaryGhg)


sGhg <- data.table(summaryGhg)

sGhg <- melt(sGhg, id=1:6)
head(sGhg)
sapply(sGhg,class)

sGhg <- sGhg[, variable:=as.character(variable)]
sGhg <- sGhg[, variable:=as.numeric(variable)]
# interesting
levels(sGhg$variable)
sGhg <- sGhg[, variable:=as.numeric(levels(variable))[variable]]

setnames(sGhg, "variable", "Year")
setnames(sGhg, "value", "Ghg")

#   Res Com Ind Trn Elc         Sector Year      Ghg
#1:   0   0   0   0 0.0    Residential 2013 30.49982
#2:   0   0   0   0 0.0     Commercial 2013 14.30613
#3:   0   0   0   0 0.0     Industrial 2013 11.35580
#4:   0   0   0   0 0.0 Transportation 2013 72.98294
#5:   0   0   0   0 0.0    Electricity 2013 23.49111
#6:   0   0   0   0 0.1    Residential 2013 30.49982

# [1] 90  8
wedgesR <- sGhg[Res == .1 & Com == .1 & Ind == .1 & Trn == .1 & Elc == .1,]

projNghgM

wedgesR[, 6:8, with = FALSE ]

nghgD <- data.frame(Sector = "Non-CO2", projNghgM)
nghgD <- data.table(Sector = "Non-CO2", Year = projNghgM$Year, Ghg = projNghgM$ghgN)

datList <- list(a = wedgesR[, 6:8, with = FALSE ], b = nghgD)

wedgesR1 <- rbindlist(datList)

lines


# wedges plot creation -------------------------------------------------------------------------------------------------

gg <- ggplot(wedgesR1, aes(x = Year, y = Ghg, group = Sector)) 
gg <- gg + stat_smooth(mapping = aes(fill = Sector), geom="area", position="stack", method="gam", formula = y~s(x)) 
gg <- gg + scale_fill_manual(values = colors1) 
gg <- gg + geom_line(data = lines1, aes(x = x, y = y),color = "red", size = 1.5)
gg <- gg + geom_hline(yintercept=127, color = "black", size = 1, linetype = 5 )
gg <- gg + scale_x_continuous(breaks=c(2013,2015,2020,2025,2030))
gg <- gg + scale_y_continuous(breaks=c(14,38,49,61,92,165))

gg <- gg + labs(x=NULL,y="Million Metric Tons", title = "GHG By Sector")
gg <- gg + theme_bw() + theme(legend.position="bottom") + guides(fill=guide_legend(title=NULL))

gg <- gg + theme(plot.title=element_text(face="bold", hjust=0))
gg <- gg + theme(panel.grid=element_blank())
gg <- gg + theme(panel.border=element_blank())

gg

colors <- c("#6EC400", "#5089E0", "#FF9C01", "#FFE78C", "#657383")

colors1 <- c("#6EC400", "#5089E0", "#FF9C01", "#FFE78C", "#657383", "#0000FF")


# how lines was originally created
lines1 <- ggplot_build(gg)$data[[1]] %>% filter(group == 6) %>% select(x,y) 
lines1$Sector <- factor("Total")

yScale <- ggplot_build(gg)$data[[1]] %>% filter(x == 2013) %>% select(y) %>% round(0)

# this is a copy of lines from another environment
lines <- read.excel()


reactiveSvg <- function (outputId)
{
HTML(paste("<div id=\"", outputId, "\" class=\"shiny-network-output\"><svg /></div>", sep=""))
}


# Working with rCharts and other Designs ------------------------------------------------------------------------------


data <- read.delim("http://pmsi-alignalytics.github.io/dimple/data/example_data.tsv",stringsAsFactors=FALSE)
names(data)
sapply(data,class)

# original data is ~ 1000 rows, which is too huge as demo.
set.seed(42)

data <- data[sample(nrow(data), 50), ]
colnames(data) <- gsub("[.]","",colnames(data))

d1 <- dPlot(
UnitSales ~ Month,
groups = "Channel",
data = subset(data, Owner %in% c("Aperture","Black Mesa")),
type = "line", bounds=list(x=100,y=100,height=420,width=330)
)
d1$xAxis(type = "addMeasureAxis")
d1$yAxis(type = "addMeasureAxis")
d1$legend(
x = 200,
y = 10,
width = 500,
height = 20,
horizontalAlign = "right"
)
d1

names(data)
data$Date
data$Month


# https://github.com/PMSI-AlignAlytics/dimple/issues/55

# Functions and Code Snips ------------------------------------------------

read.excel <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,as.is=TRUE,...)
}

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

all_values <- function(x) {
if(is.null(x)) return(NULL)
if(is.null(x$ID)) return(NULL)
 
all_data <- isolate(chooseScen())
data <- all_data[all_data$ID == x$ID, ]
 
paste0("<b>", data$Scenario, "</b><br>", data$Year, "<br>", format(data$ghg,digits=0), collapse = "<br />")

}

all_values1 <- function(x) {
if(is.null(x)) return(NULL)
unique(x[,1])
}

#output$plot2 <- renderChart2({
       
       #iData <- chooseScen()
       #mytooltip <- "#!function(item){return(item.Scenario + '\n' + item.Year + '\n' + item.ghg)}!#"
       #n1 <- rPlot(ghg ~ Year, color = "Scenario", data = iData, type = "line", size=list(const=3),tooltip=mytooltip)
       #n1$layer(x="x",y="y",data=guideLine,type='line',color=list(const='black'),size=list(const=3))
       #n1$set(width = 1200, height = 600)
       #n1$guides(y = list(min = 0, title = "Million Metric Tons"), x = list(min=1990,max=2031,title="",numticks=8))
       #n1$set(dom = 'plot2')
       #return(n1)
#})
 
#output$plot2 <- renderChart({
       
       #iData <- chooseScen()
       #d1 <- dPlot(ghg ~ Year, groups = "Scenario", data = iData, type = "line")
       #d1$xAxis(type = "addCategoryAxis", orderRule = "Date")
       #d1$yAxis(type = "addMeasureAxis")
       #d1$layer(y ~ x,groups = c("x","y"), data = data.frame(x = c(2013,2013,2013,2013), y = c(0,25,50,125)), type="line")
       #d1$legend(x = 200, y = 10, width = 500, height = 20, horizontalAlign = "right")
       #d1$set(dom = 'plot2')
       #return(d1)
#})


sPlot1 <- reactive({
  
     chooseScen %>% ggvis(~Year,~ghg,stroke = ~Scenario) %>% layer_lines(strokeWidth := 2.5,strokeWidth.hover := 5) %>% 
     layer_points(size := 25, size.hover := 100,fillOpacity := 0.1, fillOpacity.hover := 0.25,key := ~ID) %>%
     scale_numeric("y",domain = c(10,225)) %>%
     add_axis("y",format = "d",title = "Million Metric Tons",title_offset = 50,properties = axis_props(labels = list(fontSize = 15))) %>% 
     add_axis("x",format = "d",title = "",properties = axis_props(labels = list(angle = -45,fontSize = 15,align = "right"))) %>% 
     add_tooltip(data_values, "hover") %>% set_options(width = 600,height = 600/1.333, padding = padding(25, 225, 75, 75)) %>%
     layer_paths(x = ~x, y = ~y, stroke := "black",strokeWidth := 2.5, opacity := .5, data = guideLine) %>%
     layer_paths(x = ~Year, y = ~goal, stroke := "red",strokeWidth := 3.5,strokeDash := 5, data = goalLine)
 
#})

sPlot1 <- reactive({
  
     chooseScen %>% ggvis(~Year,~ghgT,stroke = ~Scenario) %>% layer_lines(strokeWidth := 2.5,strokeWidth.hover := 5) %>% 
     scale_numeric("y",domain = c(10,225)) %>%
     add_axis("y",format = "d",title = "Million Metric Tons CO2e",title_offset = 50,properties = axis_props(labels = list(fontSize = 15))) %>% 
     add_axis("x",format = "d",title = "",properties = axis_props(labels = list(angle = -45,fontSize = 15,align = "right"))) %>% 
     add_tooltip(all_values1, "hover") %>% set_options(width = 600,height = 600/1.333, padding = padding(25, 225, 75, 75)) %>%
     layer_paths(x = ~x, y = ~y, stroke := "black",strokeWidth := 2.5, opacity := .5, data = guideLine) %>%
     layer_paths(x = ~Year, y = ~goal, stroke := "red",strokeWidth := 3.5,strokeDash := 5, data = goalLine)
 
})

all_values1 <- function(x) {
if(is.null(x)) return(NULL)
unique(x[,1])
}

##### Constructing chart for slidify presentation
 
head(regionData)
unique(regionData$Scenario)
class(regionData)
regionData
 
regionDataA <- regionData %>% select(Scenario,Year,ghgT) %>% filter(Scenario %in% c('Reference','Low Economic Growth','High Economic Growth')) %>%
  mutate(ID=1:nrow(.))


## Pie chart data for 2030
 
projMP <- proj %>% group_by(Scenario,Year,Sector) %>% summarise(ghg=sum(value)) %>% filter(Scenario=="Reference",Year==2030)
projNghgMP <- projNghg %>% group_by(Year,Sector) %>% summarise(ghgN=sum(value)) %>% filter(Year==2030)

write.table(projNghgMP,"C:/Answerv6/Scenario Development/Project Groups/NEG-ECP Work/Researchs Documents/proj2030NonFF.csv",sep=",")
 
 
 
 
# Experimenting with streamgraphs
 
ggplot2::movies %>%
  select(year, Action, Animation, Comedy, Drama, Documentary, Romance, Short) %>%
  tidyr::gather(genre, value, -year) %>%
  group_by(year, genre) %>%
  tally(wt=value) %>%
  streamgraph("genre", "n", "year") %>%
  sg_axis_x(20) %>%
  sg_colors("PuOr") %>%
  sg_legend(show=TRUE, label="Genres: ") 
 
 
 
 
data_values1 <- function(x) {
 
     if(is.null(x)) return(NULL)
     if(is.null(x$ID)) return(NULL)
 
     all_data <- isolate(x)
     data <- all_data[all_data$ID == x$ID,]
 
     paste0("<b>", data$Scenario, "</b><br>", data$Year, "<br>", 
     format(data$ghgT,digits=0), collapse = "<br />")

} 

regionDataB %>% ggvis(~Year,~ghgT,stroke = ~Scenario) %>% layer_lines(strokeWidth := 4,strokeWidth.hover := 6) %>% 
     layer_points(size := 25, size.hover := 100,fillOpacity := 0.1, fillOpacity.hover := 0.25,key := ~ID) %>%
     layer_points(x = ~x, y = ~y, size := 100,stroke := "blue", fillOpacity := 0.1, data = refPoint) %>%
     add_legend(scales="stroke",properties = legend_props(title = list(fontSize = 18),labels = list(fontSize = 14))) %>%
     add_tooltip(data_values1, "hover") %>% scale_numeric("y",domain = c(10,225)) %>%
     add_axis("y",format = "d",title = "Million Metric Tons CO2e",title_offset = 50,properties = axis_props(labels = list(fontSize = 15),title = list(fontSize = 14))) %>% 
     add_axis("x",format = "d",title = "",properties = axis_props(labels = list(angle = -45,fontSize = 15,align = "right"))) %>% 
     layer_paths(x = ~x, y = ~y, stroke := "black",strokeWidth := 2.5, opacity := .5, data = guideLine) %>%
     layer_paths(x = ~year, y = ~goal, stroke := "red",strokeWidth := 2.5,strokeDash := 2, data = goalLine1) %>%
     layer_paths(x = ~year, y = ~goal, stroke := "red",strokeWidth := 2.5,strokeDash := 2, data = goalLine2) %>%
     layer_paths(x = ~year, y = ~goal, stroke := "red",strokeWidth := 2.5,strokeDash := 2, data = goalLine3)
 
 
goalLine1 <- data.frame(year=1990:2010,goal=rep(191.45,(2010-1990)+1))
goalLine2 <- data.frame(year=2010:2020,goal=rep(172.31,(2020-2010)+1))
goalLine3 <- data.frame(year=2020:2050,goal=rep(38.28,(2050-2020)+1)) 
refPoint <- data.frame(x=2030,y=127)
 

regionDataB <- regionDataA 
 
regionDataB$Scenario <- revalue(regionDataB$Scenario,c("High Growth" = "Highest GHG Scenario","Low Growth" = "Lowest GHG Scenario")) 
regionDataB <- regionDataB %>% rename('AEO Scenario' = Scenario) 
 

 
 
  
write.table(projFCheck,"C:/Answerv6/Scenario Development/Project Groups/NEG-ECP Work/Researchs Documents/refData.csv",sep=",")
write.table(regionDataA,"C:/Users/jrudokas/Desktop/Notes - Data Experiments/Stat Learning/SlidifyPresentations/Learning/Deck1/assets/files/refData.csv",sep=",") 
write.table(guideLine,"C:/Users/jrudokas/Desktop/Notes - Data Experiments/Stat Learning/SlidifyPresentations/Learning/Deck1/assets/files/guide.csv",sep=",")
write.table(goalLine,"C:/Users/jrudokas/Desktop/Notes - Data Experiments/Stat Learning/SlidifyPresentations/Learning/Deck1/assets/files/goal.csv",sep=",")

 
 
regionData %>% filter(Scenario == "Reference")
 
 


#### Trying googleVis

exData1 <- exData %>% group_by(Scenario,Year) %>% summarise(ghg=sum(value))

Line4 <- gvisLineChart(exData1, xvar="Year", yvar="ghg")

plot(Line4)


 
# http://stackoverflow.com/questions/23386012/add-line-to-rchart-dimple-scatter-plot
 

# trying different ID strategies

regionData <- exData %>% group_by(Scenario,Year) %>% summarise(ghg=sum(value)) 
regionData$ID <- seq_len(nrow(regionData))

groupData <- exData %>% group_by(Scenario,Year) %>% summarise(ghg=sum(value)) %>% mutate(group=row_number())

groupData[1:50,]



### Comparing reference with and with out Non-CO2 FF and AG and Waste

projFCheck <- left_join(projM,projNghgM,by="Year") %>% filter(Scenario=="Reference") %>% mutate(ghgT=ghg+ghgN) %>% 
left_join(.,goalLine[-1,],by="Year") %>% ungroup() %>% select(Year,ghg,ghgT,goal) %>% melt(id="Year")


p1 <- ggplot(projFCheck,aes(x=Year,y=value,colour=variable)) + geom_line(size=1.1) + theme_bw() +
theme(legend.position=c(.5,.2),legend.title = element_text(size=16),legend.text = element_text(size=14),axis.title.x = element_blank(),axis.text.x = element_text(angle=45, vjust=.5, hjust=.6, size=14),axis.title.y = element_text(size=14),axis.text.y = element_text(size=14)) + 
scale_y_continuous(name="MMT CO2e") + expand_limits(y = c(0,225))                      
p1 + scale_colour_manual(values = c("blue","black","red"),name = "CO2e Trend",labels=c("AEO Reference: CO2 From FFC","AEO Reference: All GHGs","New England 2030 Goal Line")) 


sapply(0:10,function(x) 100*(1-.001)^x)


# really neat assignment answer from SO
 
 df1$new <-  c('green', 'red')[(df1$sample=='a')+1L]
 df1
 
 

# grabing reference case data
 
regionData %>% filter(Scenario == "Reference") %>% distinct(ghgT) %>% write.excel()
 
 
#### aggregating egrid data
 
options(stringsAsFactors = FALSE) 
expMap <- data.frame(v1 = sample(letters[1:10],20, replace = TRUE))
 
mapvalues(expMap$v1, from = "a", to = "AAA") 
 
#### ggplot ref line + nghg
 
projNghgM[1,2] + lines[1,2]



