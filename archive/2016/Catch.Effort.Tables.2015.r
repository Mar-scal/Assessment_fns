#source("Y:\\Fishery data\\r\\fn\\Catch.Effort.Tables.R")

#Catch.Effort.Tables()

# Agruments: 
#
# year: numerical four digit year
#
# bank: "Georges a" = Georges Bank (a),"Georges b" = Georges Bank (b), "German" = German Bank, "Sable" = Sable/Western Bank, "Middle" = Middle Bank, "SPB" = St. Pierre Bank, "Banquereau" = Banquereau Bank, "Browns North" = Browns Bank north, "Browns South" = Browns Bank south
#
# fleet: ALL = total fleet, FT = freezer trawlers, WF = wet fishery		
#
# boxes: T = data is summerized for each management box 
#
# get.data: T = gets a data dump from marfis

Catch.Effort.Tables <- function(year = 2015, bank = "Georges a", fleet = "ALL", boxes=F, get.data='dump',print=F,output=T,nafo.div=c("5ZEJ","5ZEM","5ZEU"),export="F")
 
{
	
	source("Y:\\Fishery data\\r\\fn\\get.marfis.logs.2015.r")
	source("Y:\\Fishery data\\r\\fn\\FishMonth.2015.r")

	
	# Browns & Georges Bank management boxes
	BBntrap2008 <- read.table("Y:\\Fishery data\\boxes\\2008BBntrap.txt")
	HVclosure2008 <- read.table("Y:\\Fishery data\\boxes\\2008HVclosure.txt")
	SWEHVclosure2008 <- read.table("Y:\\Fishery data\\boxes\\2008SWEHVclosure.txt")
	eastofhappyvalley <- read.table("Y:\\Fishery data\\boxes\\eastofhappyvalley.txt")
	Happyvalley <- read.table("Y:\\Fishery data\\boxes\\Happyvalley.txt")
	HVseedbox1 <- read.table("Y:\\Fishery data\\boxes\\HVseedbox1.txt")
	seedbox08GB1 <- read.table("Y:\\Fishery data\\boxes\\seedbox08GB1.txt")
	seedbox08GB2 <- read.table("Y:\\Fishery data\\boxes\\seedbox08GB2.txt")
	SeedboxGb2005 <- read.table("Y:\\Fishery data\\boxes\\SeedboxGb2005.txt")
	SWedge <- read.table("Y:\\Fishery data\\boxes\\SWedge.txt")
	
	BBn.boxes <<- list(BBntrap2008=BBntrap2008,HVclosure2008=HVclosure2008,SWEHVclosure2008=SWEHVclosure2008,eastofhappyvalley=eastofhappyvalley,Happyvalley=Happyvalley,HVseedbox1=HVseedbox1,SWedge=SWedge)
	
	GB.boxes <<- list(GBseedbox08_1=seedbox08GB1,GBseedbox08_2=seedbox08GB2,GBseedbox05=SeedboxGb2005)
	
	FishMonth(year=year, bank = bank, fleet = fleet, boxes=boxes, get.data=get.data,print=print,output=output,nafo.div=nafo.div,export=export)
		
}


