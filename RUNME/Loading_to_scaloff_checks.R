#### PRE-LOADING OFFSHORE SCALLOP SURVEY DATA CHECKS

#### set up directory, year, cruise info...

direct_fns <- "C:/Users/keyserf/Documents/Github/Assessment_fns/"
year <- 2023
cruise <- "LE17"

#### First, check the data file-by-file (by bank) using scaloff_bank_check.R
### Step 1: Load the scaloff_bank_check function
source(paste0(direct_fns, "Survey_and_OSAC/scaloff_bank_check.R"))

direct <- "Y:/Offshore/Assessment/"

# Make sure CSV files are saved as CSV (Comma Delimited) (*.csv) (NOT UTF-8)

### Step 2: Run the function with the various arguments you need:
scaloff_bank_check(direct = direct,  direct_fns=direct_fns, year = year, type = "csv", 
                   spatialplot = T, assign.strata=T, # NOTE: spatialplot must be T for assign.strata to work
                   cruise = cruise, bank = "GBb", survey_name = "GBb2023", 
                   tow=T, hf = T, mwsh = TRUE, un=un.ID, pwd.ID=pw, # if hf is T, tow must also be T
                   nickname = ".GBbLE18")

### Step 3: Scroll through the output and read the error messages to look at issues. Be thorough!
### For GB monitoring survey (spring), warnings like "the condition has length > 1 and only the first element will be used" are ok
### "Less than 4 coordinates in polygon" warning is fine. 

### Step 4: Look at the spatial plots in the PDF saved here: 
### Y:\Offshore scallop\Assessment\Data\Survey_data\YEAR\Database loading\CRUISE\BANK\spatial_checks.pdf
### Make sure all the tows are within the polygon for the right bank. Look out for "flagged" tows in red/pink, 
### which are flagged for having the start or end point outside the polygon. Check coordinates for flagged tows.

### Step 5: Look at the HF data plots in the PDF saved here:
### Y:\Offshore scallop\Assessment\Data\Survey_data\2019\Database loading\CRUISE\BANK\HF_distribution_checks.pdf
### Make sure that bar heights are reasonable, and that small sizes are in buckets/big sizes in baskets. 
### Make sure live and dead weren't somehow switched (should pretty much always have more live than dead for a tow)

### Step 6: Look at the MWSH plots in the PDF saved here:
### Y:\Offshore scallop\Assessment\Data\Survey_data\2019\Database loading\CRUISE\BANK\MWSH_checks.pdf
### Double check any records that look like major outliers

### Step 7: Once you've checked/fixed the files for each individual bank, use scaloff_cruise_check function to check data BETWEEN banks.
### Start by setting up some variables and loading in the function
year <- 2023
cruise <- "LE18"

direct_fns <- "./"
source(paste0(direct_fns, "Survey_and_OSAC/scaloff_cruise_check.R"))

direct <- "Y:/Offshore/Assessment/"

### Step 8: Run the function with the various arguments you need:
scaloff_cruise_check(year=year, direct=direct, direct_fns=direct_fns,
                     type="csv", 
                     cruise=c("LE18"), season="summer")

### Step 9: Scroll through the output and make sure it's all logical. 
### The last line should read: Successfully passed duplicate tow check without any issues. Yay!
### If it doesn't the duplicate tow check did not occur as expected. Look inside scalloff_cruise_check.R


