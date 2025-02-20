rm(list = ls()) #clears the environment

#Set your working directory
setwd("C:/your_directory")

#install.packages("readxl")  # Only needed if not installed
library(readxl)


# Load your data into R - Specify the full correct path
data <- read_excel("your_data.xlsx", 
                   sheet = "Export 1.1")

# Check if data is loaded correctly
head(data)

###Dropping the tester observations from the survey
# Check the frequency of 'tester' values 
table(data$tester)
# Drop rows where 'tester' is 1
data <- data[data$tester != 1, ]
# Verify if the rows were removed
table(data$tester)
prop.table(table(data$tester))  # Shows proportion of each category

###The paths of two participants in the survey were ambiguous; therefore, we code them to exclude later.
# Replace dispcode with 88 where lfdn is 1717 or 1824
data$dispcode[data$lfdn == 1717 | data$lfdn == 1824] <- 88
data[data$lfdn %in% c(1717, 1824), ]  # Show rows where lfdn is 1717 or 1824

###Excluding those who did not completed the survey
# 1. Check distribution of dispcode
table(data$dispcode)
# 2. Assign variable label (R doesn't natively support variable labels, but we can use the 'labelled' package)
#install.packages("labelled")  # Run only if the package is not installed
library(labelled)
var_label(data$dispcode) <- "Disposition code of the Survey"
# 3. Define labels for dispcode values
dispcode_labels <- c(
  "12" = "Invited",
  "14" = "E-mail could not be delivered",
  "15" = "Cannot be reached",
  "18" = "Custom unavailable",
  "88" = "Ambiguous Path",
  "22" = "Suspended",
  "31" = "Completed",
  "32" = "Completed after break",
  "37" = "Screen out-Informed consent No",
  "38" = "Screen out-No answer, Not known GEP"
)

# Convert dispcode to a factor with labels
data$dispcode <- factor(data$dispcode, levels = names(dispcode_labels), labels = dispcode_labels)
# 4. Check updated dispcode values
table(data$dispcode)
# 5. Keep only observations where dispcode is 31 (Completed) or 32 (Completed after break)
data <- data[data$dispcode %in% c("Completed", "Completed after break"), ]
# 6. Verify that only the required observations remain
table(data$dispcode)

###Dropping not necesarry variables
# Drop specific columns by name
data <- data[, !(names(data) %in% c("external_lfdn", "tester", "quality"))]
# Drop all columns that start with "rts"
data <- data[, !grepl("^rts", names(data))]
# Check remaining column names
colnames(data)


###Coding selected languages in the survey
# 1. Rename variable v_592 to Language
colnames(data)[colnames(data) == "v_592"] <- "Language"
# 2. Assign a label to the variable (using the labelled package)
library(labelled)
var_label(data$Language) <- "1.0 Language of the survey"
# 3. Define language labels
language_labels <- c(
  "1" = "English",
  "2" = "German",
  "4" = "Spanish",
  "5" = "French",
  "6" = "Polish"
)

# 4. Convert Language variable to a factor with labels
data$Language <- factor(data$Language, levels = names(language_labels), labels = language_labels)
# 5. Add a note (R doesn't have a direct equivalent for `notes`, but we can store metadata)
attr(data$Language, "note") <- "Selected Language of the Survey"
# 6. Tabulate Language (equivalent to `tab Language` in Stata)
table(data$Language)

###Coding Informed consent and keeping those gave their consent to participate the survey
# 1. Rename variable v_594 to Consent
colnames(data)[colnames(data) == "v_594"] <- "Consent"
# 2. Assign a variable label (R does not have built-in labels like Stata)
library(labelled)
var_label(data$Consent) <- "1.1 Informed consent"
# 3. Define labels for Consent values
consent_labels <- c(
  "1" = "Yes",
  "2" = "No"
)
# 4. Convert Consent variable to a factor with labels
data$Consent <- factor(data$Consent, levels = names(consent_labels), labels = consent_labels)
# 5. Keep only respondents who gave consent (Consent == "Yes")
data <- data[data$Consent == "Yes", ]
# 6. Add a note (R doesn’t have `notes`, but we can store metadata)
attr(data$Consent, "note") <- "Informed Consent"
# 7. Check distribution of Consent
table(data$Consent)


###Recoding countries
# 1. Rename variable v_17 to Country
colnames(data)[colnames(data) == "v_17"] <- "Country"
# 2. Assign a variable label (R does not have built-in labels like Stata)
var_label(data$Country) <- "2.1 Country Code"
# 3. Define labels for Country values
country_labels <- c(
  "1" = "Austria", "2" = "Belgium", "3" = "Bulgaria", "4" = "Switzerland",
  "5" = "Cyprus", "6" = "Czechia", "7" = "Denmark", "8" = "Germany",
  "9" = "Estonia", "10" = "Greece", "11" = "Spain", "12" = "Finland",
  "13" = "France", "14" = "Croatia", "15" = "Ireland", "16" = "Italy",
  "17" = "Lithuania", "18" = "Malta", "19" = "Netherlands", "20" = "Norway",
  "21" = "Poland", "22" = "Portugal", "23" = "Sweden", "24" = "Slovenia",
  "25" = "Slovakia", "26" = "Israel", "27" = "Latvia", "28" = "Luxembourg",
  "29" = "Hungary", "30" = "Romania", "31" = "United Kingdom", "32" = "Serbia",
  "33" = "Bosnia and Herzegovina"
)
# 4. Convert Country variable to a factor with labels
data$Country <- factor(data$Country, levels = names(country_labels), labels = country_labels)
# 5. Add a note (R doesn’t have `notes`, but we can store metadata)
attr(data$Country, "note") <- "Country of Organisation"
# 6. Tabulate Country (equivalent to `tab Country` in Stata)
table(data$Country)

###Recoding Organization types
# 1. Rename variable v_18 to Organisation_type
colnames(data)[colnames(data) == "v_18"] <- "Organisation_type"
var_label(data$Organisation_type) <- "2.2 Type of Organisation"
# 2. Define labels for Organisation_type values
organisation_type_labels <- c(
  "1" = "Higher Education Institution",
  "2" = "Research organisation",
  "3" = "Private Company",
  "4" = "Research Funding Organisation"
)
# 3. Convert Organisation_type variable to a factor with labels
data$Organisation_type <- factor(data$Organisation_type, 
                                 levels = names(organisation_type_labels), 
                                 labels = organisation_type_labels)
# 4. Add a note (R doesn’t have `notes`, but we can store metadata)
attr(data$Organisation_type, "note") <- "Type of Organisation"
# 5. Tabulate Organisation_type (equivalent to `tab Organisation_type` in Stata)
table(data$Organisation_type)

###Re-coding Organization size
# 1. Rename variable v_19 to Organisation_size
colnames(data)[colnames(data) == "v_19"] <- "Organisation_size"
var_label(data$Organisation_size) <- "2.3 Size of Organisation"
# 3. Define labels for Organisation_size values
organisation_size_labels <- c(
  "1" = "Less than 50",
  "2" = "51 - 250",
  "3" = "251 - 500",
  "4" = "501 - 1000",
  "5" = "1001 - 5000",
  "6" = "More than 5.000"
)
# 4. Convert Organisation_size variable to a factor with labels
data$Organisation_size <- factor(data$Organisation_size, 
                                 levels = names(organisation_size_labels), 
                                 labels = organisation_size_labels)
# 5. Add a note (R doesn’t have `notes`, but we can store metadata)
attr(data$Organisation_size, "note") <- "2.3 Size of Organisation"
# 6. Tabulate Organisation_size (equivalent to `tab Organisation_size` in Stata)
table(data$Organisation_size)

###Re-coding Organization names
# 1. Rename variable v_595 to Organisation_name_published
colnames(data)[colnames(data) == "v_595"] <- "Organisation_name_published"
var_label(data$Organisation_name_published) <- "2.4 Name of Organisation"
# 3. Define labels for Organisation_name_published values
organisation_name_labels <- c(
  "0" = "No answer",
  "1" = "I don't want to share the name",
  "2" = "I share the name of the organisation"
)
# 4. Convert Organisation_name_published variable to a factor with labels
data$Organisation_name_published <- factor(data$Organisation_name_published, 
                                           levels = names(organisation_name_labels), 
                                           labels = organisation_name_labels)
# 5. Tabulate Organisation_name_published (equivalent to `tab Organisation_name_published` in Stata)
table(data$Organisation_name_published)

###Re-coding Organization names
# 1. Rename variable v_596 to Organisation_name
colnames(data)[colnames(data) == "v_596"] <- "Organisation_name"
var_label(data$Organisation_name) <- "2.4 Organisation name"
# 2. Replace "-99" with "Not edited"
data$Organisation_name[data$Organisation_name == "-99"] <- "Not edited"
# 3. Add a note (R doesn’t have `notes`, but we can store metadata)
attr(data$Organisation_name, "note") <- "Name of Organisation"
# 4. Tabulate Organisation_name (equivalent to `tab Organisation_name` in Stata)
table(data$Organisation_name)

###Re-Coding variable Gender Equality Plan
# 1. Rename variable v_34 to GEP
colnames(data)[colnames(data) == "v_34"] <- "GEP"
var_label(data$GEP) <- "3.1 Gender Equality Plan (focusing primarily on gender equality)"
# 2. Define labels for GEP values
GEP_labels <- c(
  "0" = "No",
  "1" = "Yes"
)
# 3. Convert GEP variable to a factor with labels
data$GEP <- factor(data$GEP, 
                   levels = names(GEP_labels), 
                   labels = GEP_labels)
# 4. Add a note (R doesn’t have `notes`, but we can store metadata)
attr(data$GEP, "note") <- "Gender Equality Plan (focusing primarily on gender equality)"
# 5. Tabulate GEP (equivalent to `tab GEP` in Stata)
table(data$GEP)

###Re-coding Gender equality and Diversity plan
# 1. Rename variable v_35 to GEDP
colnames(data)[colnames(data) == "v_35"] <- "GEDP"
var_label(data$GEDP) <- "3.1 Gender equality and Diversity plan (including several inequalities but focusing on gender)"
# 2. Define labels for GEDP values
GEDP_labels <- c(
  "0" = "No",
  "1" = "Yes"
)
# 3. Convert GEDP variable to a factor with labels
data$GEDP <- factor(data$GEDP, 
                    levels = names(GEDP_labels), 
                    labels = GEDP_labels)
# 4. Add a note (R doesn’t have `notes`, but we can store metadata)
attr(data$GEDP, "note") <- "Gender equality and Diversity plan (including several inequalities but focusing on gender)"
# 5. Tabulate GEDP (equivalent to `tab GEDP` in Stata)
table(data$GEDP)


### Re-coding Diversity, equity, or inclusion plan (DEIP)
# 1. Rename variable v_36 to DEIP
colnames(data)[colnames(data) == "v_36"] <- "DEIP"
# 2. Add a variable label
var_label(data$DEIP) <- "3.1 Diversity, equity or inclusion plan (dealing with several inequalities without highlighting one)"
# 3. Define labels for DEIP values
DEIP_labels <- c(
  "0" = "No",
  "1" = "Yes"
)
# 4. Convert DEIP variable to a factor with labels
data$DEIP <- factor(data$DEIP, 
                    levels = names(DEIP_labels), 
                    labels = DEIP_labels)
# 5. Add a note (metadata)
attr(data$DEIP, "note") <- "Gender equality and Diversity plan (including several inequalities but focusing on gender)"
# 6. Tabulate DEIP (equivalent to `tab DEIP` in Stata)
table(data$DEIP)

### Re-coding No_GEP_GEDP_DEIP - No Gender Equality Plan or another institutional strategy
# 1. Rename variable v_37 to No_GEP_GEDP_DEIP
colnames(data)[colnames(data) == "v_37"] <- "No_GEP_GEDP_DEIP"
# 2. Add a variable label
var_label(data$No_GEP_GEDP_DEIP) <- "3.1 No Gender Equality Plan or another institutional strategy"
# 3. Define labels for No_GEP_GEDP_DEIP values
No_GEP_GEDP_DEIP_labels <- c(
  "0" = "No",
  "1" = "Yes"
)
# 4. Convert No_GEP_GEDP_DEIP variable to a factor with labels
data$No_GEP_GEDP_DEIP <- factor(data$No_GEP_GEDP_DEIP, 
                                levels = names(No_GEP_GEDP_DEIP_labels), 
                                labels = No_GEP_GEDP_DEIP_labels)
# 5. Add a note (metadata)
attr(data$No_GEP_GEDP_DEIP, "note") <- "No Gender Equality Plan or another institutional strategy"
# 6. Tabulate No_GEP_GEDP_DEIP (equivalent to `tab No_GEP_GEDP_DEIP` in Stata)
table(data$No_GEP_GEDP_DEIP)

### Re-coding Not_known_GEP_GEDP_DEIP - Gender Equality Plan or another institutional strategy
# 1. Rename variable v_38 to Not_known_GEP_GEDP_DEIP
colnames(data)[colnames(data) == "v_38"] <- "Not_known_GEP_GEDP_DEIP"
# 2. Add a variable label
var_label(data$Not_known_GEP_GEDP_DEIP) <- "3.1 Not known Gender Equality Plan or another institutional strategy"
# 3. Define labels for Not_known_GEP_GEDP_DEIP values
Not_known_GEP_GEDP_DEIP_labels <- c(
  "0" = "No",
  "1" = "Yes"
)
# 4. Convert Not_known_GEP_GEDP_DEIP variable to a factor with labels
data$Not_known_GEP_GEDP_DEIP <- factor(data$Not_known_GEP_GEDP_DEIP, 
                                       levels = names(Not_known_GEP_GEDP_DEIP_labels), 
                                       labels = Not_known_GEP_GEDP_DEIP_labels)
# 5. Add a note (metadata)
attr(data$Not_known_GEP_GEDP_DEIP, "note") <- "Not known Gender Equality Plan or another institutional strategy"
# 6. Tabulate Not_known_GEP_GEDP_DEIP (equivalent to `tab Not_known_GEP_GEDP_DEIP` in Stata)
table(data$Not_known_GEP_GEDP_DEIP)


### Re-coding No_answer_GEP_GEDP_DEIP - Gender Equality Plan or another institutional strategy
# 1. Rename variable v_39 to No_answer_GEP_GEDP_DEIP
colnames(data)[colnames(data) == "v_39"] <- "No_answer_GEP_GEDP_DEIP"
# 2. Add a variable label
var_label(data$No_answer_GEP_GEDP_DEIP) <- "3.1 No answer Gender Equality Plan or another institutional strategy"
# 3. Define labels for No_answer_GEP_GEDP_DEIP values
No_answer_GEP_GEDP_DEIP_labels <- c(
  "0" = "No",
  "1" = "Yes"
)
# 4. Convert No_answer_GEP_GEDP_DEIP variable to a factor with labels
data$No_answer_GEP_GEDP_DEIP <- factor(data$No_answer_GEP_GEDP_DEIP, 
                                       levels = names(No_answer_GEP_GEDP_DEIP_labels), 
                                       labels = No_answer_GEP_GEDP_DEIP_labels)
# 5. Add a note (metadata)
attr(data$No_answer_GEP_GEDP_DEIP, "note") <- "No answer Gender Equality Plan or another institutional strategy"
# 6. Tabulate No_answer_GEP_GEDP_DEIP (equivalent to `tab No_answer_GEP_GEDP_DEIP` in Stata)
table(data$No_answer_GEP_GEDP_DEIP)


### Re-coding EDP - Equity and Diversity Plan
# 1. Rename variable v_45 to EDP
colnames(data)[colnames(data) == "v_45"] <- "EDP"
# 2. Add a variable label
var_label(data$EDP) <- "3.2 Does the equity or diversity plan integrate targets and measures to foster gender equality?"
# 3. Define labels for EDP values
EDP_labels <- c(
  "1" = "Yes",
  "2" = "No",
  "3" = "Not known",
  "4" = "No answer",
  "-77" = "Not seen the question"
)
# 4. Convert EDP variable to a factor with labels
data$EDP <- factor(data$EDP, 
                   levels = names(EDP_labels), 
                   labels = EDP_labels)
# 5. Add a note (metadata)
attr(data$EDP, "note") <- "Does the equity or diversity plan integrate targets and measures to foster gender equality?"
# 6. Tabulate EDP (equivalent to `tab EDP` in Stata)
table(data$EDP)

### Re-coding Nat_lang - GEP in National language/s
# 1. Rename variable v_320 to Nat_lang
colnames(data)[colnames(data) == "v_320"] <- "Nat_lang"
# 2. Add a variable label
var_label(data$Nat_lang) <- "3.3 GEP in National language/s"
# 3. Define labels for Nat_lang values
Nat_lang_labels <- c(
  "0" = "No",
  "1" = "Yes",
  "-77" = "Not seen the question"
)
# 4. Convert Nat_lang variable to a factor with labels
data$Nat_lang <- factor(data$Nat_lang, 
                        levels = names(Nat_lang_labels), 
                        labels = Nat_lang_labels)
# 5. Add a note (metadata)
attr(data$Nat_lang, "note") <- "Is the Gender Equality Plan available in National language/s in your organisation?"
# 6. Tabulate Nat_lang (equivalent to `tab Nat_lang` in Stata)
table(data$Nat_lang)

### Re-coding Eng_lang - GEP in English

# 1. Rename variable v_321 to Eng_lang
colnames(data)[colnames(data) == "v_321"] <- "Eng_lang"
# 2. Add a variable label
var_label(data$Eng_lang) <- "3.3 GEP in English"
# 3. Define labels for Eng_lang values
Eng_lang_labels <- c(
  "0" = "No",
  "1" = "Yes",
  "-77" = "Not seen the question"
)
# 4. Convert Eng_lang variable to a factor with labels
data$Eng_lang <- factor(data$Eng_lang, 
                        levels = names(Eng_lang_labels), 
                        labels = Eng_lang_labels)
# 5. Add a note (metadata)
attr(data$Eng_lang, "note") <- "Is the Gender Equality Plan available in English in your organisation?"
# 6. Tabulate Eng_lang (equivalent to `tab Eng_lang` in Stata)
table(data$Eng_lang)


### Re-coding Oth_lang - GEP in other language/s
# 1. Rename variable v_322 to Oth_lang
colnames(data)[colnames(data) == "v_322"] <- "Oth_lang"
# 2. Add a variable label
var_label(data$Oth_lang) <- "3.3 GEP in other language/s"
# 3. Define labels for Oth_lang values
Oth_lang_labels <- c(
  "0" = "No",
  "1" = "Yes",
  "-77" = "Not seen the question"
)
# 4. Convert Oth_lang variable to a factor with labels
data$Oth_lang <- factor(data$Oth_lang, 
                        levels = names(Oth_lang_labels), 
                        labels = Oth_lang_labels)
# 5. Add a note (metadata)
attr(data$Oth_lang, "note") <- "Is the Gender Equality Plan available in other language/s in your organisation?-Other languages"
# 6. Tabulate Oth_lang (equivalent to `tab Oth_lang` in Stata)
table(data$Oth_lang)

### Re-coding EU_funded - Participation in a structural change project funded by the EU
# 1. Rename variable v_604 to EU_funded
colnames(data)[colnames(data) == "v_604"] <- "EU_funded"
# 2. Add a variable label
var_label(data$EU_funded) <- "3.4 Participation in a structural change project funded by the EU (e.g., Horizon 2020)"
# 3. Define labels for EU_funded values
EU_funded_labels <- c(
  "0" = "No",
  "1" = "Yes",
  "-77" = "Not seen the question"
)
# 4. Convert EU_funded variable to a factor with labels
data$EU_funded <- factor(data$EU_funded, 
                         levels = names(EU_funded_labels), 
                         labels = EU_funded_labels)
# 5. Tabulate EU_funded (equivalent to `tab EU_funded` in Stata)
table(data$EU_funded)

### Re-coding National_funded - Funding from a national agency
# 1. Rename variable v_605 to National_funded
colnames(data)[colnames(data) == "v_605"] <- "National_funded"
# 2. Add a variable label
var_label(data$National_funded) <- "3.4 Funding from a national agency"
# 3. Define labels for National_funded values
National_funded_labels <- c(
  "0" = "No",
  "1" = "Yes",
  "-77" = "Not seen the question"
)
# 4. Convert National_funded variable to a factor with labels
data$National_funded <- factor(data$National_funded, 
                               levels = names(National_funded_labels), 
                               labels = National_funded_labels)
# 5. Tabulate National_funded (equivalent to `tab National_funded` in Stata)
table(data$National_funded)

### Re-coding Training_or_counselling - Training or counselling offered nationally or regionally
# 1. Rename variable v_606 to Training_or_counselling
colnames(data)[colnames(data) == "v_606"] <- "Training_or_counselling"
# 2. Add a variable label
var_label(data$Training_or_counselling) <- "3.4 Training or counselling offered nationally or regionally"
# 3. Define labels for Training_or_counselling values
Training_or_counselling_labels <- c(
  "0" = "No",
  "1" = "Yes",
  "-77" = "Not seen the question"
)
# 4. Convert Training_or_counselling variable to a factor with labels
data$Training_or_counselling <- factor(data$Training_or_counselling, 
                                       levels = names(Training_or_counselling_labels), 
                                       labels = Training_or_counselling_labels)
# 5. Tabulate Training_or_counselling (equivalent to `tab Training_or_counselling` in Stata)
table(data$Training_or_counselling)


### Re-coding Networks - Cooperation in professional networks or networks of universities or research organisations
# 1. Rename variable v_607 to Networks
colnames(data)[colnames(data) == "v_607"] <- "Networks"
# 2. Add a variable label
var_label(data$Networks) <- "3.4 Cooperation in professional networks or networks of universities or research organisations"
# 3. Define labels for Networks values
Networks_labels <- c(
  "0" = "No",
  "1" = "Yes",
  "-77" = "Not seen the question"
)
# 4. Convert Networks variable to a factor with labels
data$Networks <- factor(data$Networks, 
                        levels = names(Networks_labels), 
                        labels = Networks_labels)
# 5. Tabulate Networks (equivalent to `tab Networks` in Stata)
table(data$Networks)


### Re-coding Not_Known - 3.4 Not Known
# 1. Rename variable v_608 to Not_Known
colnames(data)[colnames(data) == "v_608"] <- "Not_Known"
# 2. Add a variable label
var_label(data$Not_Known) <- "3.4 Not Known"
# 3. Define labels for Not_Known values
Not_Known_labels <- c(
  "0" = "No",
  "1" = "Yes",
  "-77" = "Not seen the question"
)
# 4. Convert Not_Known variable to a factor with labels
data$Not_Known <- factor(data$Not_Known, 
                         levels = names(Not_Known_labels), 
                         labels = Not_Known_labels)
# 5. Tabulate Not_Known (equivalent to `tab Not_Known` in Stata)
table(data$Not_Known)

### Re-coding No_Answer - 3.4 No Answer
# 1. Rename variable v_609 to No_Answer
colnames(data)[colnames(data) == "v_609"] <- "No_Answer"
# 2. Add a variable label
var_label(data$No_Answer) <- "3.4 No Answer"
# 3. Define labels for No_Answer values
No_Answer_labels <- c(
  "0" = "No",
  "1" = "Yes",
  "-77" = "Not seen the question"
)
# 4. Convert No_Answer variable to a factor with labels
data$No_Answer <- factor(data$No_Answer, 
                         levels = names(No_Answer_labels), 
                         labels = No_Answer_labels)
# 5. Tabulate No_Answer (equivalent to `tab No_Answer` in Stata)
table(data$No_Answer)


### Re-coding Horizon_Europe - Horizon Europe requirement
# 1. Rename variable v_610 to Horizon_Europe
colnames(data)[colnames(data) == "v_610"] <- "Horizon_Europe"
# 2. Add a variable label
var_label(data$Horizon_Europe) <- "3.5 Horizon Europe requirement"
# 3. Define labels for Horizon_Europe values
Horizon_Europe_labels <- c(
  "1" = "Yes – Our organisation set up a GEP because of the requirement.",
  "2" = "Yes – Our organisation adapted an existing plan to meet the requirements of HORIZON Europe.",
  "3" = "No – Our organisation set up a GEP before HORIZON Europe started (2021).",
  "4" = "No – Our organisation set up a GEP recently but regardless of the HORIZON Europe requirement.",
  "5" = "Not known",
  "6" = "No answer",
  "-77" = "Not seen the question"
)
# 4. Convert Horizon_Europe variable to a factor with labels
data$Horizon_Europe <- factor(data$Horizon_Europe, 
                              levels = names(Horizon_Europe_labels), 
                              labels = Horizon_Europe_labels)
# 5. Add a note (metadata)
attr(data$Horizon_Europe, "note") <- "Did HORIZON Europe requirement influence your organisation to set up a GEP?"
# 6. Tabulate Horizon_Europe (equivalent to `tab Horizon_Europe` in Stata)
table(data$Horizon_Europe)

### Re-coding National_or_Regional_Laws - 3.6 National or Regional Laws
# 1. Rename variable v_614 to National_or_Regional_Laws
colnames(data)[colnames(data) == "v_614"] <- "National_or_Regional_Laws"
# 2. Add a variable label
var_label(data$National_or_Regional_Laws) <- "3.6 National or Regional Laws"
# 3. Define labels for National_or_Regional_Laws values
National_or_Regional_Laws_labels <- c(
  "0" = "No",
  "1" = "Yes",
  "-77" = "Not seen the question"
)
# 4. Convert National_or_Regional_Laws variable to a factor with labels
data$National_or_Regional_Laws <- factor(data$National_or_Regional_Laws, 
                                         levels = names(National_or_Regional_Laws_labels), 
                                         labels = National_or_Regional_Laws_labels)
# 5. Tabulate National_or_Regional_Laws (equivalent to `tab National_or_Regional_Laws` in Stata)
table(data$National_or_Regional_Laws)


### Re-coding Funding_demands - 3.6 Getting (research) funding demands a gender equality plan
# 1. Rename variable v_615 to Funding_demands
colnames(data)[colnames(data) == "v_615"] <- "Funding_demands"
# 2. Add a variable label
var_label(data$Funding_demands) <- "3.6 Getting (research) funding demands a gender equality plan"
# 3. Define labels for Funding_demands values
Funding_demands_labels <- c(
  "0" = "No",
  "1" = "Yes",
  "-77" = "Not seen the question"
)
# 4. Convert Funding_demands variable to a factor with labels
data$Funding_demands <- factor(data$Funding_demands, 
                               levels = names(Funding_demands_labels), 
                               labels = Funding_demands_labels)
# 5. Tabulate Funding_demands (equivalent to `tab Funding_demands` in Stata)
table(data$Funding_demands)


### Re-coding GE_Initiative - 3.6 National or regional gender equality initiative that demands a gender equality plan
# 1. Rename variable v_616 to GE_Initiative
colnames(data)[colnames(data) == "v_616"] <- "GE_Initiative"
# 2. Add a variable label
var_label(data$GE_Initiative) <- "3.6 National or regional gender equality initiative that demands a gender equality plan"
# 3. Define labels for GE_Initiative values
GE_Initiative_labels <- c(
  "0" = "No",
  "1" = "Yes",
  "-77" = "Not seen the question"
)
# 4. Convert GE_Initiative variable to a factor with labels
data$GE_Initiative <- factor(data$GE_Initiative, 
                             levels = names(GE_Initiative_labels), 
                             labels = GE_Initiative_labels)
# 5. Tabulate GE_Initiative (equivalent to `tab GE_Initiative` in Stata)
table(data$GE_Initiative)

### Re-coding Voluntary_Initiatives - 3.6 National or regional gender equality initiative that demands a gender equality plan
# 1. Rename variable v_617 to Voluntary_Initiatives
colnames(data)[colnames(data) == "v_617"] <- "Voluntary_Initiatives"
# 2. Add a variable label
var_label(data$Voluntary_Initiatives) <- "3.6 National or regional gender equality initiative that demands a gender equality plan"
# 3. Define labels for Voluntary_Initiatives values
Voluntary_Initiatives_labels <- c(
  "0" = "No",
  "1" = "Yes",
  "-77" = "Not seen the question"
)
# 4. Convert Voluntary_Initiatives variable to a factor with labels
data$Voluntary_Initiatives <- factor(data$Voluntary_Initiatives, 
                                     levels = names(Voluntary_Initiatives_labels), 
                                     labels = Voluntary_Initiatives_labels)
# 5. Tabulate Voluntary_Initiatives (equivalent to `tab Voluntary_Initiatives` in Stata)
table(data$Voluntary_Initiatives)


### Re-coding Willingness - 3.6 National or regional gender equality initiative that demands a gender equality plan
# 1. Rename variable v_618 to Willingness
colnames(data)[colnames(data) == "v_618"] <- "Willingness"
# 2. Add a variable label
var_label(data$Willingness) <- "3.6 National or regional gender equality initiative that demands a gender equality plan"
# 3. Define labels for Willingness values
Willingness_labels <- c(
  "0" = "No",
  "1" = "Yes",
  "-77" = "Not seen the question"
)
# 4. Convert Willingness variable to a factor with labels
data$Willingness <- factor(data$Willingness, 
                           levels = names(Willingness_labels), 
                           labels = Willingness_labels)
# 5. Tabulate Willingness (equivalent to `tab Willingness` in Stata)
table(data$Willingness)

### Re-coding Bottom_up_initiatives - 3.6 Bottom-up initiatives in the organisation to foster gender equality
# 1. Rename variable v_619 to Bottom_up_initiatives
colnames(data)[colnames(data) == "v_619"] <- "Bottom_up_initiatives"
# 2. Add a variable label
var_label(data$Bottom_up_initiatives) <- "3.6 Bottom-up initiatives in the organisation to foster gender equality"
# 3. Define labels for Bottom_up_initiatives values
Bottom_up_initiatives_labels <- c(
  "0" = "No",
  "1" = "Yes",
  "-77" = "Not seen the question"
)
# 4. Convert Bottom_up_initiatives variable to a factor with labels
data$Bottom_up_initiatives <- factor(data$Bottom_up_initiatives, 
                                     levels = names(Bottom_up_initiatives_labels), 
                                     labels = Bottom_up_initiatives_labels)
# 5. Tabulate Bottom_up_initiatives (equivalent to `tab Bottom_up_initiatives` in Stata)
table(data$Bottom_up_initiatives)

### Re-coding Requirements_Other - 3.6 Other, please specify
# 1. Rename variable v_655 to Requirements_Other
colnames(data)[colnames(data) == "v_655"] <- "Requirements_Other"
# 2. Add a variable label
var_label(data$Requirements_Other) <- "3.6 Other, please specify"
# 3. Define labels for Requirements_Other values
Requirements_Other_labels <- c(
  "0" = "No",
  "1" = "Yes",
  "-77" = "Not seen the question"
)
# 4. Convert Requirements_Other variable to a factor with labels
data$Requirements_Other <- factor(data$Requirements_Other, 
                                  levels = names(Requirements_Other_labels), 
                                  labels = Requirements_Other_labels)
# 5. Tabulate Requirements_Other (equivalent to `tab Requirements_Other` in Stata)
table(data$Requirements_Other)


### Re-coding Requirements_Specify - 3.6 Open text response for other reasons not quoted
# 1. Rename variable v_656 to Requirements_Specify
colnames(data)[colnames(data) == "v_656"] <- "Requirements_Specify"
# 2. Add a variable label
var_label(data$Requirements_Specify) <- "3.6 Open text response for other reasons not quoted"
# 3. Add a note (metadata)
attr(data$Requirements_Specify, "note") <- "This variable captures open text responses for other reasons that were not quoted."
# 4. Replace coded values with text equivalents
data$Requirements_Specify[data$Requirements_Specify == "-99"] <- "Not edited"
data$Requirements_Specify[data$Requirements_Specify == "-66"] <- "Not Applicable"
# 5. Display unique responses for validation
unique(data$Requirements_Specify)


### Re-coding Requirements_Not_known - 3.6 Not known
# 1. Rename variable v_620 to Requirements_Not_known
colnames(data)[colnames(data) == "v_620"] <- "Requirements_Not_known"
# 2. Add a variable label
var_label(data$Requirements_Not_known) <- "3.6 Not known"
# 3. Define labels for Requirements_Not_known values
Requirements_Not_known_labels <- c(
  "0" = "No",
  "1" = "Yes",
  "-77" = "Not seen the question"
)
# 4. Convert Requirements_Not_known variable to a factor with labels
data$Requirements_Not_known <- factor(data$Requirements_Not_known, 
                                      levels = names(Requirements_Not_known_labels), 
                                      labels = Requirements_Not_known_labels)
# 5. Tabulate Requirements_Not_known (equivalent to `tab Requirements_Not_known` in Stata)
table(data$Requirements_Not_known)


### Re-coding Requirements_No_answer - 3.6 No answer
# 1. Rename variable v_621 to Requirements_No_answer
colnames(data)[colnames(data) == "v_621"] <- "Requirements_No_answer"
# 2. Add a variable label
var_label(data$Requirements_No_answer) <- "3.6 No answer"
# 3. Define labels for Requirements_No_answer values
Requirements_No_answer_labels <- c(
  "0" = "No",
  "1" = "Yes",
  "-77" = "Not seen the question"
)
# 4. Convert Requirements_No_answer variable to a factor with labels
data$Requirements_No_answer <- factor(data$Requirements_No_answer, 
                                      levels = names(Requirements_No_answer_labels), 
                                      labels = Requirements_No_answer_labels)
# 5. Tabulate Requirements_No_answer (equivalent to `tab Requirements_No_answer` in Stata)
table(data$Requirements_No_answer)


### Re-coding Requirements_Indicate - 3.7 Indicate Requirements if one of the two options is selected from Q3.6
# 1. Rename variable v_612 to Requirements_Indicate
colnames(data)[colnames(data) == "v_612"] <- "Requirements_Indicate"
# 2. Add a variable label
var_label(data$Requirements_Indicate) <- "3.7 Indicate Requirements if one of the two options is selected from Q3.6"
# 3. Add a note (metadata)
attr(data$Requirements_Indicate, "note") <- "This variable captures open text responses if one of the two options is selected from Q3.6"
# 4. Replace coded values with text equivalents
data$Requirements_Indicate[data$Requirements_Indicate == "-99"] <- "Not edited"
data$Requirements_Indicate[data$Requirements_Indicate == "-66"] <- "Not seen the question"
# 5. Display unique responses for validation
unique(data$Requirements_Indicate)

### Re-coding Own_board - 4.1 Management board, rectorate, executive committee, directorate
# 1. Rename variable v_72 to Own_board
colnames(data)[colnames(data) == "v_72"] <- "Own_board"
# 2. Add a variable label
var_label(data$Own_board) <- "4.1 Management board, rectorate, executive committee, directorate"
# 3. Define labels for Own_board values
Own_board_labels <- c(
  "0" = "No",
  "1" = "Yes",
  "-77" = "Not seen the question"
)
# 4. Convert Own_board variable to a factor with labels
data$Own_board <- factor(data$Own_board, 
                         levels = names(Own_board_labels), 
                         labels = Own_board_labels)
# 5. Add a note (metadata)
attr(data$Own_board, "note") <- "Is the Gender Equality Plan approved by Management board, rectorate, executive committee, directorate?"
# 6. Tabulate Own_board (equivalent to `tab Own_board` in Stata)
table(data$Own_board)

### Re-coding Own_internal - 4.1 Decision-making body of internal members: e.g. Senate
colnames(data)[colnames(data) == "v_73"] <- "Own_internal"
var_label(data$Own_internal) <- "4.1 Decision-making body of members: e.g. Senate"
Own_internal_labels <- c("0" = "No", "1" = "Yes", "-77" = "Not seen the question")
data$Own_internal <- factor(data$Own_internal, levels = names(Own_internal_labels), labels = Own_internal_labels)
attr(data$Own_internal, "note") <- "Is the Gender Equality Plan approved by the decision-making body of internal members: e.g. Senate?"
table(data$Own_internal)

### Re-coding Own_external - 4.1 Decision-making body of (mostly) external members
colnames(data)[colnames(data) == "v_74"] <- "Own_external"
var_label(data$Own_external) <- "4.1 Decision-making body of (mostly) external members: e.g. university council, supervisory board"
Own_external_labels <- c("0" = "No", "1" = "Yes", "-77" = "Not seen the question")
data$Own_external <- factor(data$Own_external, levels = names(Own_external_labels), labels = Own_external_labels)
attr(data$Own_external, "note") <- "Is the Gender Equality Plan approved by the decision-making body of (mostly) external members, such as the university council or supervisory board?"
table(data$Own_external)

### Re-coding Own_GE_comittee - 4.1 Gender equality committee (or equivalent committee)
colnames(data)[colnames(data) == "v_75"] <- "Own_GE_comittee"
var_label(data$Own_GE_comittee) <- "4.1 Gender equality committee (or equivalent committee)"
Own_GE_comittee_labels <- c("0" = "No", "1" = "Yes", "-77" = "Not seen the question")
data$Own_GE_comittee <- factor(data$Own_GE_comittee, levels = names(Own_GE_comittee_labels), labels = Own_GE_comittee_labels)
attr(data$Own_GE_comittee, "note") <- "Is the GEP approved by Gender equality committee (or equivalent committee)?"
table(data$Own_GE_comittee)

### Re-coding Own_GE_officer - 4.1 Gender equality officer (or equivalent)
colnames(data)[colnames(data) == "v_76"] <- "Own_GE_officer"
var_label(data$Own_GE_officer) <- "4.1 Gender equality officer (or equivalent)"
Own_GE_officer_labels <- c("0" = "No", "1" = "Yes", "-77" = "Not seen the question")
data$Own_GE_officer <- factor(data$Own_GE_officer, levels = names(Own_GE_officer_labels), labels = Own_GE_officer_labels)
attr(data$Own_GE_officer, "note") <- "Is the GEP approved by Gender equality officer (or equivalent)?"
table(data$Own_GE_officer)

### Re-coding Own_council - 4.1 Scientific council
colnames(data)[colnames(data) == "v_628"] <- "Own_council"
var_label(data$Own_council) <- "4.1 Scientific council"
Own_council_labels <- c("0" = "No", "1" = "Yes", "-77" = "Not seen the question")
data$Own_council <- factor(data$Own_council, levels = names(Own_council_labels), labels = Own_council_labels)
attr(data$Own_council, "note") <- "Is the GEP approved by scientific council?"
table(data$Own_council)

### Re-coding Own_assembly - 4.1 Members assembly, staff assembly
colnames(data)[colnames(data) == "v_629"] <- "Own_assembly"
var_label(data$Own_assembly) <- "4.1 Members assembly, staff assembly"
Own_assembly_labels <- c("0" = "No", "1" = "Yes", "-77" = "Not seen the question")
data$Own_assembly <- factor(data$Own_assembly, levels = names(Own_assembly_labels), labels = Own_assembly_labels)
attr(data$Own_assembly, "note") <- "Is the GEP approved by Members assembly, staff assembly?"
table(data$Own_assembly)

### Re-coding Own_other - 4.1 Other bodies then specified
colnames(data)[colnames(data) == "v_630"] <- "Own_other"
var_label(data$Own_other) <- "4.1 Other bodies then specified"
Own_other_labels <- c("0" = "No", "1" = "Yes", "-77" = "Not seen the question")
data$Own_other <- factor(data$Own_other, levels = names(Own_other_labels), labels = Own_other_labels)
attr(data$Own_other, "note") <- "Is the GEP approved by other bodies than specified above?"
table(data$Own_other)

### Re-coding Own_specify - 4.1 Open text response for other reasons not quoted
colnames(data)[colnames(data) == "v_631"] <- "Own_specify"
var_label(data$Own_specify) <- "4.1 Open text response for other reasons not quoted"
attr(data$Own_specify, "note") <- "The GEP is approved by Whom?"
data$Own_specify[data$Own_specify == "-99"] <- "Not edited"
data$Own_specify[data$Own_specify == "-66"] <- "Not seen the question"
unique(data$Own_specify)

### Re-coding Published_GEP - 4.2 GEP published?
colnames(data)[colnames(data) == "v_79"] <- "Published_GEP"
var_label(data$Published_GEP) <- "4.2 GEP published?"
Published_GEP_labels <- c(
  "1" = "The document is accessible to people outside the organisation",
  "2" = "Only internal document and accessible to all members of the organisation",
  "3" = "Only internal document and accessible only to the management",
  "-77" = "Not seen the question"
)
data$Published_GEP <- factor(data$Published_GEP, levels = names(Published_GEP_labels), labels = Published_GEP_labels)
attr(data$Published_GEP, "note") <- "In which way is the GEP published?"
table(data$Published_GEP)

### Re-coding URL_GEP - 4.3 URL of the GEP
colnames(data)[colnames(data) == "v_80"] <- "URL_GEP"
var_label(data$URL_GEP) <- "4.3 URL of the GEP"
attr(data$URL_GEP, "note") <- "Please state the URL via which the Gender Equality Plan is publicly accessible."
data$URL_GEP[data$URL_GEP == "-99"] <- "Not edited"
data$URL_GEP[data$URL_GEP == "-66"] <- "Not seen the question"
unique(data$URL_GEP)


### Re-coding First_GEP - 4.4 The first GEP
# 1. Rename variable v_84 to First_GEP
colnames(data)[colnames(data) == "v_84"] <- "First_GEP"
# 2. Convert First_GEP to numeric format
data$First_GEP <- as.numeric(data$First_GEP)
# 3. Assign values from v_85 where applicable, handling NA values
if ("v_85" %in% colnames(data)) {
  data$First_GEP[which(data$First_GEP == 1 & !is.na(data$v_85))] <- data$v_85[which(data$First_GEP == 1 & !is.na(data$v_85))]
}
# 4. Replace specific codes with corresponding years
year_replacements <- c("4" = 2021, "5" = 2022, "6" = 2023, "7" = 2009, "8" = 2020, "9" = 2016, "10" = 2017)
data$First_GEP <- ifelse(as.character(data$First_GEP) %in% names(year_replacements), 
                         year_replacements[as.character(data$First_GEP)], 
                         data$First_GEP)
# 5. Convert to character for categorical replacements
data$First_GEP <- as.character(data$First_GEP)
# 6. Replace categorical values
data$First_GEP[data$First_GEP == "2"] <- "Not known"
data$First_GEP[data$First_GEP == "3"] <- "No answer"
data$First_GEP[data$First_GEP == "0"] <- "Not edited"
data$First_GEP[data$First_GEP == "-77"] <- "Not seen the question"
# 7. Add a variable label
var_label(data$First_GEP) <- "4.4 The first GEP"
# 8. Add a note (metadata)
attr(data$First_GEP, "note") <- "When was the first GEP adopted by your organisation?"
# 9. Remove v_85 safely
if ("v_85" %in% colnames(data)) {
  data$v_85 <- NULL
}
# 10. Display unique responses for validation
unique(data$First_GEP)


### Re-coding Time_frame_GEP - 4.5 Time frame of the GEP
# 1. Rename variable v_657 to Time_frame_GEP
colnames(data)[colnames(data) == "v_657"] <- "Time_frame_GEP"
# 2. Convert to character for categorical replacements
data$Time_frame_GEP <- as.character(data$Time_frame_GEP)
# 3. Assign values from v_658 where applicable, handling NA values
if ("v_658" %in% colnames(data)) {
  data$Time_frame_GEP[which(data$Time_frame_GEP == "1" & !is.na(data$v_658))] <- as.character(data$v_658[which(data$Time_frame_GEP == "1" & !is.na(data$v_658))])
}
# 4. Replace categorical values
data$Time_frame_GEP[data$Time_frame_GEP == "2"] <- "The current plan does not have a specific time frame"
data$Time_frame_GEP[data$Time_frame_GEP == "3"] <- "Not known"
data$Time_frame_GEP[data$Time_frame_GEP == "4"] <- "No answer"
data$Time_frame_GEP[data$Time_frame_GEP == "0"] <- "Not edited"
data$Time_frame_GEP[data$Time_frame_GEP == "-99"] <- "Not edited"
data$Time_frame_GEP[data$Time_frame_GEP == "-77"] <- "Not seen the question"
# 5. Add a variable label
var_label(data$Time_frame_GEP) <- "4.5 Time frame of the GEP"
# 6. Add a note (metadata)
attr(data$Time_frame_GEP, "note") <- "What period does the current plan cover?"
# 7. Remove v_658 safely
if ("v_658" %in% colnames(data)) {
  data$v_658 <- NULL
}
# 8. Display unique responses for validation
unique(data$Time_frame_GEP)


### Re-coding Awareness_raising - 4.6 Awareness-raising and training
colnames(data)[colnames(data) == "v_91"] <- "Awareness_raising"
var_label(data$Awareness_raising) <- "4.6 Awareness-raising and training"
Awareness_raising_labels <- c(
  "1" = "No measures designed",
  "2" = "1 measure",
  "3" = "2-5 measures",
  "4" = "6-10 measures",
  "5" = "more than 10 measures",
  "6" = "not applicable",
  "-77" = "Not seen the question"
)
data$Awareness_raising <- factor(data$Awareness_raising, levels = names(Awareness_raising_labels), labels = Awareness_raising_labels)
attr(data$Awareness_raising, "note") <- "Awareness-raising and training (e.g. workshops and training on gender bias, training for recruitment committees, booklets, films or posters)"
table(data$Awareness_raising)

### Re-coding Work_life_balance - 4.6 Work-life balance and organisational culture
colnames(data)[colnames(data) == "v_92"] <- "Work_life_balance"
var_label(data$Work_life_balance) <- "4.6 Work-life balance and organisational culture"
Work_life_balance_labels <- Awareness_raising_labels  # Reuse the same labels
data$Work_life_balance <- factor(data$Work_life_balance, levels = names(Work_life_balance_labels), labels = Work_life_balance_labels)
attr(data$Work_life_balance, "note") <- "Work-life balance and organisational culture (e.g. child-care facilities for staff and students, dual career policy, network of fathers on the campus, respectful interaction, welcoming culture)"
table(data$Work_life_balance)

### Re-coding Gender_balance_leadership - 4.6 Gender balance in leadership and decision-making
colnames(data)[colnames(data) == "v_93"] <- "Gender_balance_leadership"
var_label(data$Gender_balance_leadership) <- "4.6 Gender balance in leadership and decision-making"
data$Gender_balance_leadership <- factor(data$Gender_balance_leadership, levels = names(Awareness_raising_labels), labels = Awareness_raising_labels)
attr(data$Gender_balance_leadership, "note") <- "Gender balance in leadership and decision-making (e.g. quota for decision-making bodies, gender-integrated leadership program)"
table(data$Gender_balance_leadership)

### Re-coding Gender_equality_rec - 4.6 Gender equality in recruitment and career progression
colnames(data)[colnames(data) == "v_94"] <- "Gender_equality_rec"
var_label(data$Gender_equality_rec) <- "4.6 Gender equality in recruitment and career progression"
data$Gender_equality_rec <- factor(data$Gender_equality_rec, levels = names(Awareness_raising_labels), labels = Awareness_raising_labels)
attr(data$Gender_equality_rec, "note") <- "Gender equality in recruitment and career progression (e.g. active recruitment, gender equality in appointment procedures, coaching and mentoring programs for women researchers)"
table(data$Gender_equality_rec)

### Re-coding Measures_gender_violence - 4.6 Measures against gender-based violence, including sexual harassment
colnames(data)[colnames(data) == "v_95"] <- "Measures_gender_violence"
var_label(data$Measures_gender_violence) <- "4.6 Measures against gender-based violence, including sexual harassment"
data$Measures_gender_violence <- factor(data$Measures_gender_violence, levels = names(Awareness_raising_labels), labels = Awareness_raising_labels)
attr(data$Measures_gender_violence, "note") <- "Measures against gender-based violence, including sexual harassment, (e.g. complaints office, guidelines on sexual harassment)"
table(data$Measures_gender_violence)

### Re-coding Gender_dimension_int - 4.6 Integration of the gender dimension into research and teaching content
colnames(data)[colnames(data) == "v_96"] <- "Gender_dimension_int"
var_label(data$Gender_dimension_int) <- "4.6 Integration of the gender dimension into research and teaching content"
data$Gender_dimension_int <- factor(data$Gender_dimension_int, levels = names(Awareness_raising_labels), labels = Awareness_raising_labels)
attr(data$Gender_dimension_int, "note") <- "Integration of the gender dimension into research and teaching content (e.g. counselling for research funding, gender lectureship)"
table(data$Gender_dimension_int)


### Re-coding Target_Students - 4.7 Students addressed in the GEP
colnames(data)[colnames(data) == "v_97"] <- "Target_Students"
var_label(data$Target_Students) <- "4.7 Students addressed in the GEP"
Target_Students_labels <- c("0" = "No", "1" = "Yes", "-77" = "Not seen the question")
data$Target_Students <- factor(data$Target_Students, levels = names(Target_Students_labels), labels = Target_Students_labels)
attr(data$Target_Students, "note") <- "Which target groups does the Gender Equality Plan address?-Students"
table(data$Target_Students)

### Re-coding Target_PHDs - 4.7 PhD students/candidates addressed in the GEP
colnames(data)[colnames(data) == "v_98"] <- "Target_PHDs"
var_label(data$Target_PHDs) <- "4.7 PhD students/candidates addressed in the GEP"
data$Target_PHDs <- factor(data$Target_PHDs, levels = names(Target_Students_labels), labels = Target_Students_labels)
attr(data$Target_PHDs, "note") <- "Which target groups does the Gender Equality Plan address?-PhD students/candidates"
table(data$Target_PHDs)

### Re-coding Target_AcademicStaff - 4.7 Academic or Scientific Staff addressed in the GEP
colnames(data)[colnames(data) == "v_99"] <- "Target_AcademicStaff"
var_label(data$Target_AcademicStaff) <- "4.7 Academic or Scientific Staff addressed in the GEP"
data$Target_AcademicStaff <- factor(data$Target_AcademicStaff, levels = names(Target_Students_labels), labels = Target_Students_labels)
attr(data$Target_AcademicStaff, "note") <- "Which target groups does the Gender Equality Plan address?-Academic or Scientific Staff"
table(data$Target_AcademicStaff)

### Re-coding Target_AdminTechStaff - 4.7 Administrative and Technical Staff addressed in the GEP
colnames(data)[colnames(data) == "v_100"] <- "Target_AdminTechStaff"
var_label(data$Target_AdminTechStaff) <- "4.7 Administrative and Technical Staff addressed in the GEP"
data$Target_AdminTechStaff <- factor(data$Target_AdminTechStaff, levels = names(Target_Students_labels), labels = Target_Students_labels)
attr(data$Target_AdminTechStaff, "note") <- "Which target groups does the Gender Equality Plan address?-Administrative and Technical Staff"
table(data$Target_AdminTechStaff)

### Re-coding Target_LeadingPositions - 4.7 Leading Positions addressed in the GEP
colnames(data)[colnames(data) == "v_101"] <- "Target_LeadingPositions"
var_label(data$Target_LeadingPositions) <- "4.7 Leading Positions addressed in the GEP"
data$Target_LeadingPositions <- factor(data$Target_LeadingPositions, levels = names(Target_Students_labels), labels = Target_Students_labels)
attr(data$Target_LeadingPositions, "note") <- "Which target groups does the Gender Equality Plan address?-Leading Positions"
table(data$Target_LeadingPositions)

### Re-coding Target_Applicants - 4.7 Applicants addressed in the GEP
colnames(data)[colnames(data) == "v_102"] <- "Target_Applicants"
var_label(data$Target_Applicants) <- "4.7 Applicants addressed in the GEP"
data$Target_Applicants <- factor(data$Target_Applicants, levels = names(Target_Students_labels), labels = Target_Students_labels)
attr(data$Target_Applicants, "note") <- "Which target groups does the Gender Equality Plan address?-Applicants"
table(data$Target_Applicants)

### Re-coding Target_Reviewers - 4.7 Reviewers addressed in the GEP
colnames(data)[colnames(data) == "v_103"] <- "Target_Reviewers"
var_label(data$Target_Reviewers) <- "4.7 Reviewers addressed in the GEP"
data$Target_Reviewers <- factor(data$Target_Reviewers, levels = names(Target_Students_labels), labels = Target_Students_labels)
attr(data$Target_Reviewers, "note") <- "Which target groups does the Gender Equality Plan address?-Reviewers"
table(data$Target_Reviewers)

### Re-coding Target_Other - 4.7 Others addressed in the GEP
colnames(data)[colnames(data) == "v_104"] <- "Target_Other"
var_label(data$Target_Other) <- "4.7 Others addressed in the GEP"
data$Target_Other <- factor(data$Target_Other, levels = names(Target_Students_labels), labels = Target_Students_labels)
attr(data$Target_Other, "note") <- "Which target groups does the Gender Equality Plan address?"
table(data$Target_Other)

### Re-coding Target_Specified - 4.7 Open text response for other reasons not quoted
colnames(data)[colnames(data) == "v_105"] <- "Target_Specified"
var_label(data$Target_Specified) <- "4.7 Open text response for other reasons not quoted"
attr(data$Target_Specified, "note") <- "The GEP addresses whom?"
data$Target_Specified[data$Target_Specified == "-99"] <- "Not edited"
data$Target_Specified[data$Target_Specified == "-66"] <- "Not seen the question"
unique(data$Target_Specified)

### Re-coding Terms_Diff_Groups - 4.8 GEP mentions differences within gender groups
data$Terms_Diff_Groups <- NA  # Create a new variable with missing values
var_label(data$Terms_Diff_Groups) <- "4.8 GEP mentions differences within gender groups (e.g. Black women or persons with disabilities)"
attr(data$Terms_Diff_Groups, "note") <- "GEP mentions differences within gender groups (e.g. Black women or persons with disabilities)"

data$Terms_Diff_Groups[data$v_106 == 1] <- 1
data$Terms_Diff_Groups[data$v_107 == 1] <- 2
data$Terms_Diff_Groups[data$v_122 == 1] <- 3
data$Terms_Diff_Groups[data$v_130 == 1] <- 4
data$Terms_Diff_Groups[data$v_106 == -77 & data$v_107 == -77 & data$v_122 == -77 & data$v_130 == -77] <- -77
data$Terms_Diff_Groups[data$v_106 == 0 & data$v_107 == 0 & data$v_122 == 0 & data$v_130 == 0] <- 0

Terms_Diff_Groups_labels <- c("1" = "Yes", "2" = "No", "3" = "Not known", "4" = "No answer", "-77" = "Not seen the question", "0" = "Not edited")
data$Terms_Diff_Groups <- factor(data$Terms_Diff_Groups, levels = names(Terms_Diff_Groups_labels), labels = Terms_Diff_Groups_labels)

table(data$Terms_Diff_Groups)

### Re-coding Terms_Disa_Groups - 4.8 Equal opportunities for disadvantaged groups
data$Terms_Disa_Groups <- NA
var_label(data$Terms_Disa_Groups) <- "4.8 Equal opportunities for disadvantaged groups"
attr(data$Terms_Disa_Groups, "note") <- "It provides measures to enhance equal opportunities for disadvantaged groups."

data$Terms_Disa_Groups[data$v_108 == 1] <- 1
data$Terms_Disa_Groups[data$v_109 == 1] <- 2
data$Terms_Disa_Groups[data$v_123 == 1] <- 3
data$Terms_Disa_Groups[data$v_131 == 1] <- 4
data$Terms_Disa_Groups[data$v_108 == -77 & data$v_109 == -77 & data$v_123 == -77 & data$v_131 == -77] <- -77
data$Terms_Disa_Groups[data$v_108 == 0 & data$v_109 == 0 & data$v_123 == 0 & data$v_131 == 0] <- 0

data$Terms_Disa_Groups <- factor(data$Terms_Disa_Groups, levels = names(Terms_Diff_Groups_labels), labels = Terms_Diff_Groups_labels)

table(data$Terms_Disa_Groups)

### Re-coding Terms_Intersectionality - 4.8 Concept of intersectionality
data$Terms_Intersectionality <- NA
var_label(data$Terms_Intersectionality) <- "4.8 Concept of intersectionality"
attr(data$Terms_Intersectionality, "note") <- "It includes the concept of intersectionality."

data$Terms_Intersectionality[data$v_110 == 1] <- 1
data$Terms_Intersectionality[data$v_111 == 1] <- 2
data$Terms_Intersectionality[data$v_124 == 1] <- 3
data$Terms_Intersectionality[data$v_132 == 1] <- 4
data$Terms_Intersectionality[data$v_110 == -77 & data$v_111 == -77 & data$v_124 == -77 & data$v_132 == -77] <- -77
data$Terms_Intersectionality[data$v_110 == 0 & data$v_111 == 0 & data$v_124 == 0 & data$v_132 == 0] <- 0

data$Terms_Intersectionality <- factor(data$Terms_Intersectionality, levels = names(Terms_Diff_Groups_labels), labels = Terms_Diff_Groups_labels)

table(data$Terms_Intersectionality)

### Re-coding Terms_Diversity - 4.8 Concept of diversity
data$Terms_Diversity <- NA
var_label(data$Terms_Diversity) <- "4.8 Concept of diversity"
attr(data$Terms_Diversity, "note") <- "It includes the concept of diversity."

data$Terms_Diversity[data$v_112 == 1] <- 1
data$Terms_Diversity[data$v_113 == 1] <- 2
data$Terms_Diversity[data$v_125 == 1] <- 3
data$Terms_Diversity[data$v_133 == 1] <- 4
data$Terms_Diversity[data$v_112 == -77 & data$v_113 == -77 & data$v_125 == -77 & data$v_133 == -77] <- -77
data$Terms_Diversity[data$v_112 == 0 & data$v_113 == 0 & data$v_125 == 0 & data$v_133 == 0] <- 0

data$Terms_Diversity <- factor(data$Terms_Diversity, levels = names(Terms_Diff_Groups_labels), labels = Terms_Diff_Groups_labels)

table(data$Terms_Diversity)

### Re-coding Terms_Gender_Diversity - 4.8 Gender diversity
data$Terms_Gender_Diversity <- NA
var_label(data$Terms_Gender_Diversity) <- "4.8 Gender diversity"
attr(data$Terms_Gender_Diversity, "note") <- "It mentions gender diversity (e.g. non-binary, divers, trans, etc. persons)."

data$Terms_Gender_Diversity[data$v_114 == 1] <- 1
data$Terms_Gender_Diversity[data$v_115 == 1] <- 2
data$Terms_Gender_Diversity[data$v_126 == 1] <- 3
data$Terms_Gender_Diversity[data$v_134 == 1] <- 4
data$Terms_Gender_Diversity[data$v_114 == -77 & data$v_115 == -77 & data$v_126 == -77 & data$v_134 == -77] <- -77
data$Terms_Gender_Diversity[data$v_114 == 0 & data$v_115 == 0 & data$v_126 == 0 & data$v_134 == 0] <- 0

data$Terms_Gender_Diversity <- factor(data$Terms_Gender_Diversity, levels = names(Terms_Diff_Groups_labels), labels = Terms_Diff_Groups_labels)

table(data$Terms_Gender_Diversity)

### Re-coding Terms_Gender_Sensitive - 4.8 Gender-neutral or gender-sensitive language
data$Terms_Gender_Sensitive <- NA
var_label(data$Terms_Gender_Sensitive) <- "4.8 Gender-neutral or gender-sensitive language"
attr(data$Terms_Gender_Sensitive, "note") <- "It uses gender-neutral or gender-sensitive language."

data$Terms_Gender_Sensitive[data$v_116 == 1] <- 1
data$Terms_Gender_Sensitive[data$v_117 == 1] <- 2
data$Terms_Gender_Sensitive[data$v_127 == 1] <- 3
data$Terms_Gender_Sensitive[data$v_135 == 1] <- 4
data$Terms_Gender_Sensitive[data$v_116 == -77 & data$v_117 == -77 & data$v_127 == -77 & data$v_135 == -77] <- -77
data$Terms_Gender_Sensitive[data$v_116 == 0 & data$v_117 == 0 & data$v_127 == 0 & data$v_135 == 0] <- 0

data$Terms_Gender_Sensitive <- factor(data$Terms_Gender_Sensitive, levels = names(Terms_Diff_Groups_labels), labels = Terms_Diff_Groups_labels)

table(data$Terms_Gender_Sensitive)

### Dropping Unnecessary Columns
drop_cols <- c("v_106", "v_107", "v_122", "v_130", "v_108", "v_109", "v_123", "v_131",
               "v_110", "v_111", "v_124", "v_132", "v_112", "v_113", "v_125", "v_133",
               "v_114", "v_115", "v_126", "v_134", "v_116", "v_117", "v_127", "v_135")
data <- data[, !colnames(data) %in% drop_cols]

### Reordering variables after Target_Specified

# 1. Get current column names
current_cols <- colnames(data)
# 2. Define the variables to reorder after Target_Specified
new_vars <- c("Terms_Diff_Groups", "Terms_Disa_Groups", "Terms_Intersectionality", 
              "Terms_Diversity", "Terms_Gender_Diversity", "Terms_Gender_Sensitive")
# 3. Find position of Target_Specified
pos_target_specified <- match("Target_Specified", current_cols)
# 4. Create new column order: Keep everything before Target_Specified, insert new vars, then add the rest
new_order <- c(current_cols[1:pos_target_specified], new_vars, current_cols[(pos_target_specified + 1):length(current_cols)])
# 5. Reorder data
data <- data[, new_order]
# 6. Verify new order
print(colnames(data))



### Re-coding Inequalities_Race - 4.9 Race, ethnicity, ethnic minorities, BAME
colnames(data)[colnames(data) == "v_138"] <- "Inequalities_Race"
var_label(data$Inequalities_Race) <- "4.9 Race, ethnicity, ethnic minorities, BAME"
Inequalities_Race_labels <- c("0" = "No", "1" = "Yes", "-77" = "Not seen the Question")
data$Inequalities_Race <- factor(data$Inequalities_Race, levels = names(Inequalities_Race_labels), labels = Inequalities_Race_labels)
attr(data$Inequalities_Race, "note") <- "Race, ethnicity, ethnic minorities, BAME [Black, Asian, Minority Ethnics], BIPOC [Black, Indigenous People of Colour]?"
table(data$Inequalities_Race)

### Re-coding Inequalities_Nationality - 4.9 Nationality
colnames(data)[colnames(data) == "v_139"] <- "Inequalities_Nationality"
var_label(data$Inequalities_Nationality) <- "4.9 Nationality"
data$Inequalities_Nationality <- factor(data$Inequalities_Nationality, levels = names(Inequalities_Race_labels), labels = Inequalities_Race_labels)
attr(data$Inequalities_Nationality, "note") <- "Nationality quoted?"
table(data$Inequalities_Nationality)

### Re-coding Inequalities_Religion - 4.9 Religion
colnames(data)[colnames(data) == "v_140"] <- "Inequalities_Religion"
var_label(data$Inequalities_Religion) <- "4.9 Religion"
data$Inequalities_Religion <- factor(data$Inequalities_Religion, levels = names(Inequalities_Race_labels), labels = Inequalities_Race_labels)
attr(data$Inequalities_Religion, "note") <- "Religion quoted?"
table(data$Inequalities_Religion)

### Re-coding Inequalities_Class - 4.9 Class, socioeconomic status, first-generation students or academics
colnames(data)[colnames(data) == "v_141"] <- "Inequalities_Class"
var_label(data$Inequalities_Class) <- "4.9 Class, socioeconomic status, first-generation students or academics"
data$Inequalities_Class <- factor(data$Inequalities_Class, levels = names(Inequalities_Race_labels), labels = Inequalities_Race_labels)
attr(data$Inequalities_Class, "note") <- "Class, socioeconomic status, first-generation students or academics quoted?"
table(data$Inequalities_Class)

### Re-coding Inequalities_Age - 4.9 Age
colnames(data)[colnames(data) == "v_142"] <- "Inequalities_Age"
var_label(data$Inequalities_Age) <- "4.9 Age"
data$Inequalities_Age <- factor(data$Inequalities_Age, levels = names(Inequalities_Race_labels), labels = Inequalities_Race_labels)
attr(data$Inequalities_Age, "note") <- "Age quoted?"
table(data$Inequalities_Age)

### Re-coding Inequalities_Sex_Orient - 4.9 Sexual orientation
colnames(data)[colnames(data) == "v_143"] <- "Inequalities_Sex_Orient"
var_label(data$Inequalities_Sex_Orient) <- "4.9 Sexual orientation"
data$Inequalities_Sex_Orient <- factor(data$Inequalities_Sex_Orient, levels = names(Inequalities_Race_labels), labels = Inequalities_Race_labels)
attr(data$Inequalities_Sex_Orient, "note") <- "Sexual orientation quoted?"
table(data$Inequalities_Sex_Orient)

### Re-coding Inequalities_Gender_Id - 4.9 Gender identity
colnames(data)[colnames(data) == "v_144"] <- "Inequalities_Gender_Id"
var_label(data$Inequalities_Gender_Id) <- "4.9 Gender identity"
data$Inequalities_Gender_Id <- factor(data$Inequalities_Gender_Id, levels = names(Inequalities_Race_labels), labels = Inequalities_Race_labels)
attr(data$Inequalities_Gender_Id, "note") <- "Gender identity quoted?"
table(data$Inequalities_Gender_Id)

### Re-coding Inequalities_Disability - 4.9 Disability/chronic health/mental impairment
colnames(data)[colnames(data) == "v_145"] <- "Inequalities_Disability"
var_label(data$Inequalities_Disability) <- "4.9 Disability/chronic health/mental impairment"
data$Inequalities_Disability <- factor(data$Inequalities_Disability, levels = names(Inequalities_Race_labels), labels = Inequalities_Race_labels)
attr(data$Inequalities_Disability, "note") <- "Disability/chronic health/mental impairment quoted?"
table(data$Inequalities_Disability)

### Re-coding Inequalities_Care - 4.9 Care responsibilities
colnames(data)[colnames(data) == "v_146"] <- "Inequalities_Care"
var_label(data$Inequalities_Care) <- "4.9 Care responsibilities"
data$Inequalities_Care <- factor(data$Inequalities_Care, levels = names(Inequalities_Race_labels), labels = Inequalities_Race_labels)
attr(data$Inequalities_Care, "note") <- "Care responsibilities quoted?"
table(data$Inequalities_Care)

### Re-coding Inequalities_Additional - 4.9 Additional inequalities not listed
colnames(data)[colnames(data) == "v_147"] <- "Inequalities_Additional"
var_label(data$Inequalities_Additional) <- "4.9 Additional inequalities not listed"
data$Inequalities_Additional <- factor(data$Inequalities_Additional, levels = names(Inequalities_Race_labels), labels = Inequalities_Race_labels)
attr(data$Inequalities_Additional, "note") <- "Additional inequalities not listed quoted?"
table(data$Inequalities_Additional)

### Re-coding Inequalities_Add_Text - 4.9 Textual response for additional inequalities not listed
colnames(data)[colnames(data) == "v_148"] <- "Inequalities_Add_Text"
var_label(data$Inequalities_Add_Text) <- "4.9 Textual response for additional inequalities not listed"
attr(data$Inequalities_Add_Text, "note") <- "Textual response for additional inequalities not listed. This variable allows for open-ended responses."
data$Inequalities_Add_Text[data$Inequalities_Add_Text == "-99"] <- "Not edited"
data$Inequalities_Add_Text[data$Inequalities_Add_Text == "-66"] <- "Not seen the Question"
unique(data$Inequalities_Add_Text)

### Re-coding No_GEP_Time - 3.9 Time-consuming activities
colnames(data)[colnames(data) == "v_622"] <- "No_GEP_Time"
var_label(data$No_GEP_Time) <- "3.9 Time-consuming activities"
No_GEP_Time_labels <- c("0" = "No", "1" = "Yes", "-77" = "Not seen the Question")
data$No_GEP_Time <- factor(data$No_GEP_Time, levels = names(No_GEP_Time_labels), labels = No_GEP_Time_labels)
attr(data$No_GEP_Time, "note") <- "Time-consuming activities quoted?"
table(data$No_GEP_Time)

### Re-coding No_GEP_Resources - 3.9 No personal resources
colnames(data)[colnames(data) == "v_623"] <- "No_GEP_Resources"
var_label(data$No_GEP_Resources) <- "3.9 No personal resources"
data$No_GEP_Resources <- factor(data$No_GEP_Resources, levels = names(No_GEP_Time_labels), labels = No_GEP_Time_labels)
attr(data$No_GEP_Resources, "note") <- "No personal resources quoted?"
table(data$No_GEP_Resources)

### Re-coding No_GEP_Acceptance - 3.9 No acceptance
colnames(data)[colnames(data) == "v_624"] <- "No_GEP_Acceptance"
var_label(data$No_GEP_Acceptance) <- "3.9 No acceptance"
data$No_GEP_Acceptance <- factor(data$No_GEP_Acceptance, levels = names(No_GEP_Time_labels), labels = No_GEP_Time_labels)
attr(data$No_GEP_Acceptance, "note") <- "No acceptance quoted?"
table(data$No_GEP_Acceptance)

### Re-coding No_GEP_Necessity - 3.9 No necessity quoted
colnames(data)[colnames(data) == "v_625"] <- "No_GEP_Necessity"
var_label(data$No_GEP_Necessity) <- "3.9 No necessity quoted"
data$No_GEP_Necessity <- factor(data$No_GEP_Necessity, levels = names(No_GEP_Time_labels), labels = No_GEP_Time_labels)
attr(data$No_GEP_Necessity, "note") <- "No necessity quoted"
table(data$No_GEP_Necessity)

### Re-coding No_GEP_Other_Specify - 3.9 Other reasons not quoted
colnames(data)[colnames(data) == "v_626"] <- "No_GEP_Other_Specify"
var_label(data$No_GEP_Other_Specify) <- "3.9 Other reasons not quoted, please specify"
data$No_GEP_Other_Specify <- factor(data$No_GEP_Other_Specify, levels = names(No_GEP_Time_labels), labels = No_GEP_Time_labels)
attr(data$No_GEP_Other_Specify, "note") <- "Other reasons not quoted, please specify?"
table(data$No_GEP_Other_Specify)

### Re-coding No_GEP_Other_Text - 3.9 Open text response for other reasons not quoted
colnames(data)[colnames(data) == "v_627"] <- "No_GEP_Other_Text"
var_label(data$No_GEP_Other_Text) <- "3.9 Open text response for other reasons not quoted"
attr(data$No_GEP_Other_Text, "note") <- "This variable captures open text responses for other reasons that were not quoted."
data$No_GEP_Other_Text[data$No_GEP_Other_Text == "-99"] <- "Not edited"
data$No_GEP_Other_Text[data$No_GEP_Other_Text == "-66"] <- "Not seen the Question"
unique(data$No_GEP_Other_Text)


### Re-coding Resp_GE_Unit - 5.1 Gender equality unit
colnames(data)[colnames(data) == "v_149"] <- "Resp_GE_Unit"
var_label(data$Resp_GE_Unit) <- "5.1 Gender equality [equality and diversity, equal opportunities…] unit (staff dedicated to gender equality)"
Resp_GE_Unit_labels <- c("0" = "No", "1" = "Yes", "-77" = "Not seen the Question")
data$Resp_GE_Unit <- factor(data$Resp_GE_Unit, levels = names(Resp_GE_Unit_labels), labels = Resp_GE_Unit_labels)
attr(data$Resp_GE_Unit, "note") <- "Gender equality [equality and diversity, equal opportunities…] unit (staff dedicated to gender equality)"
table(data$Resp_GE_Unit)

### Re-coding Resp_GE_Officer - 5.1 Gender equality officer / equal opportunity officer
colnames(data)[colnames(data) == "v_150"] <- "Resp_GE_Officer"
var_label(data$Resp_GE_Officer) <- "5.1 Gender equality officer / equal opportunity officer"
data$Resp_GE_Officer <- factor(data$Resp_GE_Officer, levels = names(Resp_GE_Unit_labels), labels = Resp_GE_Unit_labels)
attr(data$Resp_GE_Officer, "note") <- "Gender equality officer / equal opportunity officer (member of the organisation charged with supervising gender equality in the organisation)"
table(data$Resp_GE_Officer)

### Re-coding Resp_GE_Committee - 5.1 Gender equality committee
colnames(data)[colnames(data) == "v_151"] <- "Resp_GE_Committee"
var_label(data$Resp_GE_Committee) <- "5.1 Gender equality committee"
data$Resp_GE_Committee <- factor(data$Resp_GE_Committee, levels = names(Resp_GE_Unit_labels), labels = Resp_GE_Unit_labels)
attr(data$Resp_GE_Committee, "note") <- "Gender equality [equality and diversity, equal opportunities…] committee (a committee dedicated to supervising gender equality in the organisation)?"
table(data$Resp_GE_Committee)

### Re-coding Resp_GE_Departments - 5.1 Gender equality officers, staff or committees in the departments
colnames(data)[colnames(data) == "v_152"] <- "Resp_GE_Departments"
var_label(data$Resp_GE_Departments) <- "5.1 Gender equality officers, staff or committees in the departments"
data$Resp_GE_Departments <- factor(data$Resp_GE_Departments, levels = names(Resp_GE_Unit_labels), labels = Resp_GE_Unit_labels)
attr(data$Resp_GE_Departments, "note") <- "Gender equality officers, staff or committees in the departments (presence of dedicated personnel or committees for gender equality within departments)?"
table(data$Resp_GE_Departments)

### Re-coding Resp_Top_Management - 5.1 Member of the top management
colnames(data)[colnames(data) == "v_153"] <- "Resp_Top_Management"
var_label(data$Resp_Top_Management) <- "5.1 Member of the top management"
data$Resp_Top_Management <- factor(data$Resp_Top_Management, levels = names(Resp_GE_Unit_labels), labels = Resp_GE_Unit_labels)
attr(data$Resp_Top_Management, "note") <- "Member of the top management (president, vice-chancellor, CEO, head of administration, etc.) charged with gender equality responsibilities?"
table(data$Resp_Top_Management)

### Re-coding Resp_Department_Mang - 5.1 Member of the department management or administration
colnames(data)[colnames(data) == "v_154"] <- "Resp_Department_Mang"
var_label(data$Resp_Department_Mang) <- "5.1 Member of the department management or administration"
data$Resp_Department_Mang <- factor(data$Resp_Department_Mang, levels = names(Resp_GE_Unit_labels), labels = Resp_GE_Unit_labels)
attr(data$Resp_Department_Mang, "note") <- "Member of the department management or administration charged with gender equality responsibilities?"
table(data$Resp_Department_Mang)

### Re-coding Resp_Other - 5.1 Other, please specify quoted
colnames(data)[colnames(data) == "v_323"] <- "Resp_Other"
var_label(data$Resp_Other) <- "5.1 Other, please specify quoted"
data$Resp_Other <- factor(data$Resp_Other, levels = names(Resp_GE_Unit_labels), labels = Resp_GE_Unit_labels)
attr(data$Resp_Other, "note") <- "Other, please specify quoted?"
table(data$Resp_Other)

### Re-coding Resp_Specify - 5.1 Open text response for other reasons not quoted
colnames(data)[colnames(data) == "v_324"] <- "Resp_Specify"
var_label(data$Resp_Specify) <- "5.1 Open text response for other reasons not quoted"
attr(data$Resp_Specify, "note") <- "This variable captures open text responses for other reasons that were not quoted."
data$Resp_Specify[data$Resp_Specify == "-99"] <- "Not edited"
data$Resp_Specify[data$Resp_Specify == "-66"] <- "Not seen the Question"
unique(data$Resp_Specify)



### Re-coding Int_financial_res - 5.2 Internal financial resources
colnames(data)[colnames(data) == "v_156"] <- "Int_financial_res"
var_label(data$Int_financial_res) <- "5.2 Internal financial resources"
Int_financial_res_labels <- c("1" = "Yes", "2" = "No", "3" = "Not known", "4" = "No answer")
data$Int_financial_res <- factor(data$Int_financial_res, levels = names(Int_financial_res_labels), labels = Int_financial_res_labels)
attr(data$Int_financial_res, "note") <- "Does your organisation dedicate any internal financial resources to implement gender equality measures?"
table(data$Int_financial_res)

### Re-coding Assess_Budget - 5.3 Assessing the budget
colnames(data)[colnames(data) == "v_157"] <- "Assess_Budget"
var_label(data$Assess_Budget) <- "5.3 Assessing the budget"
Assess_Budget_labels <- c("1" = "Sufficient", "2" = "Almost adequate", "3" = "Hardly adequate", 
                          "4" = "Minimal", "-77" = "Not seen the question", "0" = "Not edited")
data$Assess_Budget <- factor(data$Assess_Budget, levels = names(Assess_Budget_labels), labels = Assess_Budget_labels)
attr(data$Assess_Budget, "note") <- "Please assess the relation of the dedicated budget and the planned and implemented gender equality measures."
table(data$Assess_Budget)

### Re-coding Stakeholder Involvement - 5.4
stakeholder_vars <- c("v_158" = "Stk_Manag_board", "v_159" = "Stk_Int_members", "v_160" = "Stk_Ext_members",
                      "v_161" = "Stk_Members_Assembly", "v_162" = "Stk_All_Employees", "v_171" = "Stk_Heads_Adm",
                      "v_173" = "Stk_Heads_Dep", "v_632" = "Stk_GE_Officer", "v_174" = "Stk_GE_Unit",
                      "v_175" = "Stk_GE_Committee", "v_176" = "Stk_Ministry", "v_168" = "Stk_Consultants",
                      "v_169" = "Stk_Council", "v_179" = "Stk_Other")

stakeholder_labels <- c("0" = "No", "1" = "Yes", "-77" = "Not seen the Question")

for (old_var in names(stakeholder_vars)) {
  new_var <- stakeholder_vars[old_var]
  colnames(data)[colnames(data) == old_var] <- new_var
  var_label(data[[new_var]]) <- paste("5.4", new_var)
  data[[new_var]] <- factor(data[[new_var]], levels = names(stakeholder_labels), labels = stakeholder_labels)
  attr(data[[new_var]], "note") <- paste("Which stakeholders are involved in the implementation of the Gender Equality Plan?", new_var)
  table(data[[new_var]])
}

### Re-coding Stk_Specify - 5.4 Open text response for other reasons not quoted
colnames(data)[colnames(data) == "v_180"] <- "Stk_Specify"
var_label(data$Stk_Specify) <- "5.4 Open text response for other reasons not quoted"
attr(data$Stk_Specify, "note") <- "This variable captures open text responses for other reasons that were not quoted."
data$Stk_Specify[data$Stk_Specify == "-99"] <- "Not edited"
data$Stk_Specify[data$Stk_Specify == "-66"] <- "Not seen the Question"
unique(data$Stk_Specify)
### Re-coding Capacity_Building - 5.5 Capacity Building
colnames(data)[colnames(data) == "v_181"] <- "Capacity_Building"
var_label(data$Capacity_Building) <- "5.5 Capacity Building"
Capacity_Building_labels <- c("1" = "Yes", "2" = "No", "3" = "Not known", "4" = "No answer")
data$Capacity_Building <- factor(data$Capacity_Building, levels = names(Capacity_Building_labels), labels = Capacity_Building_labels)
attr(data$Capacity_Building, "note") <- "Does your organisation provide training and capacity-building measures for gender equality (or diversity/equity)?"
table(data$Capacity_Building)

### Re-coding Monitoring_GE - 5.6 Monitoring Gender Equality
colnames(data)[colnames(data) == "v_182"] <- "Monitoring_GE"
var_label(data$Monitoring_GE) <- "5.6 Monitoring Gender Equality"
Monitoring_GE_labels <- c("1" = "Yes", "2" = "No", "3" = "Not known", "4" = "No answer")
data$Monitoring_GE <- factor(data$Monitoring_GE, levels = names(Monitoring_GE_labels), labels = Monitoring_GE_labels)
attr(data$Monitoring_GE, "note") <- "Does your organisation monitor gender equality and/or the implementation of the Gender Equality Plan?"
table(data$Monitoring_GE)



### Re-coding Collect_Student - 5.7 Students
colnames(data)[colnames(data) == "v_183"] <- "Collect_Student"
var_label(data$Collect_Student) <- "5.7 Students"
Collect_Student_labels <- c("0" = "No", "1" = "Yes", "-77" = "Not seen the Question")
data$Collect_Student <- factor(data$Collect_Student, levels = names(Collect_Student_labels), labels = Collect_Student_labels)
attr(data$Collect_Student, "note") <- "Collection of data on students involved in activities or considerations."
table(data$Collect_Student)

### Re-coding Collect_Exams_Graduates - 5.7 Exams and Graduates
colnames(data)[colnames(data) == "v_184"] <- "Collect_Exams_Graduates"
var_label(data$Collect_Exams_Graduates) <- "5.7 Exams and graduates (BA, MA and equivalent)"
data$Collect_Exams_Graduates <- factor(data$Collect_Exams_Graduates, levels = names(Collect_Student_labels), labels = Collect_Student_labels)
attr(data$Collect_Exams_Graduates, "note") <- "Collection of data on exams and graduates (BA, MA and equivalent) involved in activities or considerations."
table(data$Collect_Exams_Graduates)

### Re-coding Collect_PhDs - 5.7 PhD holders
colnames(data)[colnames(data) == "v_185"] <- "Collect_PhDs"
var_label(data$Collect_PhDs) <- "5.7 PhD holders"
data$Collect_PhDs <- factor(data$Collect_PhDs, levels = names(Collect_Student_labels), labels = Collect_Student_labels)
attr(data$Collect_PhDs, "note") <- "Collection of data on PhDs involved in activities or considerations."
table(data$Collect_PhDs)

### Re-coding Collect_Academic_Staff - 5.7 Academic or Scientific Staff
colnames(data)[colnames(data) == "v_186"] <- "Collect_Academic_Staff"
var_label(data$Collect_Academic_Staff) <- "5.7 Academic or scientific staff"
data$Collect_Academic_Staff <- factor(data$Collect_Academic_Staff, levels = names(Collect_Student_labels), labels = Collect_Student_labels)
attr(data$Collect_Academic_Staff, "note") <- "Collection of data on academic or scientific staff involved in activities or considerations."
table(data$Collect_Academic_Staff)

### Re-coding Collect_Leading_Positions - 5.7 Leading Positions
colnames(data)[colnames(data) == "v_187"] <- "Collect_Leading_Positions"
var_label(data$Collect_Leading_Positions) <- "5.7 Leading positions"
data$Collect_Leading_Positions <- factor(data$Collect_Leading_Positions, levels = names(Collect_Student_labels), labels = Collect_Student_labels)
attr(data$Collect_Leading_Positions, "note") <- "Collection of data on individuals in leading positions involved in activities or considerations."
table(data$Collect_Leading_Positions)

### Re-coding Collect_Admin_Staff - 5.7 Administrative and Technical Staff
colnames(data)[colnames(data) == "v_188"] <- "Collect_Admin_Staff"
var_label(data$Collect_Admin_Staff) <- "5.7 Administrative and technical staff"
data$Collect_Admin_Staff <- factor(data$Collect_Admin_Staff, levels = names(Collect_Student_labels), labels = Collect_Student_labels)
attr(data$Collect_Admin_Staff, "note") <- "Collection of data on administrative and technical staff involved in activities or considerations."
table(data$Collect_Admin_Staff)

### Re-coding Collect_Boards - 5.7 Boards, Councils, and Panels
colnames(data)[colnames(data) == "v_189"] <- "Collect_Boards"
var_label(data$Collect_Boards) <- "5.7 Boards, councils and panels"
data$Collect_Boards <- factor(data$Collect_Boards, levels = names(Collect_Student_labels), labels = Collect_Student_labels)
attr(data$Collect_Boards, "note") <- "Collection of data on boards, councils, and panels involved in activities or considerations."
table(data$Collect_Boards)

### Re-coding Collect_Pr_investigators - 5.7 Principal Investigators in Acquiring Funding
colnames(data)[colnames(data) == "v_190"] <- "Collect_Pr_investigators"
var_label(data$Collect_Pr_investigators) <- "5.7 Principal investigators in acquire funding"
data$Collect_Pr_investigators <- factor(data$Collect_Pr_investigators, levels = names(Collect_Student_labels), labels = Collect_Student_labels)
attr(data$Collect_Pr_investigators, "note") <- "Collection of data on Principal investigators who were successful in acquiring funding."
table(data$Collect_Pr_investigators)

### Re-coding Collect_Authors - 5.7 Authors of Scientific Publications
colnames(data)[colnames(data) == "v_191"] <- "Collect_Authors"
var_label(data$Collect_Authors) <- "5.7 Authors of Scientific publications"
data$Collect_Authors <- factor(data$Collect_Authors, levels = names(Collect_Student_labels), labels = Collect_Student_labels)
attr(data$Collect_Authors, "note") <- "Collection of data on scientific publications involved in activities or considerations."
table(data$Collect_Authors)

### Re-coding Collect_Other - 5.7 Collection - Other Specified
colnames(data)[colnames(data) == "v_192"] <- "Collect_Other"
var_label(data$Collect_Other) <- "5.7 Collection - Other"
data$Collect_Other <- factor(data$Collect_Other, levels = names(Collect_Student_labels), labels = Collect_Student_labels)
attr(data$Collect_Other, "note") <- "Collection of data on 'Other, please specify' involved in activities or considerations."
table(data$Collect_Other)

### Re-coding Collect_Specify - 5.7 Open text response for other reasons not quoted
colnames(data)[colnames(data) == "v_193"] <- "Collect_Specify"
var_label(data$Collect_Specify) <- "5.7 Open text response for other reasons not quoted"
attr(data$Collect_Specify, "note") <- "This variable captures open text responses for other reasons that were not quoted."
data$Collect_Specify <- as.character(data$Collect_Specify)
data$Collect_Specify[data$Collect_Specify == "-99"] <- "Not edited"
data$Collect_Specify[data$Collect_Specify == "-66"] <- "Not seen the Question"
#data$Collect_Specify <- factor(data$Collect_Specify)  # Convert back to factor if needed
unique(data$Collect_Specify)

### Re-coding Collect_No_Regular - 5.7 No Regular Collection of Gender/Sex-Disaggregated Data
colnames(data)[colnames(data) == "v_194"] <- "Collect_No_Regular"
var_label(data$Collect_No_Regular) <- "5.7 No regular collection of gender- or sex-disaggregated data"
data$Collect_No_Regular <- factor(data$Collect_No_Regular, levels = names(Collect_Student_labels), labels = Collect_Student_labels)
attr(data$Collect_No_Regular, "note") <- "Collection of data on the absence of regular gender- or sex-disaggregated data."
table(data$Collect_No_Regular)


### Re-coding Collect_Student - 5.7 Students
colnames(data)[colnames(data) == "v_183"] <- "Collect_Student"
var_label(data$Collect_Student) <- "5.7 Students"
Collect_Student_labels <- c("0" = "No", "1" = "Yes", "-77" = "Not seen the Question")
data$Collect_Student <- factor(data$Collect_Student, levels = names(Collect_Student_labels), labels = Collect_Student_labels)
attr(data$Collect_Student, "note") <- "Collection of data on students involved in activities or considerations."
table(data$Collect_Student)


### Re-coding Publish_Student - 5.8 Publish disaggregated data-Students
colnames(data)[colnames(data) == "v_219"] <- "Publish_Student"
var_label(data$Publish_Student) <- "5.8 Publish disaggregated data-Students"
Publish_Student_labels <- c("0" = "No", "1" = "Yes",  "-77" = "Not seen the Question")
data$Publish_Student <- factor(data$Publish_Student, levels = names(Publish_Student_labels), labels = Publish_Student_labels)
attr(data$Publish_Student, "note") <- "Publishing data on students involved in activities or considerations."
table(data$Publish_Student)


### Re-coding Publish_Exams_Graduates - 5.8 Publish disaggregated data-Exams and graduates
colnames(data)[colnames(data) == "v_220"] <- "Publish_Exams_Graduates"
var_label(data$Publish_Exams_Graduates) <- "5.8 Publish disaggregated data-Exams and graduates"
data$Publish_Exams_Graduates <- factor(data$Publish_Exams_Graduates, levels = names(Publish_Student_labels), labels = Publish_Student_labels)
attr(data$Publish_Exams_Graduates, "note") <- "Publishing data on exams and graduates (BA, MA and equivalent) involved in activities or considerations."
table(data$Publish_Exams_Graduates)

### Re-coding Publish_PhDs - 5.8 Publish disaggregated data-PhDs
colnames(data)[colnames(data) == "v_221"] <- "Publish_PhDs"
var_label(data$Publish_PhDs) <- "5.8 Publish disaggregated data-PhDs"
data$Publish_PhDs <- factor(data$Publish_PhDs, levels = names(Publish_Student_labels), labels = Publish_Student_labels)
attr(data$Publish_PhDs, "note") <- "Publishing data on PhDs involved in activities or considerations."
table(data$Publish_PhDs)

### Re-coding Publish_Academic_Staff - 5.8 Publish-Academic or scientific staff
colnames(data)[colnames(data) == "v_222"] <- "Publish_Academic_Staff"
var_label(data$Publish_Academic_Staff) <- "5.8 Publish-Academic or scientific staff"
data$Publish_Academic_Staff <- factor(data$Publish_Academic_Staff, levels = names(Publish_Student_labels), labels = Publish_Student_labels)
attr(data$Publish_Academic_Staff, "note") <- "Publishing data on academic or scientific staff involved in activities or considerations."
table(data$Publish_Academic_Staff)

### Re-coding Publish_Leading_Positions - 5.8 Publish-Leading positions
colnames(data)[colnames(data) == "v_223"] <- "Publish_Leading_Positions"
var_label(data$Publish_Leading_Positions) <- "5.8 Publish-Leading positions"
data$Publish_Leading_Positions <- factor(data$Publish_Leading_Positions, levels = names(Publish_Student_labels), labels = Publish_Student_labels)
attr(data$Publish_Leading_Positions, "note") <- "Publishing data on individuals in leading positions involved in activities or considerations."
table(data$Publish_Leading_Positions)

### Re-coding Publish_Admin_Staff - 5.8 Publish-Administrative and technical staff
colnames(data)[colnames(data) == "v_224"] <- "Publish_Admin_Staff"
var_label(data$Publish_Admin_Staff) <- "5.8 Publish-Administrative and technical staff"
data$Publish_Admin_Staff <- factor(data$Publish_Admin_Staff, levels = names(Publish_Student_labels), labels = Publish_Student_labels)
attr(data$Publish_Admin_Staff, "note") <- "Publishing data on administrative and technical staff involved in activities or considerations."
table(data$Publish_Admin_Staff)

### Re-coding Publish_Boards - 5.8 Publish-Boards, councils and panels
colnames(data)[colnames(data) == "v_225"] <- "Publish_Boards"
var_label(data$Publish_Boards) <- "5.8 Publish-Boards, councils and panels"
data$Publish_Boards <- factor(data$Publish_Boards, levels = names(Publish_Student_labels), labels = Publish_Student_labels)
attr(data$Publish_Boards, "note") <- "Publishing data on boards, councils, and panels involved in activities or considerations."
table(data$Publish_Boards)

### Re-coding Publish_Pr_investigators - 5.8 Publish-Success in acquiring funding
colnames(data)[colnames(data) == "v_226"] <- "Publish_Pr_investigators"
var_label(data$Publish_Pr_investigators) <- "5.8 Publish-Success in acquiring funding"
data$Publish_Pr_investigators <- factor(data$Publish_Pr_investigators, levels = names(Publish_Student_labels), labels = Publish_Student_labels)
attr(data$Publish_Pr_investigators, "note") <- "Publishing data on success in acquiring funding involved in activities or considerations."
table(data$Publish_Pr_investigators)

### Re-coding Publish_Authors - 5.8 Authors of scientific publications
colnames(data)[colnames(data) == "v_227"] <- "Publish_Authors"
var_label(data$Publish_Authors) <- "5.8 Authors of scientific publications"
data$Publish_Authors <- factor(data$Publish_Authors, levels = names(Publish_Student_labels), labels = Publish_Student_labels)
attr(data$Publish_Authors, "note") <- "Publish data on Principal investigators who were successful in acquiring funding."
table(data$Publish_Authors)

### Re-coding Publish_Other - 5.8 Publish - Other
colnames(data)[colnames(data) == "v_228"] <- "Publish_Other"
var_label(data$Publish_Other) <- "5.8 Publish - Other"
data$Publish_Other <- factor(data$Publish_Other, levels = names(Publish_Student_labels), labels = Publish_Student_labels)
attr(data$Publish_Other, "note") <- "Collection of data on 'Other, please specify' involved in activities or considerations."
table(data$Publish_Other)

### Re-coding Publish_Specify - 5.8 Open text response for other reasons not quoted
colnames(data)[colnames(data) == "v_229"] <- "Publish_Specify"
var_label(data$Publish_Specify) <- "5.8 Open text response for other reasons not quoted"
attr(data$Publish_Specify, "note") <- "This variable captures open text responses for other reasons that were not quoted."
data$Publish_Specify <- as.character(data$Publish_Specify)
data$Publish_Specify[data$Publish_Specify == "-99"] <- "Not edited"
data$Publish_Specify[data$Publish_Specify == "-66"] <- "Not seen the Question"
# data$Publish_Specify <- factor(data$Publish_Specify)  # Convert back to factor if needed
unique(data$Publish_Specify)

### Re-coding Publish_No_Regular - 5.8 No regular Publication of gender- or sex-disaggregated data
colnames(data)[colnames(data) == "v_230"] <- "Publish_No_Regular"
var_label(data$Publish_No_Regular) <- "5.8 No regular Publication of gender - or sex-disaggregated data"
data$Publish_No_Regular <- factor(data$Publish_No_Regular, levels = names(Publish_Student_labels), labels = Publish_Student_labels)
attr(data$Publish_No_Regular, "note") <- "Collection of data on the absence of regular gender- or sex-disaggregated data."
table(data$Publish_No_Regular)


### Re-coding Col_Ineq_Race - 5.9 Race, ethnicity, ethnic minorities
colnames(data)[colnames(data) == "v_231"] <- "Col_Ineq_Race"
var_label(data$Col_Ineq_Race) <- "5.9 Race, ethnicity, ethnic minorities"
Col_Ineq_Race_labels <- c("0" = "No", "1" = "Yes", "-77" = "Not seen the Question")
data$Col_Ineq_Race <- factor(data$Col_Ineq_Race, levels = names(Col_Ineq_Race_labels), labels = Col_Ineq_Race_labels)
attr(data$Col_Ineq_Race, "note") <- "Does your organisation collect gender- or sex-disaggregated data about the inequalities listed below? -Race, ethnicity, ethnic minorities"
table(data$Col_Ineq_Race)

### Re-coding Col_Ineq_Nat - 5.9 Nationality
colnames(data)[colnames(data) == "v_232"] <- "Col_Ineq_Nat"
var_label(data$Col_Ineq_Nat) <- "5.9 Nationality"
Col_Ineq_Nat_labels <- c("0" = "No", "1" = "Yes", "-77" = "Not seen the Question")
data$Col_Ineq_Nat <- factor(data$Col_Ineq_Nat, levels = names(Col_Ineq_Nat_labels), labels = Col_Ineq_Nat_labels)
attr(data$Col_Ineq_Nat, "note") <- "Does your organisation collect gender- or sex-disaggregated data about the inequalities listed below? - Nationality"
table(data$Col_Ineq_Nat)

### Re-coding Col_Ineq_Rel - 5.9 Religion
colnames(data)[colnames(data) == "v_233"] <- "Col_Ineq_Rel"
var_label(data$Col_Ineq_Rel) <- "5.9 Religion"
Col_Ineq_Rel_labels <- c("0" = "No", "1" = "Yes", "-77" = "Not seen the Question")
data$Col_Ineq_Rel <- factor(data$Col_Ineq_Rel, levels = names(Col_Ineq_Rel_labels), labels = Col_Ineq_Rel_labels)
attr(data$Col_Ineq_Rel, "note") <- "Does your organisation collect gender- or sex-disaggregated data about the inequalities listed below? - Religion"
table(data$Col_Ineq_Rel)

### Re-coding Col_Ineq_Class - 5.9 Class, Socioeconomic Background, First-Generation Status
colnames(data)[colnames(data) == "v_234"] <- "Col_Ineq_Class"
var_label(data$Col_Ineq_Class) <- "5.9 Class, Socioeconomic Background, First-Generation Status"
Col_Ineq_Class_labels <- c("0" = "No", "1" = "Yes", "-77" = "Not seen the Question")
data$Col_Ineq_Class <- factor(data$Col_Ineq_Class, levels = names(Col_Ineq_Class_labels), labels = Col_Ineq_Class_labels)
attr(data$Col_Ineq_Class, "note") <- "Does your organisation collect gender- or sex-disaggregated data about the inequalities listed below? - Class, Socioeconomic Background, First-Generation Students or Academics"
table(data$Col_Ineq_Class)

### Re-coding Col_Ineq_Age - 5.9 Age
colnames(data)[colnames(data) == "v_235"] <- "Col_Ineq_Age"
var_label(data$Col_Ineq_Age) <- "5.9 Age"
Col_Ineq_Age_labels <- c("0" = "No", "1" = "Yes", "-77" = "Not seen the Question")
data$Col_Ineq_Age <- factor(data$Col_Ineq_Age, levels = names(Col_Ineq_Age_labels), labels = Col_Ineq_Age_labels)
attr(data$Col_Ineq_Age, "note") <- "Does your organisation collect gender- or sex-disaggregated data about the inequalities listed below? - Age"
table(data$Col_Ineq_Age)

### Re-coding Col_Ineq_Sex - 5.9 Sexual Orientation
colnames(data)[colnames(data) == "v_236"] <- "Col_Ineq_Sex"
var_label(data$Col_Ineq_Sex) <- "5.9 Sexual Orientation"
Col_Ineq_Sex_labels <- c("0" = "No", "1" = "Yes", "-77" = "Not seen the Question")
data$Col_Ineq_Sex <- factor(data$Col_Ineq_Sex, levels = names(Col_Ineq_Sex_labels), labels = Col_Ineq_Sex_labels)
attr(data$Col_Ineq_Sex, "note") <- "Does your organisation collect gender- or sex-disaggregated data about the inequalities listed below? - Sexual Orientation"
table(data$Col_Ineq_Sex)

### Re-coding Col_Ineq_Gen_Id - 5.9 Gender Identity
colnames(data)[colnames(data) == "v_237"] <- "Col_Ineq_Gen_Id"
var_label(data$Col_Ineq_Gen_Id) <- "5.9 Gender Identity"
Col_Ineq_Gen_Id_labels <- c("0" = "No", "1" = "Yes", "-77" = "Not seen the Question")
data$Col_Ineq_Gen_Id <- factor(data$Col_Ineq_Gen_Id, levels = names(Col_Ineq_Gen_Id_labels), labels = Col_Ineq_Gen_Id_labels)
attr(data$Col_Ineq_Gen_Id, "note") <- "Does your organisation collect gender- or sex-disaggregated data about the inequalities listed below? - Gender Identity"
table(data$Col_Ineq_Gen_Id)

### Re-coding Col_Ineq_Disa - 5.9 Disability, Chronic Health, Mental Impairment
colnames(data)[colnames(data) == "v_238"] <- "Col_Ineq_Disa"
var_label(data$Col_Ineq_Disa) <- "5.9 Disability, Chronic Health, Mental Impairment"
Col_Ineq_Disa_labels <- c("0" = "No", "1" = "Yes", "-77" = "Not seen the Question")
data$Col_Ineq_Disa <- factor(data$Col_Ineq_Disa, levels = names(Col_Ineq_Disa_labels), labels = Col_Ineq_Disa_labels)
attr(data$Col_Ineq_Disa, "note") <- "Does your organisation collect gender- or sex-disaggregated data about the inequalities listed below? - Disability, Chronic Health, Mental Impairment"
table(data$Col_Ineq_Disa)

### Re-coding Col_Ineq_Care - 5.9 Care Responsibilities
colnames(data)[colnames(data) == "v_239"] <- "Col_Ineq_Care"
var_label(data$Col_Ineq_Care) <- "5.9 Care Responsibilities"
Col_Ineq_Care_labels <- c("0" = "No", "1" = "Yes", "-77" = "Not seen the Question")
data$Col_Ineq_Care <- factor(data$Col_Ineq_Care, levels = names(Col_Ineq_Care_labels), labels = Col_Ineq_Care_labels)
attr(data$Col_Ineq_Care, "note") <- "Does your organisation collect gender- or sex-disaggregated data about the inequalities listed below? - Care Responsibilities"
table(data$Col_Ineq_Care)

### Re-coding Col_Ineq_Add - 5.9 Additional Inequalities Not Listed
colnames(data)[colnames(data) == "v_240"] <- "Col_Ineq_Add"
var_label(data$Col_Ineq_Add) <- "5.9 Additional Inequalities Not Listed"
Col_Ineq_Add_labels <- c("0" = "No", "1" = "Yes", "-77" = "Not seen the Question")
data$Col_Ineq_Add <- factor(data$Col_Ineq_Add, levels = names(Col_Ineq_Add_labels), labels = Col_Ineq_Add_labels)
attr(data$Col_Ineq_Add, "note") <- "Does your organisation collect gender- or sex-disaggregated data about the inequalities listed below? - Additional Inequalities Not Listed"
table(data$Col_Ineq_Add)

### Re-coding Col_Ineq_Specify - 5.9 Open text response for other reasons not quoted
colnames(data)[colnames(data) == "v_241"] <- "Col_Ineq_Specify"
var_label(data$Col_Ineq_Specify) <- "5.9 Open text response for other reasons not quoted"
attr(data$Col_Ineq_Specify, "note") <- "This variable captures open text responses for other reasons that were not quoted."
data$Col_Ineq_Specify[data$Col_Ineq_Specify == "-99"] <- "Not edited"
data$Col_Ineq_Specify[data$Col_Ineq_Specify == "-66"] <- "Not seen the Question"
unique(data$Col_Ineq_Specify)


### Re-coding Item_Col_Women - 5.10 Women, Female
colnames(data)[colnames(data) == "v_243"] <- "Item_Col_Women"
var_label(data$Item_Col_Women) <- "5.10 Women, Female"
Item_Col_Women_labels <- c("0" = "No", "1" = "Yes", "-77" = "Not seen the Question")
data$Item_Col_Women <- factor(data$Item_Col_Women, levels = names(Item_Col_Women_labels), labels = Item_Col_Women_labels)
attr(data$Item_Col_Women, "note") <- "When collecting data on gender, which items are generally used in your organisation?-Women, Female"
table(data$Item_Col_Women)

### Re-coding Item_Col_Men - 5.10 Men, Male
colnames(data)[colnames(data) == "v_244"] <- "Item_Col_Men"
var_label(data$Item_Col_Men) <- "5.10 Men, Male"
data$Item_Col_Men <- factor(data$Item_Col_Men, levels = names(Item_Col_Women_labels), labels = Item_Col_Women_labels)
attr(data$Item_Col_Men, "note") <- "When collecting data on gender, which items are generally used in your organisation?-Men, Male"
table(data$Item_Col_Men)

### Re-coding Item_Col_Divers - 5.10 Legal term for other genders
colnames(data)[colnames(data) == "v_245"] <- "Item_Col_Divers"
var_label(data$Item_Col_Divers) <- "5.10 Legal term for other genders"
data$Item_Col_Divers <- factor(data$Item_Col_Divers, levels = names(Item_Col_Women_labels), labels = Item_Col_Women_labels)
attr(data$Item_Col_Divers, "note") <- "When collecting data on gender, which items are generally used in your organisation?-Legal term for other genders (f.ex. divers in Germany)"
table(data$Item_Col_Divers)

### Re-coding Item_Col_Non_Bin - 5.10 Non-Binary
colnames(data)[colnames(data) == "v_246"] <- "Item_Col_Non_Bin"
var_label(data$Item_Col_Non_Bin) <- "5.10 Non-Binary"
data$Item_Col_Non_Bin <- factor(data$Item_Col_Non_Bin, levels = names(Item_Col_Women_labels), labels = Item_Col_Women_labels)
attr(data$Item_Col_Non_Bin, "note") <- "When collecting data on gender, which items are generally used in your organisation?-Non-Binary"
table(data$Item_Col_Non_Bin)

### Re-coding Item_Col_Trans - 5.10 Trans* /TIN
colnames(data)[colnames(data) == "v_247"] <- "Item_Col_Trans"
var_label(data$Item_Col_Trans) <- "5.10 Trans* /TIN"
data$Item_Col_Trans <- factor(data$Item_Col_Trans, levels = names(Item_Col_Women_labels), labels = Item_Col_Women_labels)
attr(data$Item_Col_Trans, "note") <- "When collecting data on gender, which items are generally used in your organisation?-Trans* /TIN"
table(data$Item_Col_Trans)

### Re-coding Item_Col_Self - 5.10 Self-identification
colnames(data)[colnames(data) == "v_633"] <- "Item_Col_Self"
var_label(data$Item_Col_Self) <- "5.10 Self-identification"
data$Item_Col_Self <- factor(data$Item_Col_Self, levels = names(Item_Col_Women_labels), labels = Item_Col_Women_labels)
attr(data$Item_Col_Self, "note") <- "When collecting data on gender, which items are generally used in your organisation?-Opportunity for self-identification"
table(data$Item_Col_Self)

### Re-coding Item_Col_Var_No - 5.10 The variable -No answer-
colnames(data)[colnames(data) == "v_248"] <- "Item_Col_Var_No"
var_label(data$Item_Col_Var_No) <- "5.10 The variable -No answer-"
data$Item_Col_Var_No <- factor(data$Item_Col_Var_No, levels = names(Item_Col_Women_labels), labels = Item_Col_Women_labels)
attr(data$Item_Col_Var_No, "note") <- "When collecting data on gender, which items are generally used in your organisation?-The variable -No answer-"
table(data$Item_Col_Var_No)

### Re-coding Item_Col_Other - 5.10 Other
colnames(data)[colnames(data) == "v_249"] <- "Item_Col_Other"
var_label(data$Item_Col_Other) <- "5.10 Other"
data$Item_Col_Other <- factor(data$Item_Col_Other, levels = names(Item_Col_Women_labels), labels = Item_Col_Women_labels)
attr(data$Item_Col_Other, "note") <- "When collecting data on gender, which items are generally used in your organisation?-The variable -Other"
table(data$Item_Col_Other)

### Re-coding Item_Col_Specify - 5.10 Open text response for other reasons not quoted
colnames(data)[colnames(data) == "v_250"] <- "Item_Col_Specify"
var_label(data$Item_Col_Specify) <- "5.10 Open text response for other reasons not quoted"
attr(data$Item_Col_Specify, "note") <- "This variable captures open text responses for other reasons that were not quoted."
data$Item_Col_Specify[data$Item_Col_Specify == "-99"] <- "Not edited"
data$Item_Col_Specify[data$Item_Col_Specify == "-66"] <- "Not seen the Question"
unique(data$Item_Col_Specify)


### Re-coding Implemented_Fully - 5.11 Fully implemented
colnames(data)[colnames(data) == "v_251"] <- "Implemented_Fully"
var_label(data$Implemented_Fully) <- "5.11 Fully implemented"
attr(data$Implemented_Fully, "note") <- "Overall Status - Fully implemented: % of the following measures"
data$Implemented_Fully <- as.character(data$Implemented_Fully)
data$Implemented_Fully[data$Implemented_Fully == "-66"] <- "Not seen the Question"
unique(data$Implemented_Fully)

### Re-coding Implemented_Started - 5.11 Have started
colnames(data)[colnames(data) == "v_252"] <- "Implemented_Started"
var_label(data$Implemented_Started) <- "5.11 Have started"
attr(data$Implemented_Started, "note") <- "Overall Status - Have started: % of the following measures"
data$Implemented_Started <- as.character(data$Implemented_Started)
data$Implemented_Started[data$Implemented_Started == "-66"] <- "Not seen the Question"
unique(data$Implemented_Started)

### Re-coding Implemented_Not_Started - 5.11 Not started yet
colnames(data)[colnames(data) == "v_253"] <- "Implemented_Not_Started"
var_label(data$Implemented_Not_Started) <- "5.11 Not started yet"
attr(data$Implemented_Not_Started, "note") <- "Not started yet: % of the following measures"
data$Implemented_Not_Started <- as.character(data$Implemented_Not_Started)
data$Implemented_Not_Started[data$Implemented_Not_Started == "-66"] <- "Not seen the Question"
unique(data$Implemented_Not_Started)


### Re-coding Procedure_Mission_ex - 5.12 Mission statement-Exist in the organisation
colnames(data)[colnames(data) == "v_634"] <- "Procedure_Mission_ex"
var_label(data$Procedure_Mission_ex) <- "5.12 Mission statement-Exist in the organisation"
Procedure_labels <- c("0" = "No", "1" = "Yes", "-77" = "Not seen the Question")
data$Procedure_Mission_ex <- factor(data$Procedure_Mission_ex, levels = names(Procedure_labels), labels = Procedure_labels)
attr(data$Procedure_Mission_ex, "note") <- "Which documents/regulations/procedures of your organisation contain gender equality issues? - Mission statement (Exist in the organisation)"
table(data$Procedure_Mission_ex)

### Re-coding Procedure_Mission_inc - 5.12 Mission statement-Includes GE issues
colnames(data)[colnames(data) == "v_635"] <- "Procedure_Mission_inc"
var_label(data$Procedure_Mission_inc) <- "5.12 Mission statement-Includes GE issues"
data$Procedure_Mission_inc <- factor(data$Procedure_Mission_inc, levels = names(Procedure_labels), labels = Procedure_labels)
attr(data$Procedure_Mission_inc, "note") <- "Which documents/regulations/procedures of your organisation contain gender equality issues? - Mission statement (Includes gender equality issues)"
table(data$Procedure_Mission_inc)

### Re-coding Procedure_Strategy_ex - 5.12 Strategy, university development plan -Exist in the organisation
colnames(data)[colnames(data) == "v_636"] <- "Procedure_Strategy_ex"
var_label(data$Procedure_Strategy_ex) <- "5.12 Strategy, university development plan -Exist in the organisation"
data$Procedure_Strategy_ex <- factor(data$Procedure_Strategy_ex, levels = names(Procedure_labels), labels = Procedure_labels)
attr(data$Procedure_Strategy_ex, "note") <- "Which documents/regulations/procedures of your organisation include gender equality issues? - Strategy, university development plan (Exist in the organisation)"
table(data$Procedure_Strategy_ex)

### Re-coding Procedure_Strategy_inc - 5.12 Strategy, university development plan -Includes gender equality issues
colnames(data)[colnames(data) == "v_637"] <- "Procedure_Strategy_inc"
var_label(data$Procedure_Strategy_inc) <- "5.12 Strategy, university development plan -Includes gender equality issues"
data$Procedure_Strategy_inc <- factor(data$Procedure_Strategy_inc, levels = names(Procedure_labels), labels = Procedure_labels)
attr(data$Procedure_Strategy_inc, "note") <- "Which documents/regulations/procedures of your organisation include gender equality issues? - Strategy, university development plan (Includes gender equality issues)"
table(data$Procedure_Strategy_inc)

### Re-coding Procedure_Uni_Chr_ex - 5.12 University charter -Exist in the organisation
colnames(data)[colnames(data) == "v_659"] <- "Procedure_Uni_Chr_ex"
var_label(data$Procedure_Uni_Chr_ex) <- "5.12 University charter -Exist in the organisation"
data$Procedure_Uni_Chr_ex <- factor(data$Procedure_Uni_Chr_ex, levels = names(Procedure_labels), labels = Procedure_labels)
attr(data$Procedure_Uni_Chr_ex, "note") <- "Which documents/regulations/procedures of your organisation include gender equality issues? - University charter (Exist in the organisation)"
table(data$Procedure_Uni_Chr_ex)

### Re-coding Procedure_Uni_Chr_inc - 5.12 University charter -Includes gender equality issues
colnames(data)[colnames(data) == "v_660"] <- "Procedure_Uni_Chr_inc"
var_label(data$Procedure_Uni_Chr_inc) <- "5.12 University charter -Includes gender equality issues"
data$Procedure_Uni_Chr_inc <- factor(data$Procedure_Uni_Chr_inc, levels = names(Procedure_labels), labels = Procedure_labels)
attr(data$Procedure_Uni_Chr_inc, "note") <- "Which documents/regulations/procedures of your organisation include gender equality issues? - University charter (Includes gender equality issues)"
table(data$Procedure_Uni_Chr_inc)

### Re-coding Procedure_Quality_ex - 5.12 Quality assurance -Exist in the organisation
colnames(data)[colnames(data) == "v_638"] <- "Procedure_Quality_ex"
var_label(data$Procedure_Quality_ex) <- "5.12 Quality assurance -Exist in the organisation"
data$Procedure_Quality_ex <- factor(data$Procedure_Quality_ex, levels = names(Procedure_labels), labels = Procedure_labels)
attr(data$Procedure_Quality_ex, "note") <- "Which documents/regulations/procedures of your organisation include gender equality issues? - Quality assurance (Exist in the organisation)"
table(data$Procedure_Quality_ex)

### Re-coding Procedure_Quality_inc - 5.12 Quality assurance -Includes gender equality issues
colnames(data)[colnames(data) == "v_639"] <- "Procedure_Quality_inc"
var_label(data$Procedure_Quality_inc) <- "5.12 Quality assurance -Includes gender equality issues"
data$Procedure_Quality_inc <- factor(data$Procedure_Quality_inc, levels = names(Procedure_labels), labels = Procedure_labels)
attr(data$Procedure_Quality_inc, "note") <- "Which documents/regulations/procedures of your organisation include gender equality issues? - Quality assurance (Includes gender equality issues)"
table(data$Procedure_Quality_inc)

### Re-coding Procedure_Recruitment_ex - 5.12 Recruitment or appointment regulations or policies -Exist in the organisation
colnames(data)[colnames(data) == "v_640"] <- "Procedure_Recruitment_ex"
var_label(data$Procedure_Recruitment_ex) <- "5.12 Recruitment or appointment regulations or policies -Exist in the organisation"
data$Procedure_Recruitment_ex <- factor(data$Procedure_Recruitment_ex, levels = names(Procedure_labels), labels = Procedure_labels)
attr(data$Procedure_Recruitment_ex, "note") <- "Which documents/regulations/procedures of your organisation include gender equality issues? - Recruitment or appointment regulations or policies (Exist in the organisation)"
table(data$Procedure_Recruitment_ex)

### Re-coding Procedure_Recruitment_inc - 5.12 Recruitment or appointment regulations or policies -Includes gender equality issues
colnames(data)[colnames(data) == "v_641"] <- "Procedure_Recruitment_inc"
var_label(data$Procedure_Recruitment_inc) <- "5.12 Recruitment or appointment regulations or policies -Includes gender equality issues"
data$Procedure_Recruitment_inc <- factor(data$Procedure_Recruitment_inc, levels = names(Procedure_labels), labels = Procedure_labels)
attr(data$Procedure_Recruitment_inc, "note") <- "Which documents/regulations/procedures of your organisation include gender equality issues? - Recruitment or appointment regulations or policies (Includes gender equality issues)"
table(data$Procedure_Recruitment_inc)


### Re-coding Assess_Impact - 6.1 Assessing the Impact
colnames(data)[colnames(data) == "v_268"] <- "Assess_Impact"
var_label(data$Assess_Impact) <- "6.1 Assessing the Impact"
Assess_Impact_labels <- c("1" = "Yes", "2" = "No", "3" = "Not known", "4" = "No answer", "-77" = "Not seen the Question")
data$Assess_Impact <- factor(data$Assess_Impact, levels = names(Assess_Impact_labels), labels = Assess_Impact_labels)
attr(data$Assess_Impact, "note") <- "Does your organisation assess the impact (the long-term effects) of the Gender Equality Plan and/or gender equality measures?-Assessing the Impact"
table(data$Assess_Impact)

### Re-coding Name_Impact - 6.2 Name the impact
colnames(data)[colnames(data) == "v_269"] <- "Name_Impact"
var_label(data$Name_Impact) <- "6.2 Name the impact"
attr(data$Name_Impact, "note") <- "Please name keywords on how your organisation assesses the impact of the Gender Equality Plan."
data$Name_Impact <- as.character(data$Name_Impact)
data$Name_Impact[data$Name_Impact == "-99"] <- "Not edited"
data$Name_Impact[data$Name_Impact == "-66"] <- "Not seen the Question"
unique(data$Name_Impact)

### Re-coding Approach_Self - 6.3 Self Evaluation
colnames(data)[colnames(data) == "v_270"] <- "Approach_Self"
var_label(data$Approach_Self) <- "6.3 Self Evaluation"
Approach_labels <- c("0" = "No", "1" = "Yes", "-77" = "Not seen the Question")
data$Approach_Self <- factor(data$Approach_Self, levels = names(Approach_labels), labels = Approach_labels)
attr(data$Approach_Self, "note") <- "Which approaches does your organisation use to measure the impact?-Self evaluation"
table(data$Approach_Self)

### Re-coding Approach_External - 6.3 External Evaluation
colnames(data)[colnames(data) == "v_271"] <- "Approach_External"
var_label(data$Approach_External) <- "6.3 External Evaluation"
data$Approach_External <- factor(data$Approach_External, levels = names(Approach_labels), labels = Approach_labels)
attr(data$Approach_External, "note") <- "Which approaches does your organisation use to measure the impact?-External evaluation"
table(data$Approach_External)

### Re-coding Approach_Audit - 6.3 External Audit
colnames(data)[colnames(data) == "v_654"] <- "Approach_Audit"
var_label(data$Approach_Audit) <- "6.3 External Audit"
data$Approach_Audit <- factor(data$Approach_Audit, levels = names(Approach_labels), labels = Approach_labels)
attr(data$Approach_Audit, "note") <- "Which approaches does your organisation use to measure the impact?-External Audit"
table(data$Approach_Audit)

### Re-coding Approach_Other - 6.3 Other Approaches
colnames(data)[colnames(data) == "v_275"] <- "Approach_Other"
var_label(data$Approach_Other) <- "6.3 Other Approaches"
data$Approach_Other <- factor(data$Approach_Other, levels = names(Approach_labels), labels = Approach_labels)
attr(data$Approach_Other, "note") <- "Which approaches does your organisation use to measure the impact?-Other approaches"
table(data$Approach_Other)

### Re-coding Approach_Specify - 6.3 Open text response for other reasons not quoted
colnames(data)[colnames(data) == "v_276"] <- "Approach_Specify"
var_label(data$Approach_Specify) <- "6.3 Open text response for other reasons not quoted"
attr(data$Approach_Specify, "note") <- "Which approaches does your organisation use to measure the impact?-Other approaches, please specify"
data$Approach_Specify <- as.character(data$Approach_Specify)
data$Approach_Specify[data$Approach_Specify == "-99"] <- "Not edited"
data$Approach_Specify[data$Approach_Specify == "-66"] <- "Not seen the Question"
unique(data$Approach_Specify)

### Re-coding Impact_Students - 6.4 Survey on working conditions and/or organisation climate among students
colnames(data)[colnames(data) == "v_277"] <- "Impact_Students"
var_label(data$Impact_Students) <- "6.4 Survey on working conditions and/or organisation climate among students"
Impact_labels <- c("0" = "No", "1" = "Yes", "-77" = "Not seen the Question")
data$Impact_Students <- factor(data$Impact_Students, levels = names(Impact_labels), labels = Impact_labels)
attr(data$Impact_Students, "note") <- "Which data and methods does your organisation use to measure the impact?-Survey on working conditions and/or organisation climate among students"
table(data$Impact_Students)

### Re-coding Impact_Staff - 6.4 Survey on working conditions and/or organisation climate among Staff
colnames(data)[colnames(data) == "v_278"] <- "Impact_Staff"
var_label(data$Impact_Staff) <- "6.4 Survey on working conditions and/or organisation climate among Staff"
data$Impact_Staff <- factor(data$Impact_Staff, levels = names(Impact_labels), labels = Impact_labels)
attr(data$Impact_Staff, "note") <- "Which data and methods does your organisation use to measure the impact?-Survey on working conditions and/or organisation climate among Staff"
table(data$Impact_Staff)

### Re-coding Impact_Int_Stu - 6.4 Qualitative interviews or focus groups with students
colnames(data)[colnames(data) == "v_279"] <- "Impact_Int_Stu"
var_label(data$Impact_Int_Stu) <- "6.4 Qualitative interviews or focus groups with students"
data$Impact_Int_Stu <- factor(data$Impact_Int_Stu, levels = names(Impact_labels), labels = Impact_labels)
attr(data$Impact_Int_Stu, "note") <- "Which data and methods does your organisation use to measure the impact?-Qualitative interviews or focus groups with students"
table(data$Impact_Int_Stu)

### Re-coding Impact_Int_Staff - 6.4 Qualitative interviews or focus groups with academic and/or administrative staff
colnames(data)[colnames(data) == "v_280"] <- "Impact_Int_Staff"
var_label(data$Impact_Int_Staff) <- "6.4 Qualitative interviews or focus groups with academic and/or administrative staff"
data$Impact_Int_Staff <- factor(data$Impact_Int_Staff, levels = names(Impact_labels), labels = Impact_labels)
attr(data$Impact_Int_Staff, "note") <- "Which data and methods does your organisation use to measure the impact?-Qualitative interviews or focus groups with academic and/or administrative staff"
table(data$Impact_Int_Staff)

### Re-coding Impact_Gen_Data - 6.4 Data analysis of the administrative data about gender distribution among departments
colnames(data)[colnames(data) == "v_281"] <- "Impact_Gen_Data"
var_label(data$Impact_Gen_Data) <- "6.4 Data analysis of the administrative data about gender distribution among departments"
data$Impact_Gen_Data <- factor(data$Impact_Gen_Data, levels = names(Impact_labels), labels = Impact_labels)
attr(data$Impact_Gen_Data, "note") <- "Which data and methods does your organisation use to measure the impact?-Data analysis of the administrative data about gender distribution among departments"
table(data$Impact_Gen_Data)

### Re-coding Impact_Mon_Data - 6.4 Analysis of monitoring data
colnames(data)[colnames(data) == "v_282"] <- "Impact_Mon_Data"
var_label(data$Impact_Mon_Data) <- "6.4 Analysis of monitoring data"
data$Impact_Mon_Data <- factor(data$Impact_Mon_Data, levels = names(Impact_labels), labels = Impact_labels)
attr(data$Impact_Mon_Data, "note") <- "Which data and methods does your organisation use to measure the impact?-Analysis of monitoring data (e.g., mentoring, workshops, or other equality measures, complaints about sexual harassment)"
table(data$Impact_Mon_Data)

### Re-coding Impact_Other - 6.4 Other, please specify
colnames(data)[colnames(data) == "v_283"] <- "Impact_Other"
var_label(data$Impact_Other) <- "6.4 Other, please specify"
data$Impact_Other <- factor(data$Impact_Other, levels = names(Impact_labels), labels = Impact_labels)
attr(data$Impact_Other, "note") <- "Which data and methods does your organisation use to measure the impact?-Other, please specify"
table(data$Impact_Other)

### Re-coding Impact_Specify - 6.4 Open text response for other reasons not quoted
colnames(data)[colnames(data) == "v_284"] <- "Impact_Specify"
var_label(data$Impact_Specify) <- "6.4 Open text response for other reasons not quoted"
attr(data$Impact_Specify, "note") <- "This variable captures open text responses for other reasons that were not quoted."
data$Impact_Specify <- as.character(data$Impact_Specify)
data$Impact_Specify[data$Impact_Specify == "-99"] <- "Not edited"
data$Impact_Specify[data$Impact_Specify == "-66"] <- "Not seen the Question"
unique(data$Impact_Specify)

### Re-coding Impact_in_2022 - 6.5 Percentage of women in 2022
colnames(data)[colnames(data) == "v_285"] <- "Impact_in_2022"
var_label(data$Impact_in_2022) <- "6.5 Percentage of women in 2022"
attr(data$Impact_in_2022, "note") <- "Please indicate the current percentage of women in your organisation's highest research position (or position with research tasks) and that five years ago.-in 2022"

# Convert to character to replace -99, then back to numeric where possible
data$Impact_in_2022 <- as.character(data$Impact_in_2022)
data$Impact_in_2022[data$Impact_in_2022 == "-99" | is.na(data$Impact_in_2022)] <- "Not edited"
data$Impact_in_2022 <- suppressWarnings(as.numeric(data$Impact_in_2022))  # Convert back to numeric, ignoring warnings
data$Impact_in_2022[is.na(data$Impact_in_2022)] <- "Not edited"  # Ensure NAs are replaced with "Not edited"
unique(data$Impact_in_2022)

### Re-coding Impact_in_2017 - 6.5 Percentage of women in 2017
colnames(data)[colnames(data) == "v_286"] <- "Impact_in_2017"
var_label(data$Impact_in_2017) <- "6.5 Percentage of women in 2017"
attr(data$Impact_in_2017, "note") <- "Please indicate the current percentage of women in your organisation's highest research position (or position with research tasks) and that five years ago.-in 2017"

# Convert to character to replace -99, then back to numeric where possible
data$Impact_in_2017 <- as.character(data$Impact_in_2017)
data$Impact_in_2017[data$Impact_in_2017 == "-99" | is.na(data$Impact_in_2017)] <- "Not edited"
data$Impact_in_2017 <- suppressWarnings(as.numeric(data$Impact_in_2017))  # Convert back to numeric, ignoring warnings
data$Impact_in_2017[is.na(data$Impact_in_2017)] <- "Not edited"  # Ensure NAs are replaced with "Not edited"
unique(data$Impact_in_2017)

### Re-coding Impact_Position - 6.5 Description of the position for the provided data
colnames(data)[colnames(data) == "v_288"] <- "Impact_Position"
var_label(data$Impact_Position) <- "6.5 Description of the position for the provided data"
attr(data$Impact_Position, "note") <- "Please indicate the current percentage of women in your organisation's highest research position (or position with research tasks) and that five years ago. Please describe the position you provide the data (salary or other characteristics)"

# Convert to character and replace -99 and NAs
data$Impact_Position <- as.character(data$Impact_Position)
data$Impact_Position[data$Impact_Position == "-99" | is.na(data$Impact_Position)] <- "Not edited"
unique(data$Impact_Position)

### Re-coding Changes_Awareness - 6.6 Awareness of gender equality academia and science (-5 to +5)
colnames(data)[colnames(data) == "v_290"] <- "Changes_Awareness"
var_label(data$Changes_Awareness) <- "6.6 Awareness of gender equality academia and science (-5 to +5)"
attr(data$Changes_Awareness, "note") <- "Please rate the extent of changes toward gender equality in the following areas in your organisation since Awareness of gender equality academia and science"

# Convert to numeric and apply transformation
data$Changes_Awareness <- as.numeric(data$Changes_Awareness)
data$Changes_Awareness[data$Changes_Awareness == 1] <- -5
data$Changes_Awareness[data$Changes_Awareness == 2] <- -4
data$Changes_Awareness[data$Changes_Awareness == 3] <- -3
data$Changes_Awareness[data$Changes_Awareness == 4] <- -2
data$Changes_Awareness[data$Changes_Awareness == 5] <- -1
data$Changes_Awareness[data$Changes_Awareness == 6] <- 0
data$Changes_Awareness[data$Changes_Awareness == 7] <- 1
data$Changes_Awareness[data$Changes_Awareness == 8] <- 2
data$Changes_Awareness[data$Changes_Awareness == 9] <- 3
data$Changes_Awareness[data$Changes_Awareness == 10] <- 4
data$Changes_Awareness[data$Changes_Awareness == 11] <- 5

table(data$Changes_Awareness)

### Repeat for other variables

### Re-coding Changes_Balance - 6.6 Work-life balance and organisational culture (-5 to +5)
colnames(data)[colnames(data) == "v_291"] <- "Changes_Balance"
var_label(data$Changes_Balance) <- "6.6 Work-life balance and organisational culture (-5 to +5)"
attr(data$Changes_Balance, "note") <- "Please rate the extent of changes toward gender equality in the following areas in your organisation since Work-life balance and organisational culture"
data$Changes_Balance <- as.numeric(data$Changes_Balance)
data$Changes_Balance[data$Changes_Balance == 1] <- -5
data$Changes_Balance[data$Changes_Balance == 2] <- -4
data$Changes_Balance[data$Changes_Balance == 3] <- -3
data$Changes_Balance[data$Changes_Balance == 4] <- -2
data$Changes_Balance[data$Changes_Balance == 5] <- -1
data$Changes_Balance[data$Changes_Balance == 6] <- 0
data$Changes_Balance[data$Changes_Balance == 7] <- 1
data$Changes_Balance[data$Changes_Balance == 8] <- 2
data$Changes_Balance[data$Changes_Balance == 9] <- 3
data$Changes_Balance[data$Changes_Balance == 10] <- 4
data$Changes_Balance[data$Changes_Balance == 11] <- 5

table(data$Changes_Balance)

### Repeat for Changes_Leadership
colnames(data)[colnames(data) == "v_292"] <- "Changes_Leadership"
var_label(data$Changes_Leadership) <- "6.6 Gender balance in leadership and decision-making (-5 to +5)"
attr(data$Changes_Leadership, "note") <- "Please rate the extent of changes toward gender equality in the following areas in your organisation since Gender balance in leadership and decision-making"
data$Changes_Leadership <- as.numeric(data$Changes_Leadership)
data$Changes_Leadership[data$Changes_Leadership == 1] <- -5
data$Changes_Leadership[data$Changes_Leadership == 2] <- -4
data$Changes_Leadership[data$Changes_Leadership == 3] <- -3
data$Changes_Leadership[data$Changes_Leadership == 4] <- -2
data$Changes_Leadership[data$Changes_Leadership == 5] <- -1
data$Changes_Leadership[data$Changes_Leadership == 6] <- 0
data$Changes_Leadership[data$Changes_Leadership == 7] <- 1
data$Changes_Leadership[data$Changes_Leadership == 8] <- 2
data$Changes_Leadership[data$Changes_Leadership == 9] <- 3
data$Changes_Leadership[data$Changes_Leadership == 10] <- 4
data$Changes_Leadership[data$Changes_Leadership == 11] <- 5

table(data$Changes_Leadership)

### Re-coding Changes_Recruitment - 6.6 Gender equality in recruitment and career progression (-5 to +5)
colnames(data)[colnames(data) == "v_293"] <- "Changes_Recruitment"
var_label(data$Changes_Recruitment) <- "6.6 Gender equality in recruitment and career progression (-5 to +5)"
attr(data$Changes_Recruitment, "note") <- "Please rate the extent of changes toward gender equality in the following areas in your organisation since Gender equality in recruitment and career progression"

data$Changes_Recruitment <- as.numeric(data$Changes_Recruitment)
data$Changes_Recruitment[data$Changes_Recruitment == 1] <- -5
data$Changes_Recruitment[data$Changes_Recruitment == 2] <- -4
data$Changes_Recruitment[data$Changes_Recruitment == 3] <- -3
data$Changes_Recruitment[data$Changes_Recruitment == 4] <- -2
data$Changes_Recruitment[data$Changes_Recruitment == 5] <- -1
data$Changes_Recruitment[data$Changes_Recruitment == 6] <- 0
data$Changes_Recruitment[data$Changes_Recruitment == 7] <- 1
data$Changes_Recruitment[data$Changes_Recruitment == 8] <- 2
data$Changes_Recruitment[data$Changes_Recruitment == 9] <- 3
data$Changes_Recruitment[data$Changes_Recruitment == 10] <- 4
data$Changes_Recruitment[data$Changes_Recruitment == 11] <- 5

table(data$Changes_Recruitment)

### Re-coding Changes_GBV - 6.6 Gender-based violence, including sexual harassment (-5 to +5)
colnames(data)[colnames(data) == "v_294"] <- "Changes_GBV"
var_label(data$Changes_GBV) <- "6.6 Gender-based violence, including sexual harassment (-5 to +5)"
attr(data$Changes_GBV, "note") <- "Please rate the extent of changes toward gender equality in the following areas in your organisation since addressing Gender-based violence, including sexual harassment"

data$Changes_GBV <- as.numeric(data$Changes_GBV)
data$Changes_GBV[data$Changes_GBV == 1] <- -5
data$Changes_GBV[data$Changes_GBV == 2] <- -4
data$Changes_GBV[data$Changes_GBV == 3] <- -3
data$Changes_GBV[data$Changes_GBV == 4] <- -2
data$Changes_GBV[data$Changes_GBV == 5] <- -1
data$Changes_GBV[data$Changes_GBV == 6] <- 0
data$Changes_GBV[data$Changes_GBV == 7] <- 1
data$Changes_GBV[data$Changes_GBV == 8] <- 2
data$Changes_GBV[data$Changes_GBV == 9] <- 3
data$Changes_GBV[data$Changes_GBV == 10] <- 4
data$Changes_GBV[data$Changes_GBV == 11] <- 5

table(data$Changes_GBV)

### Re-coding Changes_Gen_Dim - 6.6 Integration of the gender dimension into research and teaching content (-5 to +5)
colnames(data)[colnames(data) == "v_295"] <- "Changes_Gen_Dim"
var_label(data$Changes_Gen_Dim) <- "6.6 Integration of the gender dimension into research and teaching content (-5 to +5)"
attr(data$Changes_Gen_Dim, "note") <- "Please rate the extent of changes toward gender equality in the following areas in your organisation since integrating the gender dimension into research and teaching content"

data$Changes_Gen_Dim <- as.numeric(data$Changes_Gen_Dim)
data$Changes_Gen_Dim[data$Changes_Gen_Dim == 1] <- -5
data$Changes_Gen_Dim[data$Changes_Gen_Dim == 2] <- -4
data$Changes_Gen_Dim[data$Changes_Gen_Dim == 3] <- -3
data$Changes_Gen_Dim[data$Changes_Gen_Dim == 4] <- -2
data$Changes_Gen_Dim[data$Changes_Gen_Dim == 5] <- -1
data$Changes_Gen_Dim[data$Changes_Gen_Dim == 6] <- 0
data$Changes_Gen_Dim[data$Changes_Gen_Dim == 7] <- 1
data$Changes_Gen_Dim[data$Changes_Gen_Dim == 8] <- 2
data$Changes_Gen_Dim[data$Changes_Gen_Dim == 9] <- 3
data$Changes_Gen_Dim[data$Changes_Gen_Dim == 10] <- 4
data$Changes_Gen_Dim[data$Changes_Gen_Dim == 11] <- 5

table(data$Changes_Gen_Dim)


### Re-coding Reached_Awareness - 6.7 Actions of awareness-raising and training (0 to 5)
colnames(data)[colnames(data) == "v_296"] <- "Reached_Awareness"
var_label(data$Reached_Awareness) <- "6.7 Actions of awareness-raising and training (0 to 5)"
attr(data$Reached_Awareness, "note") <- "Please rate how far your organisation has stabilised activities for gender equality in the following areas. -Actions of awareness-raising and training (e.g.workshops and training on gender bias, training for recruitment committees, booklets, films or posters)"

# Convert to numeric and apply transformation
data$Reached_Awareness <- as.numeric(data$Reached_Awareness)
data$Reached_Awareness[data$Reached_Awareness == 1] <- 0
data$Reached_Awareness[data$Reached_Awareness == 2] <- 1
data$Reached_Awareness[data$Reached_Awareness == 3] <- 2
data$Reached_Awareness[data$Reached_Awareness == 4] <- 3
data$Reached_Awareness[data$Reached_Awareness == 5] <- 4
data$Reached_Awareness[data$Reached_Awareness == 6] <- 5

table(data$Reached_Awareness)

### Re-coding Reached_Balance - 6.7 Work-life balance and organisational culture (0 to 5)
colnames(data)[colnames(data) == "v_297"] <- "Reached_Balance"
var_label(data$Reached_Balance) <- "6.7 Work-life balance and organisational culture (0 to 5)"
attr(data$Reached_Balance, "note") <- "Please rate how far your organisation has stabilised activities for gender equality in the following areas. -Work-life balance and organisational culture (e.g. childcare facilities for staff and students, dual career policy, network of fathers on the campus)"

data$Reached_Balance <- as.numeric(data$Reached_Balance)
data$Reached_Balance[data$Reached_Balance == 1] <- 0
data$Reached_Balance[data$Reached_Balance == 2] <- 1
data$Reached_Balance[data$Reached_Balance == 3] <- 2
data$Reached_Balance[data$Reached_Balance == 4] <- 3
data$Reached_Balance[data$Reached_Balance == 5] <- 4
data$Reached_Balance[data$Reached_Balance == 6] <- 5

table(data$Reached_Balance)

### Re-coding Reached_Leadership - 6.7 Gender balance in leadership and decision-making (0 to 5)
colnames(data)[colnames(data) == "v_298"] <- "Reached_Leadership"
var_label(data$Reached_Leadership) <- "6.7 Gender balance in leadership and decision-making (0 to 5)"
attr(data$Reached_Leadership, "note") <- "Please rate how far your organisation has stabilised activities for gender equality in the following areas. -Gender balance in leadership and decision-making (e.g. quota for decision-making bodies, gender-integrated leadership program)"

data$Reached_Leadership <- as.numeric(data$Reached_Leadership)
data$Reached_Leadership[data$Reached_Leadership == 1] <- 0
data$Reached_Leadership[data$Reached_Leadership == 2] <- 1
data$Reached_Leadership[data$Reached_Leadership == 3] <- 2
data$Reached_Leadership[data$Reached_Leadership == 4] <- 3
data$Reached_Leadership[data$Reached_Leadership == 5] <- 4
data$Reached_Leadership[data$Reached_Leadership == 6] <- 5

table(data$Reached_Leadership)

### Re-coding Reached_Recruitment - 6.7 Gender equality in recruitment and career progression (0 to 5)
colnames(data)[colnames(data) == "v_299"] <- "Reached_Recruitment"
var_label(data$Reached_Recruitment) <- "6.7 Gender equality in recruitment and career progression (0 to 5)"
attr(data$Reached_Recruitment, "note") <- "Please rate how far your organisation has stabilised activities for gender equality in the following areas. -Gender equality in recruitment and career progression"

data$Reached_Recruitment <- as.numeric(data$Reached_Recruitment)
data$Reached_Recruitment[data$Reached_Recruitment == 1] <- 0
data$Reached_Recruitment[data$Reached_Recruitment == 2] <- 1
data$Reached_Recruitment[data$Reached_Recruitment == 3] <- 2
data$Reached_Recruitment[data$Reached_Recruitment == 4] <- 3
data$Reached_Recruitment[data$Reached_Recruitment == 5] <- 4
data$Reached_Recruitment[data$Reached_Recruitment == 6] <- 5

table(data$Reached_Recruitment)

### Re-coding Reached_GBV - 6.7 Measures against gender-based violence, including sexual harassment (0 to 5)
colnames(data)[colnames(data) == "v_300"] <- "Reached_GBV"
var_label(data$Reached_GBV) <- "6.7 Measures against gender-based violence, including sexual harassment (0 to 5)"
attr(data$Reached_GBV, "note") <- "Please rate how far your organisation has stabilised activities for gender equality in the following areas. -Measures against gender-based violence, including sexual harassment"

data$Reached_GBV <- as.numeric(data$Reached_GBV)
data$Reached_GBV[data$Reached_GBV == 1] <- 0
data$Reached_GBV[data$Reached_GBV == 2] <- 1
data$Reached_GBV[data$Reached_GBV == 3] <- 2
data$Reached_GBV[data$Reached_GBV == 4] <- 3
data$Reached_GBV[data$Reached_GBV == 5] <- 4
data$Reached_GBV[data$Reached_GBV == 6] <- 5

table(data$Reached_GBV)


### Re-coding Reached_Gen_Dim - 6.7 Integration of the gender dimension into research and teaching content (0 to 5)
colnames(data)[colnames(data) == "v_301"] <- "Reached_Gen_Dim"
var_label(data$Reached_Gen_Dim) <- "6.7 Integration of the gender dimension into research and teaching content (0 to 5)"
attr(data$Reached_Gen_Dim, "note") <- "Please rate how far your organisation has stabilised activities for gender equality in the following areas. -Integration of the gender dimension into research and teaching content"

# Convert to numeric and apply transformation
data$Reached_Gen_Dim <- as.numeric(data$Reached_Gen_Dim)
data$Reached_Gen_Dim[data$Reached_Gen_Dim == 1] <- 0
data$Reached_Gen_Dim[data$Reached_Gen_Dim == 2] <- 1
data$Reached_Gen_Dim[data$Reached_Gen_Dim == 3] <- 2
data$Reached_Gen_Dim[data$Reached_Gen_Dim == 4] <- 3
data$Reached_Gen_Dim[data$Reached_Gen_Dim == 5] <- 4
data$Reached_Gen_Dim[data$Reached_Gen_Dim == 6] <- 5

table(data$Reached_Gen_Dim)

### Re-coding Positive_Change - 6.8 Achieved Positive_Change (0 to 5)
colnames(data)[colnames(data) == "v_302"] <- "Positive_Change"
var_label(data$Positive_Change) <- "6.8 Achieved Positive_Change (0 to 5)"
attr(data$Positive_Change, "note") <- "Please rate the relevance of the Gender Equality Plan for the achieved positive changes.- Achieved Positive_Change"

# Convert to numeric and apply transformation
data$Positive_Change <- as.character(data$Positive_Change)
data$Positive_Change[data$Positive_Change == "1"] <- "0"
data$Positive_Change[data$Positive_Change == "2"] <- "1"
data$Positive_Change[data$Positive_Change == "3"] <- "2"
data$Positive_Change[data$Positive_Change == "4"] <- "3"
data$Positive_Change[data$Positive_Change == "5"] <- "4"
data$Positive_Change[data$Positive_Change == "6"] <- "5"
data$Positive_Change[data$Positive_Change == "-77"] <- "Not seen the Question"

# Convert back to factor to retain labeled values
Positive_Change_labels <- c("0" = "No influence", "1" = "Minimal influence", "2" = "Some influence", 
                            "3" = "Moderate influence", "4" = "Considerable influence", "5" = "High influence",
                            "Not seen the Question" = "Not seen the Question")
data$Positive_Change <- factor(data$Positive_Change, levels = names(Positive_Change_labels), labels = Positive_Change_labels)

table(data$Positive_Change)

### Re-coding Low_GEP - 6.9 Open text response for other reasons not quoted
colnames(data)[colnames(data) == "v_303"] <- "Low_GEP"
var_label(data$Low_GEP) <- "6.9 Open text response for other reasons not quoted"
attr(data$Low_GEP, "note") <- "This variable captures open text responses for other reasons that were not quoted."

# Convert to character and replace values
data$Low_GEP <- as.character(data$Low_GEP)
data$Low_GEP[data$Low_GEP == "-99"] <- "Not edited"
data$Low_GEP[data$Low_GEP == "-66"] <- "Not seen the Question"

unique(data$Low_GEP)

### Re-coding Involved_Management - 7.1 Member of the top management
colnames(data)[colnames(data) == "v_304"] <- "Involved_Management"
var_label(data$Involved_Management) <- "7.1 Member of the top management (e.g. president, vice-chancellor, rectorate, executive committee)"
attr(data$Involved_Management, "note") <- "Which individuals were involved in the processing of this survey? -Member of the top management (e.g. president, vice-chancellor, rectorate, executive committee)"

Involved_Management_labels <- c("0" = "No", "1" = "Yes", "-77" = "Not seen the Question")
data$Involved_Management <- factor(data$Involved_Management, levels = names(Involved_Management_labels), labels = Involved_Management_labels)
table(data$Involved_Management)

### Re-coding Involved_Leadership - 7.1 Staff linked to the leadership
colnames(data)[colnames(data) == "v_305"] <- "Involved_Leadership"
var_label(data$Involved_Leadership) <- "7.1 Staff linked to the leadership"
attr(data$Involved_Leadership, "note") <- "Which individuals were involved in the processing of this survey? -Staff linked to the leadership"

data$Involved_Leadership <- factor(data$Involved_Leadership, levels = names(Involved_Management_labels), labels = Involved_Management_labels)
table(data$Involved_Leadership)

### Re-coding Involved_GE_Officer - 7.1 Gender equality officer
colnames(data)[colnames(data) == "v_306"] <- "Involved_GE_Officer"
var_label(data$Involved_GE_Officer) <- "7.1 Gender equality officer"
attr(data$Involved_GE_Officer, "note") <- "Which individuals were involved in the processing of this survey? -Gender equality officer"

data$Involved_GE_Officer <- factor(data$Involved_GE_Officer, levels = names(Involved_Management_labels), labels = Involved_Management_labels)
table(data$Involved_GE_Officer)

### Re-coding Involved_GE_Unit - 7.1 Gender equality unit
colnames(data)[colnames(data) == "v_307"] <- "Involved_GE_Unit"
var_label(data$Involved_GE_Unit) <- "7.1 Gender equality unit"
attr(data$Involved_GE_Unit, "note") <- "Which individuals were involved in the processing of this survey? -Gender equality unit"

data$Involved_GE_Unit <- factor(data$Involved_GE_Unit, levels = names(Involved_Management_labels), labels = Involved_Management_labels)
table(data$Involved_GE_Unit)

### Re-coding Involved_Staff - 7.1 Administration staff (e.g., human resources department)
colnames(data)[colnames(data) == "v_308"] <- "Involved_Staff"
var_label(data$Involved_Staff) <- "7.1 Administration staff (e.g., human resources department)"
attr(data$Involved_Staff, "note") <- "Which individuals were involved in the processing of this survey? -Administration staff (e.g., human resources department)"

data$Involved_Staff <- factor(data$Involved_Staff, levels = names(Involved_Management_labels), labels = Involved_Management_labels)
table(data$Involved_Staff)

### Re-coding Involved_Other - 7.1 Other, please specify
colnames(data)[colnames(data) == "v_309"] <- "Involved_Other"
var_label(data$Involved_Other) <- "7.1 Other, please specify"
attr(data$Involved_Other, "note") <- "Which individuals were involved in the processing of this survey? -Other, please specify"

data$Involved_Other <- factor(data$Involved_Other, levels = names(Involved_Management_labels), labels = Involved_Management_labels)
table(data$Involved_Other)

### Re-coding Involved_Specify - 7.1 Open text response for other reasons not quoted
colnames(data)[colnames(data) == "v_310"] <- "Involved_Specify"
var_label(data$Involved_Specify) <- "7.1 Open text response for other reasons not quoted"
attr(data$Involved_Specify, "note") <- "This variable captures open text responses for other reasons that were not quoted."

# Convert to character and replace values
data$Involved_Specify <- as.character(data$Involved_Specify)
data$Involved_Specify[data$Involved_Specify == "-99"] <- "Not edited"
data$Involved_Specify[data$Involved_Specify == "-66"] <- "Not seen the Question"

unique(data$Involved_Specify)


### Re-coding CoPs_Joining - 7.2 CoPs question
colnames(data)[colnames(data) == "v_311"] <- "CoPs_Joining"
var_label(data$CoPs_Joining) <- "7.2 CoPs question"
attr(data$CoPs_Joining, "note") <- "Would you be interested in joining a Community of Practice (CoP) for Research Funding Organisations (RFOs) or for companies (both aim to exchange knowledge on gender and innovation)?"

# Define labels
CoPs_Joining_labels <- c("1" = "Yes", "3" = "No interest", "-77" = "Not seen the Question")
data$CoPs_Joining <- factor(data$CoPs_Joining, levels = names(CoPs_Joining_labels), labels = CoPs_Joining_labels)

table(data$CoPs_Joining)

### Re-coding Further_GE_GEPs - 7.4 Further remarks on gender equality or equality plans in your organisation
colnames(data)[colnames(data) == "v_313"] <- "Further_GE_GEPs"
var_label(data$Further_GE_GEPs) <- "7.4 Further remarks on gender equality or equality plans in your organisation"
attr(data$Further_GE_GEPs, "note") <- "Further remarks on gender equality or equality plans in your organisation"

# Convert to character and replace values
data$Further_GE_GEPs <- as.character(data$Further_GE_GEPs)
data$Further_GE_GEPs[data$Further_GE_GEPs == "-99"] <- "Not edited"
data$Further_GE_GEPs[data$Further_GE_GEPs == "-66"] <- "Not seen the Question"

unique(data$Further_GE_GEPs)



# Reordering variables to place No_GEP_* after Requirements_Indicate
var_order <- c(
  names(data)[1:(which(names(data) == "Requirements_Indicate"))], 
  "No_GEP_Time", "No_GEP_Resources", "No_GEP_Acceptance", 
  "No_GEP_Necessity", "No_GEP_Other_Specify", "No_GEP_Other_Text",
  names(data)[(which(names(data) == "Requirements_Indicate") + 1):ncol(data)]
)

# Apply new order to data
data <- data[, var_order]

# Dropping unwanted columns (variables created by the survey platform, they all have missing values)
drop_columns <- c("browser", "referer", "device_type", "participant_browser", 
                  "participant_browser_version", "participant_os", "participant_device", 
                  "participant_brand", "participant_model", "participant_isbot", 
                  "participant_continent", "participant_country", "participant_region", 
                  "participant_city", "participant_latitude", "participant_longitude", 
                  "quota", "quota_assignment", "quota_rejected_id", "page_history", 
                  "hflip", "vflip", "output_mode", "javascript", "flash", "session_id", 
                  "language", "cleaned", "ats", "datetime", "date_of_last_access", 
                  "date_of_first_mail", "lfdn", "dispcode", "lastpage", "duration")

# Remove the columns from the dataset
data <- data[, !(names(data) %in% drop_columns)]




