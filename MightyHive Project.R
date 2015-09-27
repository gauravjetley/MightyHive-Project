abandoned = read.csv("Data/Abandoned_Data_Seed.csv",header = T,stringsAsFactors = F)
reservation = read.csv("Data/Reservation_Data_Seed.csv",header = T,stringsAsFactors = F)

nrow(abandoned)
reservation[reservation$Test_Control=='test',]
nrow(reservation[reservation$Test_Control=='test',])


#### Getting Duplicates in both Datasets
#Email only
matching_email = match(abandoned$Email,reservation$Email,nomatch = 0)
matching_email
duplicated(matching_email)
matching_email[!duplicated(matching_email)] # (76-2) = 74 obs



#Incoming Phone only
matching_incoming = match(abandoned$Incoming_Phone,reservation$Incoming_Phone,nomatch = 0)

duplicated(matching_incoming)
matching_incoming[!duplicated(matching_incoming)] # 313 obs


#Contact Phone only
matching_contact = match(abandoned$Contact_Phone,reservation$Contact_Phone,nomatch = 0)
duplicated(matching_contact)
matching_contact[!duplicated(matching_contact)] # 166 obs

#Eamil | Incoming Phone | Contact 
matching_eic = ((abandoned$Incoming_Phone %in% reservation$Incoming_Phone) 
                | (abandoned$Contact_Phone %in% reservation$Contact_Phone) 
                | abandoned$Email %in% reservation$Email)& 
                (!duplicated(matching_email) 
                | !duplicated(matching_incoming) 
                | !duplicated(matching_contact)
                )                                    # To select matching observations in Abandoned DataSet on either email, 
                                                        # incoming or Contact phone and removing any duplicates from either.
matching_eic
sum(matching_eic) 
abandoned_match = abandoned[matching_eic,]
nrow(abandoned_match) #388 obs in Abandoned matched with obs in Reservation Dataset

### Getting corresponding matching Reservation Dataset

matching_i_r = match(abandoned$Incoming_Phone, reservation$Incoming_Phone, nomatch = 0)
matching_i_r = matching_i_r[!duplicated(matching_i_r)]
matching_i_r

matching_e_r = match(abandoned$Email, reservation$Email, nomatch = 0)
matching_e_r = matching_e_r[!duplicated(matching_e_r)]
matching_e_r

matching_c_r = match(abandoned$Contact_Phone, reservation$Contact_Phone, nomatch = 0)
matching_c_r = matching_c_r[!duplicated(matching_c_r)]
matching_c_r

matching_eic_r = c(matching_i_r,matching_e_r,matching_c_r)
matching_eic_r

matching_eic_r = matching_eic_r[!duplicated(matching_eic_r)]
matching_eic_r

reservation_match <- reservation[matching_eic_r,]
reservation_match

### Writing Matched datasets

write.csv(x = reservation_match, file = "reservation_match.csv")
write.csv(x = abandoned_match, file = "abandoned_match.csv")




####Cleaning Matched Datasets


# Reservation_match Data set cleaning
# Crosschecking for any errors
sum(match(reservation_match$Email, abandoned_match$Email, nomatch = 0) | 
  match(reservation_match$Incoming_Phone, abandoned_match$Incoming_Phone, nomatch = 0) |
  match(reservation_match$Contact_Phone, abandoned_match$Contact_Phone, nomatch = 0))

sum(match(abandoned_match$Email,reservation_match$Email, nomatch = 0) | 
  match( abandoned_match$Incoming_Phone,reservation_match$Incoming_Phone, nomatch = 0) |
  match( abandoned_match$Contact_Phone, reservation_match$Contact_Phone,nomatch = 0))

# Checking for more than one reservations 
duplicated(reservation_match$Incoming_Phone, incomparables = "")
sum(duplicated(reservation_match$Incoming_Phone,incomparables = "")) # 10 obs with >1 reservation from same Incoming Phone

duplicated(reservation_match$Contact_Phone,incomparables = "")
sum(duplicated(reservation_match$Contact_Phone, incomparables = "")) # 0 obs with >1 reservation from same Contact Phone

duplicated(reservation_match$Email,incomparables = "")
sum(duplicated(reservation_match$Email, incomparables = "")) # 5 obs

# Removing >1 reservation (incoming) obs from reservation_match dataset
reservation_match_oneres = reservation_match[!duplicated(reservation_match$Incoming_Phone, incomparables = ""),]
nrow(reservation_match_oneres) #same as abandoned_match

# Checking again for duplicate emails 
duplicated(reservation_match_oneres$Email,incomparables = "")
sum(duplicated(reservation_match_oneres$Email, incomparables = ""))

# Removing >1 reservation (Email) obs from reservation_match dataset
reservation_match_oneres <- reservation_match_oneres[!duplicated(reservation_match_oneres$Email, incomparables = ""),]
nrow(reservation_match_oneres) # one less than abandoned_match

# Writing new reservation_match_oneres dataset
write.csv(x = reservation_match_oneres, file = "reservation_match_oneres.csv")


#Abandoned_match dataset cleaning
#Finding duplicates
sum(duplicated(abandoned_match$Incoming_Phone, incomparables = ""))
sum(duplicated(abandoned_match$Contact_Phone, incomparables = "")) # 1 dublicate Contact Phone
sum(duplicated(abandoned_match$Email, incomparables = ""))

# Removing duplicates
abandoned_match[abandoned_match$Contact_Phone=="(207)-726-7898",]
abandoned_match[duplicated(abandoned_match$Contact_Phone, incomparables = ""),] # making sure that 8168 is older observation of the two

abandoned_match_nodup <- abandoned_match[!duplicated(abandoned_match$Contact_Phone, incomparables = ""),]
nrow(abandoned_match_nodup) # Same as reservation_match_oneres

# Writing new abandoned_match_nodup data set
write.csv(x = abandoned_match_nodup, file = "abandoned_match_nodup.csv")



#########

# Creating variable for Bought/Not Bought in abandoned_matched_nodup:

abandoned_match_nodup[,13] <- 1  #V13 is variable for Bought(1) and NoBought(0)

# Concatenating Abandoned and Abandoned_match_nodup together in excel

abandoned_match_nodup_all <- read.csv("~/Studies/Courses/USF/Statistical Data Mining/MightyHive Project/MightyHive-Project/abandoned_match_nodup_all.csv",stringsAsFactors = FALSE)

# Removing Duplicates
sum(duplicated(abandoned_match_nodup_all$Incoming_Phone, incomparables = "")) #447
sum(duplicated(abandoned_match_nodup_all$Contact_Phone, incomparables = "")) #516
sum(duplicated(abandoned_match_nodup_all$Email, incomparables = "")) #97

abandoned_match_nodup_all_c <- abandoned_match_nodup_all[!duplicated(abandoned_match_nodup_all$Contact_Phone, incomparables = ""),]
abandoned_match_nodup_all_c <- abandoned_match_nodup_all_c[!duplicated(abandoned_match_nodup_all_c$Incoming_Phone, incomparables = ""),]
abandoned_match_nodup_all_c <- abandoned_match_nodup_all_c[!duplicated(abandoned_match_nodup_all_c$Email, incomparables = ""),]

sum(duplicated(abandoned_match_nodup_all_c)) # duplicates removed

#### Creating Dummy Variables
#Test_Control
abandoned_match_nodup_all_c$Test_Control[abandoned_match_nodup_all_c$Test_Control=="control"] <- 0 
abandoned_match_nodup_all_c$Test_Control[abandoned_match_nodup_all_c$Test_Control=="test"] <- 1 

abandoned_match_nodup_all_c$Test_Control <- as.factor(abandoned_match_nodup_all_c$Test_Control)

#Email
abandoned_match_nodup_all_c$Email[abandoned_match_nodup_all_c$Email==""] <- 0
abandoned_match_nodup_all_c$Email[abandoned_match_nodup_all_c$Email!="0"] <- 1

abandoned_match_nodup_all_c$Email <- as.factor(abandoned_match_nodup_all_c$Email)

#Address
abandoned_match_nodup_all_c$Address[abandoned_match_nodup_all_c$Address==""] <- 0
abandoned_match_nodup_all_c$Address[abandoned_match_nodup_all_c$Address!="0"] <- 1

abandoned_match_nodup_all_c$Test_Control <- as.factor(abandoned_match_nodup_all_c$Test_Control)

#Contact
abandoned_match_nodup_all_c$Contact_Phone[abandoned_match_nodup_all_c$Contact_Phone==""] <- 0
abandoned_match_nodup_all_c$Contact_Phone[abandoned_match_nodup_all_c$Contact_Phone!="0"] <- 1


#Interaction Terms


####Manuplating date time in excel

write.csv(abandoned_match_nodup_all_c ,"abandoned_match_nodup_all_c.csv")








##########################################
### SECOND TAKE AT MATCHING DATASETS ####
##########################################


#### CREATING NEW DATASET WITH (CONTACT + INCOMING) and (INCOMING + EMAIL) KEYS

#### Getting Duplicates in both Datasets

#Eamil & Incoming Phone | Contact & Incoming 
matching_eic = (((abandoned$Incoming_Phone %in% reservation$Incoming_Phone) 
                & (abandoned$Contact_Phone %in% reservation$Contact_Phone) 
                ) |  ((abandoned$Incoming_Phone %in% reservation$Incoming_Phone) 
                     & (abandoned$Email %in% reservation$Email) 
                ) )& 
                (!duplicated(matching_incoming) 
                | !duplicated(matching_contact)
                | !duplicated(matching_email) )     # To select matching observations in Abandoned DataSet on either email, 
                                      # incoming or Contact phone and removing any duplicates from either.
matching_eic
sum(matching_eic) 
abandoned_match = abandoned[matching_eic,]
nrow(abandoned_match) #340 obs in Abandoned matched with obs in Reservation Dataset

abandoned_match <- abandoned[matching_eic,]
abandoned_match

### Getting corresponding matching Reservation Dataset

matching_eic_r = ((reservation$Incoming_Phone %in% abandoned_match$Incoming_Phone) &
                 (reservation$Contact_Phone %in% abandoned_match$Contact_Phone))
                 #Only matching on incoming and contact because email is giving wrong values(matching blanks with blanks)
                 
                 
sum(matching_eic_r) #330
matching_eic_r

reservation_match <- reservation[matching_eic_r,]
reservation_match

(match(reservation_match$Incoming_Phone,abandoned_match$Incoming_Phone)&
  match(reservation_match$Contact_Phone,abandoned_match$Contact_Phone))








##########################################
##### THIRD TAKE AT MATCHING DATASETS ####
##########################################

# Created 2 keys in Excel for each data set
# Key 1: In_Co <- concatinate(incoming,contact)
# Key 2: In_Em <- concatinate(incoming,email)
# Removed all blank keys

#loading the 2 datasets with keys
abandoned = read.csv("Abandoned_with_keys.csv",header = T,stringsAsFactors = F)
reservation = read.csv("Reservation_with_keys.csv",header = T,stringsAsFactors = F)



#### Getting Matching in both Datasets
match(abandoned$In_Co,reservation$In_Co,nomatch = 0) | match(abandoned$In_Em,reservation$In_Em,nomatch = 0) #175
match(reservation$In_Co,abandoned$In_Co,nomatch = 0) | match(reservation$In_Em,abandoned$In_Em,nomatch = 0) #186

#In_Co and In_Em 
#abandoned dataset
matching_eic = ((abandoned$In_Co %in% reservation$In_Co) | (abandoned$In_Em %in% reservation$In_Em))
matching_eic
sum(matching_eic) 
abandoned_match = abandoned[matching_eic,]
nrow(abandoned_match) #175 obs in Abandoned matched with obs in Reservation Dataset
  #removing duplicates
  sum(!duplicated(abandoned_match)) #still 175 so all good
  abandoned_match_nodup <- abandoned_match

#reservation dataset
inco <- match(abandoned_match_nodup$In_Co, reservation$In_Co,nomatch = 0)
inem <- match(abandoned_match_nodup$In_Em, reservation$In_Em,nomatch = 0)
inco <- inco[inco != 0]
inco
inem <- inem[inem != 0]
inem

reservation_match <- reservation[c(inco,inem),]
nrow(reservation_match) #231
  #removing duplicates
  reservation_match_nodup <- reservation_match[!duplicated(reservation_match),]
  nrow(reservation_match_nodup) #171


### Getting corresponding matching Reservation Dataset
match(reservation_match_nodup$In_Co, abandoned_match_nodup$In_Co, nomatch = 0)
match(reservation_match_nodup$In_Em, abandoned_match_nodup$In_Em, nomatch = 0)  
indexinco <- match(abandoned_match_nodup$In_Co, reservation_match_nodup$In_Co, nomatch = 0)
indexinem <- match(abandoned_match_nodup$In_Em, reservation_match_nodup$In_Em, nomatch = 0)

abandoned_match_nodup[,15] <- indexinco
abandoned_match_nodup[,16] <- indexinem

### Writing Matched datasets

write.csv(x = reservation_match_nodup, file = "reservation_match_nodup.csv")
write.csv(x = abandoned_match_nodup, file = "abandoned_match_nodup.csv")


##### Done in Excel

#NON INTERACTION DATASET
#Creating Reservation_Index in excel in both datasets to match properly
#Sorting the datasets in excel according to Res_Index
#Converting the session variable to datetime in excel
#Subtracting Reservation session with Abandoned session to get days_in_between
#Creating days_in_between variable in excel
#subtituting session variable with days_in_between variable in the abandoned dataset
#Combining the abandoned dataset with newly created both_matched_v3 dataset
#removing duplicates with criteria (incoming and contact) first and then (email and incoming). (7325 obs left)
#replacing days_in_between variable values in abandoned dataset rows with 200. (abandoned dataset rows are above 171)
#deleting Caller_ID, Last_Name, Street, City, Zipcode variables in the both_matched_aban dataset
#Adding Customer_ID variable with index as ID from 1 to 7325
#Creating Variables D_Email (email given: 1), D_State (state given: 1), Outcome (reservation: 1), Test_Variable (test,control)
#Saving dataset as both_matched_aban_v3
#Removing Address, Email, Incoming_Phone, Contact_Phone
#Saving as both_matched_aban_v4
#deleting First_Name variable and saving dataset as both_matched_aban_v5
#Final dataset is both_matched_aban_v5 (no interactions)

#INTERACTION DATASET
#Saved in Interaction Folder
#both_matched_v3 includes matched abandoned and reservation obs with Date, Time, days_in_between, in_co, in_em
# Creating Interactions:
    #Int_T_Email: 0: no email given, time value: email given
    #Int_T_State: 0: no state given, 1: state given
#Saved as both_match_V4
#Combining abandoned with both_match_v4 and saving as both_match_aban_v1
#Populating Values for Time, Date, Days_in_Between, In_Co, In-Em, Int_T_Email, Int_T_State
#Saving as both_match_aban_v2
# Removing Duplicates using In_Co first and then In_Em (7325 obs left)
#Saving as both_match_aban_v3
# Converting both interaction variable values to "only hour" values
#Saving as both_match_aban_v4
#Created Customer_ID, D_Email, D_State, Outcome variables
#Saving as both_match_aban_v5
#Deleting Address, email variables and saving as both_match_aban_v6
#Deleting First_Name and saving as both_match_aban_v7
#Adding Interactions variables Int_T_State_bin (binary), Int_T_Email_bin (binary), Int_Test_Email (Test*D_Email), Int_Test_State(Test*D_State), Test_Var (binary)
#Readding First_Names for future analysis (Male/Female) using NLP
#Saving as both_matching_v8


### Reading Final Cleaned Datasets

Non_int_data <- read.csv("both_matched_aban_v5.csv",header = T,stringsAsFactors = F)
Int_data <- read.csv("Datasets for Interactions/both_matched_aban_v7.csv",header = T,stringsAsFactors = F)


#Q2 compute the summary statistics (mean, median, q5, q95, standard deviation) 
#of the Test_variable: a dummy with a value of 1 if tested 0 if control in the ABD database.

Test <- abandoned$Test_Control
Test[Test=="test"] <- 1
Test[Test=="control"] <- 0

mean(as.numeric(Test))
median(as.numeric(Test))
sd(as.numeric(Test))
quantile(as.numeric(Test),c(.05,.95))
hist(as.numeric(Test), main="Histogram of Test_Control Variable",labels = c("Control","","","","","","","","","Test"))


#Q3: compute the same summary statistics for this Test_variable by blocking on States 
#(meaning considering only the entries with known “State”), wherever this information is available.

Test <- abandoned$Test_Control[abandoned$Address!=""]
Test[Test=="test"] <- 1
Test[Test=="control"] <- 0
Test <- as.numeric(Test)
mean(as.numeric(Test))
median(as.numeric(Test))
sd(as.numeric(Test))
quantile(as.numeric(Test),c(.05,.95))
hist(as.numeric(Test), main="Histogram of Test_Control Variable (State-only Level)",labels = c("Control","","","","","","","","","Test"))

#Q7 1: Identification of Customers in the TREATMENT group who bought
Int_data[(Int_data$Test_Variable=="test") & (Int_data$Outcome==1), ]

#(2) Identification of Customers in the TREATMENT group who did not buy
head(Int_data[(Int_data$Test_Variable=="test") & (Int_data$Outcome!=1), ])

#(3) Identification of Customers in the Control group who bought:
head(Int_data[(Int_data$Test_Variable=="control") & (Int_data$Outcome==1), ])

#(4) Identification of Customers in the Control group who did not buy
head(Int_data[(Int_data$Test_Variable=="control") & (Int_data$Outcome!=1), ])

#Q9 Complete the following cross-tabulation:

#Group\Outcome  Buy	    No Buy
#Treatment      Number	Number
#Control	      Number	Number

nrow(Int_data[(Int_data$Test_Variable=="test") & (Int_data$Outcome==1), ])
nrow(Int_data[(Int_data$Test_Variable=="test") & (Int_data$Outcome!=1), ])
nrow(Int_data[(Int_data$Test_Variable=="control") & (Int_data$Outcome==1), ])
nrow(Int_data[(Int_data$Test_Variable=="control") & (Int_data$Outcome!=1), ])

#Group\Outcome  Buy	    No Buy
#Treatment      139   	3541
#Control	      30    	3614

library(ggplot2)                           
# Setting up the vectors                           
Outcome <- c("Buy","No Buy")
Group <- c("Control","Trearment")
# Creating data frame
df <- expand.grid(Outcome, Group)
df$value <- c(30,3614,139,3541)    
#Plotting Data
g <- ggplot(df, aes(Var1, Var2)) + geom_point(aes(size = value), colour = "green") + theme_bw() + xlab("Outcome") + ylab("Group")
g + scale_size_continuous(range=c(10,30)) + geom_text(aes(label = value))


#Q10 Repeat Q9 for 5 randomly picked states. Report 5 different tables by specifying the states you “randomly picked”.
Q10 <- read.csv("Datasets for Interactions/both_matched_aban_v5.csv",header = TRUE,stringsAsFactors = FALSE)

#LA
nrow(Q10[(Q10$Test_Variable=="test") & (Q10$Outcome==1) & (Q10$Address=="LA"), ])
nrow(Q10[(Q10$Test_Variable=="test") & (Q10$Outcome!=1) & (Q10$Address=="LA"), ])
nrow(Q10[(Q10$Test_Variable=="control") & (Q10$Outcome==1) & (Q10$Address=="LA"), ])
nrow(Q10[(Q10$Test_Variable=="control") & (Q10$Outcome!=1) & (Q10$Address=="LA"), ])

#ND
nrow(Q10[(Q10$Test_Variable=="test") & (Q10$Outcome==1) & (Q10$Address=="ND"), ])
nrow(Q10[(Q10$Test_Variable=="test") & (Q10$Outcome!=1) & (Q10$Address=="ND"), ])
nrow(Q10[(Q10$Test_Variable=="control") & (Q10$Outcome==1) & (Q10$Address=="ND"), ])
nrow(Q10[(Q10$Test_Variable=="control") & (Q10$Outcome!=1) & (Q10$Address=="ND"), ])

#MO
nrow(Q10[(Q10$Test_Variable=="test") & (Q10$Outcome==1) & (Q10$Address=="MO"), ])
nrow(Q10[(Q10$Test_Variable=="test") & (Q10$Outcome!=1) & (Q10$Address=="MO"), ])
nrow(Q10[(Q10$Test_Variable=="control") & (Q10$Outcome==1) & (Q10$Address=="MO"), ])
nrow(Q10[(Q10$Test_Variable=="control") & (Q10$Outcome!=1) & (Q10$Address=="MO"), ])

#WY
nrow(Q10[(Q10$Test_Variable=="test") & (Q10$Outcome==1) & (Q10$Address=="WY"), ])
nrow(Q10[(Q10$Test_Variable=="test") & (Q10$Outcome!=1) & (Q10$Address=="WY"), ])
nrow(Q10[(Q10$Test_Variable=="control") & (Q10$Outcome==1) & (Q10$Address=="WY"), ])
nrow(Q10[(Q10$Test_Variable=="control") & (Q10$Outcome!=1) & (Q10$Address=="WY"), ])

#OR
nrow(Q10[(Q10$Test_Variable=="test") & (Q10$Outcome==1) & (Q10$Address=="OR"), ])
nrow(Q10[(Q10$Test_Variable=="test") & (Q10$Outcome!=1) & (Q10$Address=="OR"), ])
nrow(Q10[(Q10$Test_Variable=="control") & (Q10$Outcome==1) & (Q10$Address=="OR"), ])
nrow(Q10[(Q10$Test_Variable=="control") & (Q10$Outcome!=1) & (Q10$Address=="OR"), ])


#Q11: Run a Linear regression model for Outcome = alpha + beta * Test_Variable + error

fit <- lm(Outcome~Test_Variable, data=Int_data)
fit
summary(fit)
plot(fit)

plot(Int_data_v2$Outcome~Int_data_v2$Test_Var, main="")
abline(fit)


#Q14: Now add to the regression model the dummies for State and Emails. Also consider 
#including interactions with the treatment. Report the outcome and comment on the results. 
#(You can compare with Q10)

Int_data_v2 <- read.csv("Datasets for Interactions/both_matched_aban_v8.csv",header = TRUE,stringsAsFactors = FALSE)

fit2 <- lm(Outcome~Test_Var+D_State+D_Email+Int_Test_Email+Int_Test_State,data=Int_data_v2)
fit2
summary(fit2)

#Removing D_State
fit2 <- lm(Outcome~Test_Var+D_Email+Int_Test_Email+Int_Test_State,data=Int_data_v2)
fit2
summary(fit2) #increased R^2 slightly 

#adding Int_T_Email_bin
fit2 <- lm(Outcome~Test_Var+Int_T_Email_bin+D_Email+Int_Test_Email+Int_Test_State,data=Int_data_v2)
fit2
summary(fit2) #increased R^2 

#adding Int_T_State_bin
fit2 <- lm(Outcome~Test_Var+D_Email+Int_T_State_bin+Int_Test_Email+Int_Test_State,data=Int_data_v2)
fit2
summary(fit2) #decreased R^2

#Going with addition of Int_T_Email_bin+Int_Test_Email+Int_Test_State to the model
fit2 <- lm(Outcome~Test_Var+Int_T_Email_bin+D_Email+Int_Test_Email+Int_Test_State,data=Int_data_v2)
fit2
summary(fit2)
plot(fit2)



#RQ2 You want now to investigate whether the response time (time to make a purchase after the first contact)
#is influenced by the retargeting campaign.
#Q15: Set up an appropriate linear regression model to address the RQ2 above. Make sure to select the 
#appropriate subset of customers.Report output analysis with your interpretation. Can the coefficients 
#be interpreted as causal in this case?

fit3 <- lm(Days_in_Between~Test_Variable,data=Int_data_v2[Int_data_v2$Days_in_Between!=200,])
summary(fit3)
plot(fit3)

