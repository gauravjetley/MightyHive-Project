abandoned = read.csv("Data/Abandoned_Data_Seed.csv",header = T,stringsAsFactors = F)
reservation = read.csv("Data/Reservation_Data_Seed.csv",header = T,stringsAsFactors = F)

nrow(abandoned)
reservation[reservation$Test_Control=='test',]
nrow(reservation[reservation$Test_Control=='test',])


#### Getting Duplicates in both Datasets
#Email only
matching_email = match(abandoned$Email,reservation$Email,nomatch = 0)

matching_email[!duplicated(matching_email)] # 76 obs


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
                | abandoned$Email %in% reservation$Email) 
                & 
                  (!duplicated(matching_email) 
                   | !duplicated(matching_incoming) 
                   | !duplicated(matching_contact)
                   )                                    # To select matching observations in Abandoned DataSet on either email, 
                                                        # incoming or Contact phone and removing any duplicates from either.
sum(matching_eic) 
matching_data = abandoned[matching_eic,]
nrow(matching_data) #388 obs in Abandoned matched with obs in Reservation Dataset

#Cleaning Matched Dataset

