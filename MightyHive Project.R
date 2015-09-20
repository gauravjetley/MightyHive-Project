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
write.csv(x = reservation_match, file = "reservation_match_oneres.csv")


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
write.csv(x = reservation_match, file = "abandoned_match_nodup.csv")
