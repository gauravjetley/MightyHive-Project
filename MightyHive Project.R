abandoned = read.csv("Data/Abandoned_Data_Seed.csv",header = T,stringsAsFactors = F)
reservation = read.csv("Data/Reservation_Data_Seed.csv",header = T,stringsAsFactors = F)

nrow(abandoned)
reservation[reservation$Test_Control=='test',]
nrow(reservation[reservation$Test_Control=='test',])


#### Getting Duplicates in both Datasets
#Email only
matching = match(abandoned$Email,reservation$Email,nomatch = 0)
matching[matching==92] = 0
matching[matching > 0] # 75 obs

#Incoming Phone only
matching = match(abandoned$Incoming_Phone,reservation$Incoming_Phone,nomatch = 0)
matching
matching[matching==1329] = 0
matching[matching > 0] # 327 obs

#Contact Phone only
matching = match(abandoned$Contact_Phone,reservation$Contact_Phone,nomatch = 0)
matching
matching[matching==1] = 0
matching[matching > 0] # 185 obs

