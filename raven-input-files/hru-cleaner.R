######################################################################################################
##
## This script cleans HRUs generated in "rvh-filegenerator.R"
##
######################################################################################################


##################################################################################################
##
## Read *.rvh file back in to generate subbasin network plot & cofirm correct structure
##
##################################################################################################
require(RavenR)

HRUs <- rvh.read("/var/obwb-hydro-modelling/simulations/test/test-zero.rvh")

clean <- rvh.cleanHRUs(HRUs$HRUtable, HRUs$SBtable, merge = TRUE, area_tol = 0.0001, elev_tol = 50, slope_tol = 5, aspect_tol = 45)

rvh.overwrite("cleantest.rvh", "test.rvh", HRUs$SBtable, clean$HRUtable)

plot(HRUs$SBnetwork)


HRUtab <- HRUs$HRUtable

SBtab <- HRUs$SBtable


rvh.cleanHRUs<-function(HRUtab,SBtab,area_tol=0,merge=FALSE,elev_tol=50,slope_tol=4,aspect_tol=20,ProtectedHRUList=c())
{
  #routine:
  init_nHRUs<-nrow(HRUtab)
  init_Area<-sum(HRUtab$Area)
  rem1<-0
  rem2<-0
  
  # remove overly small HRUs (contributing area less than area_tol% of subbasin)
  #-----------------------------------------------------------
  
  # get fraction of SB area in each HRU
  #areavec<-base::merge(HRUtab["SBID"],SBtab[c("SBID","Area")],by="SBID")$Area
  SBtab$AreaSB <- SBtab$Area
  
  HRUtab <- base::merge(HRUtab,SBtab[c("SBID","AreaSB")],by="SBID")
  HRUtab$areafrac<-HRUtab$Area/HRUtab$AreaSB
  
  # re-sort HRU dataframe by SBID then by area frac
  HRUtab<-HRUtab[with(HRUtab, order(SBID, areafrac)),]
  
  # tag to remove if area fraction less than tolerance %
  HRUtab$remove<-(HRUtab$areafrac<area_tol) & !(HRUtab$ID %in% ProtectedHRUList) # (&& HRUtab$cumareafrac<0.25)
  
  # calculate area correction factors for all HRUs
  A<-stats::aggregate(areafrac ~ SBID, FUN = sum, data=HRUtab[HRUtab$remove==FALSE, ])
  rownames(A)<-A$SBID
  B<-base::merge(HRUtab["SBID"],A[c("SBID","areafrac")],by="SBID")$areafrac
  
  # unremove HRUs which were eradicating an entire SB
  B=ifelse(B==0,1.0,B)
  HRUtab$remove<-ifelse(B==1.0,B!=1.0,HRUtab$remove)
  
  HRUtab$newarea<-HRUtab$Area / B *as.numeric(!HRUtab$remove)
  
  rem1<-sum(HRUtab$remove==TRUE)
  area1<-sum(HRUtab[HRUtab$remove==TRUE,]$Area)
  
  # merge overly similar HRUs (varying minutely in elevation/slope/aspect)
  #-----------------------------------------------------------
  if (merge==TRUE){
    for (i in 1:nrow(HRUtab))
    {
      if (i %% 100==0){print(i)}
      for (k in 1:max(i-1,1)){
        if (HRUtab$SBID[i]==HRUtab$SBID[k]){ # kept separate for speed
          if (HRUtab$LandUse[i]==HRUtab$LandUse[k])  {
            if ((HRUtab$remove[i]!=TRUE) & (HRUtab$remove[k]!=TRUE)){
              if  ((HRUtab$Vegetation[i]==HRUtab$Vegetation[k]) &
                   (HRUtab$SoilProfile[i]==HRUtab$SoilProfile[k]) &
                   (HRUtab$Terrain[i]==HRUtab$Terrain[k]) &
                   (HRUtab$Aquifer[i]==HRUtab$Aquifer[k]) &
                   !(HRUtab$ID[i] %in% ProtectedHRUList) &
                   !(HRUtab$ID[k] %in% ProtectedHRUList) &
                   (abs(HRUtab$Elevation[i]-HRUtab$Elevation[k])<elev_tol) &
                   (abs(HRUtab$Slope[i]-HRUtab$Slope[k])<slope_tol) &
                   ((abs(HRUtab$Aspect[i]-HRUtab$Aspect[k])<aspect_tol) |
                    (abs(HRUtab$Aspect[i]-HRUtab$Aspect[k]-360)<aspect_tol) |
                    (abs(HRUtab$Aspect[i]-HRUtab$Aspect[k]+360)<aspect_tol))
              )
              {
                HRUtab$similar[i]<-HRUtab$ID[k]
                HRUtab$remove[i]<-TRUE
                Ak<-HRUtab$newarea[k]
                Ai<-HRUtab$newarea[i]
                
                HRUtab$Latitude[k]<-(HRUtab$Latitude[k]*Ak+HRUtab$Latitude[i]*Ai)/(Ai+Ak)
                HRUtab$Longitude[k]<-(HRUtab$Longitude[k]*Ak+HRUtab$Longitude[i]*Ai)/(Ai+Ak)
                HRUtab$Slope[k]<-(HRUtab$Slope[k]*Ak+HRUtab$Slope[i]*Ai)/(Ai+Ak)
                # HRUtab$Aspect[k]<-(HRUtab$Aspect[k]*Ak+HRUtab$Aspect[i]*Ai)/(Ai+Ak)
                
                HRUtab$newarea[k]<-Ai+Ak
                HRUtab$newarea[i]<-0.0
                rem2<-rem2+1
              }
              else{
                HRUtab$similar[i]<-0
              }
            }
          }
        }
      }
    }
  }
  
  # finalize changes
  #-----------------------------------------------------------
  # apply changed areas
  HRUtab$Area<-HRUtab$newarea
  # delete removed HRUs
  HRUtab<-HRUtab[ HRUtab$remove==FALSE, ]
  # remove temporary columns
  HRUtab<-HRUtab[ , !(names(HRUtab) %in% c("areafrac","newarea","remove","similar"))]
  # return to order by HRUID
  HRUtab<-HRUtab[with(HRUtab, order(ID)),]
  
  #report results
  #-----------------------------------------------------------
  print(paste0("HRU table Cleaned. #HRUs reduced from ",toString(init_nHRUs)," to ",toString(nrow(HRUtab)) ))
  print(paste0(toString(rem1)," HRUs failed area tolerance test (",toString(area1)," km2 (",toString(round(area1/init_Area*100,1)),"%) recategorized)"))
  if (merge==TRUE){
    print(paste0(toString(rem2)," HRUs failed similarity tolerance test"))
  }
  print(paste0("Initial area: ",toString(init_Area)," km2;  final area: ",toString(sum(HRUtab$Area)) ," km2"))
  
  return (list(HRUtable=HRUtab)) # should also return re-classification information (e.g., similarity list for plotting results)
}
