setwd("D:/Freeze-Thaw Problem/3D Sphere Code/")

Address="D:/Freeze-Thaw Problem/3D Image Sources/F42A/Raw_F42A_111"

Full_Address=paste(Address,"_Radii.RData",sep="")

load(Full_Address)

#Radii=Radii/cos(30*pi/180)

Threshold=51.5 #micron

DimensionOfImage=dim(Radii)
X_Length=DimensionOfImage[1]
Y_Length=DimensionOfImage[2]
Z_Length=DimensionOfImage[3]

AirImbibition <- array(rep(0, X_Length*Y_Length*Z_Length), dim=c(X_Length, Y_Length, Z_Length))
Freezing <- array(rep(0, X_Length*Y_Length*Z_Length), dim=c(X_Length, Y_Length, Z_Length))

for (i in 1:1){
  for (j in 1:Y_Length){
    
    for (k in 1:Z_Length){
      if (Radii[i,j,k]>Threshold){
        
        AirImbibition[i,j,k]=1
      }
      
      
    }
  }
  
}

if (sum(AirImbibition[1,,])==0){
  stop()
}

StopCode=0

TotalImbibedVoxelOld=sum(AirImbibition)

while (StopCode==0){
  


for (i in 2:X_Length){ 
  for (j in 1:Y_Length){
    for (k in 1:Z_Length){
      if (Radii[i,j,k]>Threshold  && AirImbibition[i-1,j,k]==1){  #this sweeps in x forward
        AirImbibition[i,j,k]=1
      }

    }
  }
  for (j in 2:Y_Length){
    for (k in 1:Z_Length){
      if (Radii[i,j,k]>Threshold  && AirImbibition[i,j-1,k]==1){ #This sweeps in j backward
        AirImbibition[i,j,k]=1
      }
      
    }
  }
  for (j in 1:(Y_Length-1)){
    for (k in 1:Z_Length){
      if (Radii[i,j,k]>Threshold  && AirImbibition[i,j+1,k]==1){ #this sweeps in j forward
        AirImbibition[i,j,k]=1
      }
      
    }
  }
  
  for (j in 1:Y_Length){
    for (k in 2:Z_Length){
      if (Radii[i,j,k]>Threshold  && AirImbibition[i,j,k-1]==1){  #this sweeps in K backward
        AirImbibition[i,j,k]=1
      }
      
    }
  } 
  for (j in 1:Y_Length){
    for (k in 1:(Z_Length-1)){
      if (Radii[i,j,k]>Threshold  && AirImbibition[i,j,k+1]==1){  #this sweeps in K forward
        AirImbibition[i,j,k]=1
      }
      
    }
  }   
  
  if (sum(AirImbibition[i,,])==0){  #this calculates the front of the air imbibition
    break
  }
  
}


#_________________________________________________________________
frontVoxel=i-1

for (i in frontVoxel:2){ 
  for (j in 1:Y_Length){
    for (k in 1:Z_Length){
      if (Radii[i,j,k]>Threshold  && AirImbibition[i+1,j,k]==1){  #this sweeps in x forward
        AirImbibition[i,j,k]=1
      }
      
    }
  }
  for (j in 2:Y_Length){
    for (k in 1:Z_Length){
      if (Radii[i,j,k]>Threshold  && AirImbibition[i,j-1,k]==1){ #This sweeps in j backward
        AirImbibition[i,j,k]=1
      }
      
    }
  }
  for (j in 1:(Y_Length-1)){
    for (k in 1:Z_Length){
      if (Radii[i,j,k]>Threshold  && AirImbibition[i,j+1,k]==1){ #this sweeps in j forward
        AirImbibition[i,j,k]=1
      }
      
    }
  }
  
  for (j in 1:Y_Length){
    for (k in 2:Z_Length){
      if (Radii[i,j,k]>Threshold  && AirImbibition[i,j,k-1]==1){  #this sweeps in K backward
        AirImbibition[i,j,k]=1
      }
      
    }
  } 
  for (j in 1:Y_Length){
    for (k in 1:(Z_Length-1)){
      if (Radii[i,j,k]>Threshold  && AirImbibition[i,j,k+1]==1){  #this sweeps in K forward
        AirImbibition[i,j,k]=1
      }
      
    }
  }   

}



TotalImbibedVoxelNew=sum(AirImbibition)
print(TotalImbibedVoxelNew)

if (TotalImbibedVoxelNew==TotalImbibedVoxelOld){
  StopCode=1
}

TotalImbibedVoxelOld=TotalImbibedVoxelNew

}

#______________________________________________________________


for (i in 1:X_Length){
  for (j in 1:Y_Length){
    for (k in 1:Z_Length){
      if (Radii[i,j,k]>Threshold){
        Freezing[i,j,k]=1
      }
    }
  }
}



library(plot3D)

 # DrainageFileName=paste(Address,"_AirImbibition_",Threshold,"um.png", sep="")
 # png(DrainageFileName,width=3.25,height=3.25,units="in",res=1200)
 # isosurf3D(1:X_Length,1:X_Length,1:X_Length,AirImbibition)
 # dev.off()
 
 #DrainageFileName=paste(Address,"_Freezing_",Threshold,"um.png", sep="")
 #png(DrainageFileName,width=3.25,height=3.25,units="in",res=1200)
 #isosurf3D(1:X_Length,1:X_Length,1:X_Length,Freezing)
 #dev.off()


paste( sum(AirImbibition), sum(Freezing))

csvAdress=paste(Address,".csv",sep="")

Result <- array(rep(0, 3), dim=c(1, 3, 1))
Result[1]=Threshold
Result[2]=sum(AirImbibition)
Result[3]=sum(Freezing)

write.table(Result,csvAdress,append=TRUE,col.names=FALSE)


Full_Address2=paste(Address,"_Freezing_",Threshold,"um.RData", sep="")
save(Freezing, file=Full_Address2)

Full_Address3=paste(Address,"_Drainage_",Threshold,"um.RData", sep="")
save(AirImbibition, file=Full_Address3)


