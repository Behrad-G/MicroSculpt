# Set working directory
setwd("./3D Sphere Code/")

# Define file paths
Address <- "./3D Image Sources/F42A/Raw_F42A_111"
Full_Address <- paste(Address, "_Radii.RData", sep = "")

# Load radii data
load(Full_Address)

# Set threshold
Threshold <- 51.5 # microns

# Get dimensions of the image
DimensionOfImage <- dim(Radii)
X_Length <- DimensionOfImage[1]
Y_Length <- DimensionOfImage[2]
Z_Length <- DimensionOfImage[3]

# Initialize arrays
AirImbibition <- array(rep(0, X_Length * Y_Length * Z_Length), dim = c(X_Length, Y_Length, Z_Length))
Freezing <- array(rep(0, X_Length * Y_Length * Z_Length), dim = c(X_Length, Y_Length, Z_Length))

# Identify air imbibition voxels
for (i in 1:1) {
  for (j in 1:Y_Length) {
    for (k in 1:Z_Length) {
      if (Radii[i, j, k] > Threshold) {
        AirImbibition[i, j, k] = 1
      }
    }
  }
}

# Check if there's air imbibition at the starting point
if (sum(AirImbibition[1, , ]) == 0) {
  stop("No air imbibition at the starting point.")
}

# Initialize variables
StopCode <- 0
TotalImbibedVoxelOld <- sum(AirImbibition)

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

for (i in 1:X_Length){
  for (j in 1:Y_Length){
    for (k in 1:Z_Length){
      if (Radii[i,j,k]>Threshold){
        Freezing[i,j,k]=1
      }
    }
  }
}

# Summarize results
library(plot3D)
paste(sum(AirImbibition), sum(Freezing))

# Save results to CSV
csvAdress=paste(Address,".csv",sep="")

Result <- array(rep(0, 3), dim=c(1, 3, 1))
Result[1]=Threshold
Result[2]=sum(AirImbibition)
Result[3]=sum(Freezing)

write.table(Result,csvAdress,append=TRUE,col.names=FALSE)

# Save AirImbibition and Freezing arrays
Full_Address2=paste(Address,"_Freezing_",Threshold,"um.RData", sep="")
save(Freezing, file=Full_Address2)

Full_Address3=paste(Address,"_Drainage_",Threshold,"um.RData", sep="")
save(AirImbibition, file=Full_Address3)