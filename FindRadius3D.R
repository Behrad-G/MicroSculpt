#function [ ElementsWithinR, stop_code ] = FindRadius3D(CentreX,CentreY,CentreZ,InvestigationMatrix,counter, spacing,RadiusOfInvestigation )

FindRadius3D <- function(CentreX,CentreY,CentreZ,InvestigationMatrix,counter, spacing,RadiusOfInvestigation ){

  
#  [sizeX,sizeY,sizeZ]=size(InvestigationMatrix);
  
  SizeOfInvestigationMatrix=dim(InvestigationMatrix)
  sizeX=SizeOfInvestigationMatrix[1]
  sizeY=SizeOfInvestigationMatrix[2]
  sizeZ=SizeOfInvestigationMatrix[3]

  
  #ElementsWithinR=zeros(sizeX,sizeY,sizeZ);
  ElementsWithinR <- array(rep(0, sizeX*sizeY*sizeZ), dim=c(sizeX, sizeY, sizeZ))  

  stop_code=0 #it is default stop code. This will return code of zero if the sphere does not hit the wall in this step

for (ii in 1:sizeX){
    for (jj in 1:sizeY){
        for (kk in 1:sizeZ){
          distance=sqrt((CentreX-ii)^2 + (CentreY-jj)^2 + (CentreZ-kk)^2)*spacing;
            if (distance<=RadiusOfInvestigation){
                ElementsWithinR[ii,jj,kk]=1
            } #End if
        } #End for kk
    } # End for jj
} # End for ii

flagg=0
flaggz=0

for (ii in 1:sizeX){
    for (jj in 1:sizeY){
        for (kk in 1:sizeZ){
            if  (ElementsWithinR[ii,jj,kk]==1){  #here the aim is to see if any voxels that is within searching sphere is a grain
                if (InvestigationMatrix[ii,jj,kk]==1){ #if so, then the stop code would be 1, and then loops were break and function would exit
                     stop_code=1  
                     flagg=1
                     flaggz=1
                     break
                } #End if
            } #End if
            
        } # End for kk
        if (flagg==1){
            break
        } #End if
        
    } # End for jj
    
    if (flaggz==1){
        break
    } #End if
} #End for ii

ReturnValue0=list("matrix"=ElementsWithinR, "StopCode"=stop_code)
return(ReturnValue0) 

} #End of function



