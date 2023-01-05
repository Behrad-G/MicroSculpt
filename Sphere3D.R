#setwd("D:/Freeze-Thaw Problem/3D Sphere Code/Sphere code3D/Sphere code3D")

cat("\014")  #clc
rm(list=ls()) #clear;
#tic() ;
source('FindRadius3D.R')

#load(F42A1_Binary_3D_Image)
InitialTime=Sys.time()

load('CT111.RData')

#Raw=RawWithFreezing;

Raw=CT


#(X_Length,Y_Length,Z_Length)=dim(Raw);
DimensionOfImage=dim(Raw)
X_Length=DimensionOfImage[1]
Y_Length=DimensionOfImage[2]
Z_Length=DimensionOfImage[3]


spacing=0.005; #micron = 5nm
#Radii=zeros(X_Length,Y_Length,Z_Length);
Radii <- array(rep(0, X_Length*Y_Length*Z_Length), dim=c(X_Length, Y_Length, Z_Length))

for (i in 1:X_Length){
    for (j in 1:Y_Length){
        for (k in 1:Z_Length){
            if (Raw[i,j,k]==0){  #start searching only if the voxel is void (and not grain)
                counter=1;
                stop_code=0;
                while (stop_code==0){
                    # clear ElementsWithinR;
                    RadiusOfInvestigation=counter*spacing;
                    LowerX=max(1,i-ceiling(counter)); #These stop the sphere of investigation from going out of the image boundaries
                    LowerY=max(1,j-ceiling(counter)); #which would result in error
                    LowerZ=max(1,k-ceiling(counter)); #which would result in error
                    UpperX=min(X_Length,i+ceiling(counter));
                    UpperY=min(Y_Length,j+ceiling(counter));
                    UpperZ=min(Z_Length,k+ceiling(counter));                    
#                    [ElementsWithinR,stop_code]=FindRadius3D(i-LowerX+1,j-LowerY+1,k-LowerZ+1,Raw(LowerX:UpperX,LowerY:UpperY,LowerZ:UpperZ),counter, spacing,RadiusOfInvestigation);
                    FindRadiusReturn<-FindRadius3D(i-LowerX+1,j-LowerY+1,k-LowerZ+1,Raw[LowerX:UpperX,LowerY:UpperY,LowerZ:UpperZ],counter, spacing,RadiusOfInvestigation);
                    ElementsWithinR=FindRadiusReturn$matrix
                    stop_code=FindRadiusReturn$StopCode
                    counter=counter+0.2;
                } #End while
                
                for (iii in 1:(1+UpperX-LowerX)){
                    for (jjj in 1:(1+UpperY-LowerY)){
                        for (kkk in 1:(1+UpperZ-LowerZ)){
                             if ((ElementsWithinR[iii,jjj,kkk]==1) && (Raw[LowerX-1+iii,LowerY-1+jjj,LowerZ-1+kkk]==0)){
                                 Radii[LowerX-1+iii,LowerY-1+jjj,LowerZ-1+kkk]=max(RadiusOfInvestigation,Radii[LowerX-1+iii,LowerY-1+jjj,LowerZ-1+kkk]);
                             } #End if
                        
                        } #End for KKK
                    }#End for jjj
                }#End for iii
            
            
            } #End if
        }
    }
    
    print(i);
    CurrentTime=Sys.time()
    print(CurrentTime-InitialTime)
    
}

save(Radii, file = "Synthetic_CT_R.RData")

#toc();


# maxR=max(max(max(Radii)));
# increments=0:maxR/20:maxR;
# contour(Radii,increments);
# 
# set(gca,'Ydir','reverse')
# daspect([1 1 1])
# % 
# % ice=Raw;
# % for i=1:X_Length
# %     for j=1:Y_Length
# %         if Radii(i,j)>250
# %             ice(i,j)=2;
# %         end
# %     end
# % end
# % 
# % figure
# % imagesc(ice);
# % daspect([1 1 1])


#[RadiiXsize,RadiiYsize,RadiiZsize]=size(Radii);

#for j=1:Y_Length
 #   for k=1:Z_Length
 #       aaa(j,k)=Radii(100,j,k);
 #   end
#end
#figure
#imagesc(aaa);

#[dd,ff]=hist(Radii,2000);
#for i=1:2000
 #   ddd(i,1)=sum(dd(i,:));
#end

#save Radii Radii Raw