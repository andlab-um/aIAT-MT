
data=mouse4;
%%transform the conditions into numeric stimulus
for sub=1:26
    
    for trial=1:200
        if data{(sub-1)*200+trial,3}=='ev1'
            data{(sub-1)*200+trial,3}=Con(sub,1);
            
        end
        if data{(sub-1)*200+trial,3}=='ev2'
            data{(sub-1)*200+trial,3}=Con(sub,2);
            
        end
        if data{(sub-1)*200+trial,3}=='real'
            data{(sub-1)*200+trial,3}=Con(sub,3);
            
        end
        if data{(sub-1)*200+trial,3}=='unreal'
            data{(sub-1)*200+trial,3}=Con(sub,4);
            
        end
            
    end
end

data7=cell2mat(data);


data=mouse7;
%%transform the conditions into numeric stimulus
for sub=1:26
   
    for trial=1:200
        if data{(sub-1)*200+trial,3}=='ev1'
            data{(sub-1)*200+trial,3}=Inc(sub,1);
            
        end
        if data{(sub-1)*200+trial,3}=='ev2'
            data{(sub-1)*200+trial,3}=Inc(sub,2);
            
        end
        if data{(sub-1)*200+trial,3}=='real'
            data{(sub-1)*200+trial,3}=Inc(sub,3);
            
        end
        if data{(sub-1)*200+trial,3}=='unreal'
            data{(sub-1)*200+trial,3}=Inc(sub,4);
            
        end
            
    end
end
data4=cell2mat(data);



    