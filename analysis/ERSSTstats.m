%ERSSTstats
%compute the trendstats for each ERSST pixel
load ERSST_annual_ocean SSTanFOA yr

N=[15,20,30,50];
NY=length(yr);
for n=1:length(N);
    Es(n).Mn=nans(8993,NY);
    Es(n).SD=Es(n).Mn;
    Es(n).R=Es(n).Mn;
    
    for p=N(n)+1:NY;
        J=(1:N(n))+(p-N(n)-1);
        Es(n).Mn(:,p)=nanmean(SSTanFOA(:,J),2);
        Es(n).SD(:,p)=nanstd(SSTanFOA(:,J),1,2);
        if(mod(p,10)==0)
            fprintf('%2d\t%3d\n',N(n),p);
        end
        for q=1:8993;
            coef=[J(:),ones(N(n),1)]\SSTanFOA(q,J)';
            Es(n).R(q,p)=coef(1);
            
            
            Es(n).gamma(q,:)=SDwithtrend_inv(Es(n).R(q,:),N(n),Es(n).SD(q,:));
            [Phw,t1,t2,t3]=Psurprise(Es(n).R(q,:)', N(n), Es(n).gamma(q,:)', 0.977);
            Es(n).Phw(q,:)=Phw';
        end
    end;
end

n=find(N==30);
HW30=heatwaveagainstshift(reshape(SSTanFOA,8993,1,NY),30,2,reshape(Es(n).Mn,8993,1,NY), reshape(Es(n).SD,8993,1,NY));
HW30=squeeze(HW30);

save ERSSTtrendstats Es N SSTanFOA HW30 -append
