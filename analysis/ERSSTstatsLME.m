%ERSSTstats
%compute the trendstats for each LME
load ERSST_annual_ocean SSTanFOA yr
load LMEbounds S
N=[15,20,30,50];
NY=length(yr);


HW30=nans(66,NY);
for n=1:length(N);
    Es(n).Mn=nans(66,NY);%take annual means
    Es(n).SD=Es(n).Mn;
    Es(n).R=Es(n).Mn;
    Es(n).gamma=Es(n).Mn;
    Es(n).Phw=Es(n).Mn;
    for q=1:66;%LME
        if(~isempty(S(q).IersstOc))%have these pixels
            D=nanmean(SSTanFOA(S(q).IersstOc,:));%data-monthly, 1-by-163
            ERSSTanLME(q,:)=D;
            
            fprintf('%2d, %2d\n',N(n),q);
            for p=N(n)+1:NY;
                J=(1:N(n))+(p-N(n)-1);
                Es(n).Mn(q,p)=nanmean(D(J));
                Es(n).SD(q,p)=nanstd(D(J));%sigma
                X1=[J(:),J(:)];
                X2=X1;X2(:,2)=X2(:,2).^2;
                Y=[X1(:,1),D(J)'];
                
                coef=[J(:),ones(N(n),1)]\D(J)';
                Es(n).R(q,p)=coef(1);
                
            end;
            Es(n).gamma(q,:)=SDwithtrend_inv(Es(n).R(q,:),N(n),Es(n).SD(q,:));
            [Phw,t1,t2,t3]=Psurprise(Es(n).R(q,:)', N(n), Es(n).gamma(q,:)', 0.977);
            Es(n).Phw(q,:)=Phw';
        end
    end
end
n=find(N==30);
HW30=heatwaveagainstshift(reshape(ERSSTanLME,66,1,NY),30,2,reshape(Es(n).Mn,66,1,NY), reshape(Es(n).SD,66,1,NY));
HW30=squeeze(HW30);

save ERSSTtrendstatsLME.mat Es S N yr HW30 ERSSTanLME  -append