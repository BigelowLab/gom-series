%ERSSTstats
%Compute the probability of coldwaves

load LMEbounds S
load ERSSTtrendstatsLME.mat Es S N yr ERSSTanLME

for n=1:length(N);
    Es(n).Pcw=nans(size(Es(n).Phw));
    for q=1:66;%LME
        if(~isempty(S(q).IersstOc))%have these pixels
            
            %probability of a "cold wave" is just the probability of a
            %heatwave with the trend reversed.
            [Pcw,t1,t2,t3]=Psurprise(-Es(n).R(q,:)', N(n), Es(n).gamma(q,:)', 0.977);
            Es(n).Pcw(q,:)=Pcw';
        end
    end
end
n=find(N==30);
CW30=heatwaveagainstshift(reshape(ERSSTanLME,66,1,NY),30,2,reshape(Es(n).Mn,66,1,NY), reshape(Es(n).SD,66,1,NY),1);
CW30=squeeze(CW30);

save ERSSTtrendstatsLME_cold.mat Es S N CW30 yr ERSSTanLME