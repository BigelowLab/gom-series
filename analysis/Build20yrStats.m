%Build 20 year statistics

% /Users/apershing/matlab/logs/2017_06_27_12.11_diary.txt on vapor

disp('LME');
load ERSSTtrendstatsLME HW30 Es
NY=size(HW30,2);
FdLME=nans(21,NY,66);
mnLME=nans(NY,66);
likeLME=mnLME;
totLME=mnLME;
for k=1:66;
    for j=50:NY;
        I=(-19:0)+j;
        P=Es(3).Phw(k,I)';
        totLME(j,k)=sum(HW30(k,I));
        if(~isnan(totLME(j,k)))
            [likeLME(j,k),mnLME(j,k),FdLME(:,j,k)]=surprise_likelihood(P,totLME(j,k),10000);
        end
    end
end

save stats_20yrs mnLME likeLME totLME

% /Users/apershing/matlab/logs/2017_07_07_07.55_diary.txt on vapor

disp('ERSST');
load ERSSTtrendstats HW30all Es
NY=size(HW30all,2);
mnERSST=nans(NY,8993);
likeERSST=mnERSST;
totERSST=mnERSST;
for k=1:8993;
    if(mod(k,100)==0);
        disp(k);
    end;
    for j=50:NY;
        I=(-19:0)+j;P=Es(3).Phw(k,I)';
        totERSST(j,k)=sum(HW30all(k,I));
        if(~isnan(totERSST(j,k)))
            [likeERSST(j,k),mnERSST(j,k),tmp]=surprise_likelihood(P,totERSST(j,k),1000);
        end
    end;
end

save stats_20yrsERSST mnERSST likeERSST totERSST

% % /Users/apershing/matlab/logs/2017_07_23_22.28_diary.txt on vapor
% totCM=nans(66,199,27);
% likeCM=totCM;
% mnCM=totCM;
% for p=1:66;
%     for q=1:27;
%         for j=51:199;I=(-19:0)+j;
%             P=CMES(3).Phw(p,I,q);P=P(:);
%             totCM(p,j,q)=sum(squeeze(HW30cm(p,q,I)));
%             [likeCM(p,j,q),mnCM(p,j,q),tmp]=surprise_likelihood(P,totCM(p,j,q),1000);
%         end;
%     end;
% end
% likeCM=permute(likeCM,[2,1,3]);
% mnCM=permute(mnCM,[2,1,3]);
% totCM=permute(totCM,[2,1,3]);
% save stats_20yrsCMIP likeCM mnCM totCM CMyr
