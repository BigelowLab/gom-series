%Build Figures 1 and 2 for PNAS paper on Surprising Surprises
%
%% 1) Run UpdateERSSTforSurprises to update the analysis

%UpdateERSSTforSurprises;

%% 2) Create figures for warm (Fig 1a) and cold (Fig 1b) surprises

load ERSSTtrendstatsLME
figure(1);
clf
fld=ajpcolor(yr,0:66,Fd);
hold on
g=plot(Ns(:,1),Ns(:,2),'w');
h=plot(yr,sum(HW30),'r');
set(gca,'xlim',[yr(1)+30.5 2018.5])
set(gca,'ylim',[0 25])
set([g,h],'linewidth',1.5)
xlabel('year');
ylabel('Number of LMEs')
title('Observed and expected number of LME surprises')
colormap([0 0 0; parula(35)])
caxis([-1e-3,0.35])
colorbar
set(gcf,'position',[326   828   738   517]);
%220.902, 175.273
set(gca,'position',[0.08 0.08 0.8,0.8*175.273/220.092]);

%print('-dpng','F1A_Surprise_likelihood_warm_All.png');
% set(fld,'visible','off');
% set(g,'color','k');
% %EPSPNGsave('F1A_Surprise_likelihood_warm_Lines');
% set(fld,'visible','on');
% set([g,h],'visible','off');
%print('-djpeg','-r300','F1A_Surprise_likelihood_warm_Field.jpg');

%%
%same, but for cold waves
load ERSSTtrendstatsLME_cold
figure(2);
clf
fld=ajpcolor(yr,0:66,Fdc);
hold on
g=plot(Nsc(:,1),Nsc(:,2),'w');
h=plot(yr,sum(CW30),'r');
set(gca,'xlim',[yr(1)+30.5 2018.5])
set(gca,'ylim',[0 25])
set([g,h],'linewidth',1.5)
xlabel('year');
ylabel('Number of LMEs')
title('Observed and expected number of LME cold surprises')
colormap([0 0 0; parula(35)])
caxis([-1e-3,0.35])
colorbar
set(gcf,'position',[326   828   738   517]);
%220.902, 175.273
set(gca,'position',[0.08 0.08 0.8,0.8*175.273/220.092]);

print('-dpng','F1B_Surprise_likelihood_cold_All.png');
set(fld,'visible','off');
set(g,'color','k');
%EPSPNGsave('F1B_Surprise_likelihood_cold_Lines');
set(fld,'visible','on');
set([g,h],'visible','off');
%print('-djpeg','-r300','F1B_Surprise_likelihood_cold_Field.jpg');

%% 3) Create the time series of surprising surprises by region
load stats_20yrs; %expected and observed surprises
SupSup=totLME-mnLME;%observed minus expected--the surprising surprises

load LMEbounds Spart3; %partition into ocean regions

figure(3);
clf;
for j=1:length(Spart3);
    Y=mean(SupSup(:,Spart3(j).I),2);
    h(j)=plot(yr,Y);
    nms{j}=Spart3(j).name;
    hold on;
end

%240.17, 229.964
set(gcf,'position',[326   828   738   517]);
set(gca,'position',[0.08 0.08 0.8*240.17/229.964 0.8]);
legend(h,nms,'location','northwest');

%EPSPNGsave('F2A_ocean_trends');

%% 4) Map the Surprising Suprises
figure(4)
clf;
mapSurprisingSurprises(2018);
set(gcf,'position',[352         391        1810         954]);
discretecolorbar([-1 1]*4,8,4,'rwb');%rwb colorbar
print('-dpng','F2B_SurprisingSurprises.png');
print('-djpeg','-r300','F2B_SurprisingSurprises.jpg');

%% 5) The regression model
figure(5);
clf;
load stats_20yrs; %expected and observed surprises
D=totLME-mnLME;%observed minus expected--the surprising surprises
Iyr=find(yr==2018);
D=[(1:66)',D(Iyr,:)'];
load ERSSTtrendstatsLME Es
diffT=18;
dR=[(1:66)',Es(1).R(:,Iyr)-Es(1).R(:,Iyr-diffT)];
dG=[(1:66)',Es(1).gamma(:,Iyr)-Es(1).gamma(:,Iyr-diffT)];

fprintf('Table S2. Linear models for the number of surprising surprises as a function of the change in trend ?Trend) and change in the standard deviation (?SD).  ');
fprintf('For each independent variable, the change is the difference between the 15 year period ending in %d and the period ending in %d.  ', yr(Iyr), yr(Iyr-diffT));
fprintf(' The coefficients for ?Trend, ?SD, and the constant term (C) are shown for trend-only, variance only, and for the full model. All models were significant (p<0.01), and the R2 and AIC statistics were used to evaluate model performance.\n');

fprintf('%8s\t?Trend\t?SD\tC\tR2\tAIC\tp\n','Model');
[r2,p,coefs,Ypred,sxy, M, AIC,coefs95]=multiregress(D,dR);
fprintf('%8s\t%5.2f\t-\t%5.2f\t%4.2f\t%6.2f\t%4.2f\n','Trend',coefs(1),coefs(end),r2,AIC,p);
[r2,p,coefs,Ypred,sxy, M, AIC,coefs95]=multiregress(D,dG);
fprintf('%8s\t-\t%5.2f\t%5.2f\t%4.2f\t%6.2f\t%4.2f\n','Variance',coefs(1),coefs(end),r2,AIC,p);
[r2,p,coefs,Ypred,sxy, M, AIC,coefs95]=multiregress(D,dR,dG);
fprintf('%8s\t%5.2f\t%5.2f\t%5.2f\t%4.2f\t%6.2f\t%4.2f\n','Both',coefs(1),coefs(2),coefs(end),r2,AIC,p);
fprintf('\n');

yrreg(Ypred,D);
h=findobj(gca,'type','text');delete(h)
h=findobj(gca,'marker','o');
h.MarkerSize=9;
ylabel('Observed-Expected')
xlabel('Modeled')
legend off
%200.028, 176.215
set(gcf,'position',[500 500 800 800]);
set(gca,'position',[0.08 0.08 0.8*200.028/176.215 0.8]);


%EPSPNGsave('F2C_Regression');

%% 6) The Variability-Trend plot
figure(6);
clf;
load ERSSTtrendstatsLME Es
load LMEbounds.mat

Iyr=165;

[eX,eY]=error_ellipse(Es(3).R(:,Iyr),Es(3).gamma(:,Iyr),0.75);
e=plot(eX,eY,'k-','linewidth',2);
hold on
e2=plot(nanmean(Es(3).R(:,Iyr)),nanmean(Es(3).gamma(:,Iyr)),'kp');
e2.MarkerEdgeColor='none';
e2.MarkerFaceColor='k';
e2.MarkerSize=12;


h=plotmrks(Es(3).R(:,Iyr),Es(3).gamma(:,Iyr));
h.MarkerFaceColor='c';
h.MarkerEdgeColor='none';
h.MarkerSize=9;
for j=1:66;
    g(j)=text(Es(3).R(j,Iyr),Es(3).gamma(j,Iyr),S(j).abbrv);
end

set(gca,'xtick',0:0.02:0.1,'xlim',[0 0.1]);
set(gca,'ytick',0:0.1:1,'ylim',[0 1]);

%width: 240.17, height: 230.645;
set(gcf,'position',[500 500 800 800]);
set(gca,'position',[0.13 0.11 0.8*240.17/230.645 0.8]);
xlabel('Temperature Trend (° yr-1)');
ylabel('Temperature Variability (°)');

%EPSPNGsave('F2D_VarianceTrend');


%% Figure S1--the LME map

load LMEbounds
Nr=length(Spart3);
R=nans(66,1);

fid=fopen('S01_Table.txt','wt');

for j=1:Nr;
    R(Spart3(j).I)=j;
    Rnms{j}=Spart3(j).name;
    for k=1:length(Spart3(j).I);
        fprintf(fid, '%s\t%s\t%s\n',Rnms{j},S(Spart3(j).I(k)).abbrv, S(Spart3(j).I(k)).LME_NAME);
    end
end
fclose(fid);
figure(7);
clf;

maponLMEs(R);
title('');
caxis([0.5,Nr+0.5]);
cb=colorbar;
cb.Ticks=1:Nr;
cb.TickLabels=Rnms;
colormap(parula(Nr));
set(gcf,'position',[352         391        1810         954]);
%print -dpng FS1_Regions.png
%print -djpeg -r300 FS1_Regions.jpg


