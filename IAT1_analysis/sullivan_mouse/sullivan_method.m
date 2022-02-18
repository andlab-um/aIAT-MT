



[angle4,stim4]=preprocessing(data4);
[angle7,stim7]=preprocessing(data7);



%% time course regressions

minTrials = 30; % minimum trials req'd for regression (only really important in non-normalized time)

l4.coef = NaN(101,33);
l4.pval = NaN(101,33);
for subj = 1:33 % subject

    mouseVar4 = angle4{subj};
    
    %regression: angle~ stim
    
    for t = 1:101 % one regression per time point
        if sum(~isnan(mouseVar4(t,:))) > minTrials
            stats4 = regstats(mouseVar4(t,:), stim4(:,subj), 'linear', {'tstat'});
            l4.coef(t,subj) = stats4.tstat.beta(2);
            l4.pval(t,subj) = stats4.tstat.pval(2);
        end
        
    end
    
     % st calc: get one-sided t-test
    test4 = l4.pval(:,subj)./2 < .05;
    if ismember(1,test4)==0
        continue
    end
    l4.st(subj) = 1+find(test4==1,1,'first');
        
end

% omit significance times of t >= post-choice
% if sig time >= 101, then it's not really a signif. time, is it?
l4.st(l4.st>=101) = NaN;

% now group significance times
% all subjects
l4.stAll = 1+find(ttest(l4.coef',0,.05,'right')==0,1,'last');


%%%%%%%%%%%7
minTrials = 30; % minimum trials req'd for regression (only really important in non-normalized time)

l7.coef = NaN(101,33);
l7.pval = NaN(101,33);

for subj = 1:33 % subject

    mouseVar7 = angle7{subj};
    
    %regression: angle~ stim
    
    for t = 1:101 % one regression per time point
        if sum(~isnan(mouseVar7(t,:))) > minTrials
            stats7 = regstats(mouseVar7(t,:), stim7(:,subj), 'linear', {'tstat'});
            l7.coef(t,subj) = stats7.tstat.beta(2);
            l7.pval(t,subj) = stats7.tstat.pval(2);
        end
        
        
    end
    % st calc: get one-sided t-test
    test7 = l7.pval(:,subj)./2 < .05;
    if ismember(1,test7)==0
        continue
    end

    l7.st(subj) = 1+find(test7==1,1,'first');
    
end

% omit significance times of t >= post-choice
% if sig time >= 101, then it's not really a signif. time, is it?
l7.st(l7.st>=101) = NaN;

% now group significance times
% all subjects
l7.stAll = 1+find(ttest(l7.coef',0,.05,'right')==0,1,'last');

%% timecourse regression graph (must run timepoint_regressions first)

lColor=[0 0 0];
timeArray = 1:101;
xLimitToPlot = timeArray(end);
figure(1); hold on;
line([timeArray(1) timeArray(end)],[0 0],'color',[.4 .4 .4]) % zero-line
shadedErrorBar(timeArray,abs(nanmean(l4.coef,2)), ...
    nanstd(l4.coef')'/sqrt(33), {'Color','g','linewidth',1.5},1);
xlim([timeArray(1) timeArray(end)])
xlabel('Time normalized to 101 points','fontsize',24);
ylabel('Beta','fontsize',24)
set(gca,'XTick',0:20:100,'fontsize',20);
hold on;

line([timeArray(1) timeArray(end)],[0 0],'color',[.4 .4 .4]) % zero-line
shadedErrorBar(timeArray,abs(nanmean(l7.coef,2)), ...
    nanstd(l7.coef')'/sqrt(33), {'Color','r','linewidth',1.5},1);
xlim([timeArray(1) timeArray(end)])
xlabel('Time normalized to 101 points','fontsize',24);
ylabel('Beta','fontsize',24)
set(gca,'XTick',0:20:100,'fontsize',20);


%% figure 4B: significance time histogram
%first significant
mx = nanmax(nanmax(l4.st,l7.st));
mn = nanmin(nanmin(l4.st,l7.st));
%xvalues = linspace(mn,mx,2); 
       

figure(2); hold on;
scatter(l4.st(1,:),zeros(1,33));
scatter(l7.st(1,:),ones(1,33));

% hist(l4.st(1,:),xvalues);
% hist(l7.st(1,:),xvalues);
% h = get (gca, 'children');
% set(h(2),'FaceColor',[.2 .2 .2],'EdgeColor','k')
% set(h(1),'FaceColor',[.6 .6 .6],'EdgeColor','k','LineStyle','-.')
% alpha(h(1),.6)
% graphkey = h;
% legend(graphkey,{'health','taste'},'location','northwest','fontsize',20)
% legend BOXOFF
% xlim([40,105])
% xlabel('Significance time','fontsize',24);
% ylabel('Frequency','fontsize',24);
% set(gca,'fontsize',20)






function [angle,stim]=preprocessing(data4)
%% load and format data
% get mouse data
% columns in the csv file:
% [subject | RT | stim | choice | x-position (pixels) | y-position (pixels) ] 
%choice (1==left)
%%%%%手动导入mouse4。元胞数组。最终使用data4

xPos = NaN(200,101,33);
yPos = NaN(200,101,33);
stim = NaN(200,33);
choice = NaN(200,33);
RT = NaN(200,33);
for subj = 1:33
    thisSubjData = data4(data4(:,1)==(subj+100),:);
    for trial=1:200
    % find this subject
     
     RT(trial,subj) = thisSubjData(trial,2);
     stim(trial,subj)=thisSubjData(trial,3);
     choice(trial,subj) = thisSubjData(trial,4);
     nTrials = length(thisSubjData);
    for timebin = 1:101
        xPos(trial,1:101,subj) = thisSubjData(trial,5:105);
        yPos(trial,1:101,subj) = thisSubjData(trial,106:206);       
    end 
    end
end
% %%substitute 0 of first movement coordinates
% for i= 1:33
%     for j =1:200
%         x1=find(xPos(j,:,i),1,'first');
%         
%         xPos(j,1:x1-1,i)=xPos(j,x1,i);
%     end
% end
% for i= 1:33
%     for j =1:200
%         y1=find(yPos(j,:,i),1,'first');
%         
%         yPos(j,1:y1-1,i)=yPos(j,y1,i);
%     end
% end

%% transform mouse data

angle = cell(33,1);


for subj = 1:33 % subject
    angle{subj} = NaN(101,200);
    for trial = 1:200 
        
        % angle
        angle{subj}(:,trial) = ...
            abs(atand(xPos(trial,:,subj) ./ ...
            yPos(trial,:,subj))) .* ...
            sign(xPos(trial,:,subj));
        % optional: exclude points where mouse moving downward
        excludeInd = [false yPos(trial,2:end,subj) < yPos(trial,1:end-1,subj)];
        angle{subj}(excludeInd,trial) = NaN;
        
    end
    
end


%% excluding mouse trials

%logical variables
exclude = false(200,33);
reason.yCross =  false(200,33);
reason.RT =  false(200,33);
reason.overshoot =  false(200,33);
reason.smallRT =  false(200,33);
for subj = 1:33 % subject
    
    for trial = 1:200 % trial
        
        % how many times does cursor cross y axis? exclude if > 3
        ycross=0;
        for t=2:101
            if (xPos(trial,t,subj) < 0 && xPos(trial,t-1,subj) >= 0) ...
                || (xPos(trial,t,subj) > 0 && xPos(trial,t-1,subj) <= 0)
                ycross = ycross+1;
            end
        end
        if ycross > 3
            exclude(trial, subj) = true;
            reason.yCross(trial, subj) = true;
        end
        
   end
    
    % exclude b/c RT > 1 or 2 SD above mean
    % in calculating standard deviation, don't include trials where RT > 5 min b/c that's a mistake or question they're asking (i.e. they're not doing the task).
    ind = ~exclude(:,subj) & RT(:,subj) <= 5000;%available trials
    rtData = RT(ind,subj);
    oneSD = nanstd(rtData);
    twoSD = oneSD*2;
    ave = nanmean(rtData);
    excl.oneSD = RT(:,subj) > ave+oneSD;
    excl.twoSD = RT(:,subj) > ave+twoSD;
    output.excludeTrialOneSDRT = oneSD;
    output.excludeTrialTwoSDRT = twoSD;
    exclude(excl.twoSD,subj) = true;
    reason.RT(excl.twoSD,subj) = true;

    % **optional**: exclude "overshoots"
%     % (times where the cursor goes far past food box before clicking)
%     for trial = 1:nTrials(subj) % trial
%         if sum(abs(spatDirect{subj}(:,1,trial)) >= 1.3) > 0
%             exclude(trial,subj) = true;
%             reason.overshoot(trial,subj) =true;
%         end
%     end 
%     
    % **optional**: exclude very small RTs b/c that's weird
    if RT(:,subj) <= 200
        exclude(:, subj) = true;
        reason.smallRT(:, subj) = true;
    end
    
    % now remove those trials:
    angle{subj}(:,exclude(:,subj)) = NaN;
    
end
end