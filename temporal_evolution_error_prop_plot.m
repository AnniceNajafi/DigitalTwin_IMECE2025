%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @author Annice Najafi
%
% Creates a 1×4 row of 3D “stacked” plots—one each for MAE, MSE, RMSE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


modules      = {'error_m1','error_m2','error_m3','error_m4'};
moduleLabels = {'Module 1','Module 2','Module 3','Module 4'};
baseColors   = [0.2667,0.0902,0.3216;  
                0.6588,0.5333,0.7098;  
                0.4706,0.7020,0.8078;  
                0.2196,0.4549,0.4706]; 
targetColors = [0.1216,0.3804,0.5529;
                0.1608,0.5020,0.7255;
                0.1412,0.4431,0.6392;
                0.0549,0.3843,0.3176];


metrics   = {'mae','mse','rmse'};
fileNames = { 'error_prop_mae.csv', ...
              'error_prop_mse.csv', ...
              'error_prop_rmse.csv'};
titles    = {'Mean Absolute Error', ...
             'Mean Squared Error', ...
             'Root Mean Squared Error'};


figure('Position',[100 100 1600 400]);

for k = 1:3
   
    T      = readtable(fullfile('~/Downloads', fileNames{k}));
    extVals= unique(T.external_err);
    
 
    ax = subplot(1,4,k);
    hold(ax,'on'); grid(ax,'on');
    xlabel(ax,'Time');
    ylabel(ax,'External Error');
    zlabel(ax,'Error Value');
    title(ax,titles{k});
    
    
    T_offset = max(T.time) + 0.05*(max(T.time)-min(T.time));
    
   
    for m = 1:numel(modules)
        modName = modules{m};
        for i = 1:numel(extVals)
            ext = extVals(i);
            S   = T(T.external_err==ext,:);
            
          
            alpha = ext / max(extVals);
            color = baseColors(m,:) + alpha*(targetColors(m,:)-baseColors(m,:));
            
    
            plot3(ax, S.time, repmat(ext,height(S),1), S.(modName), '-.', ...
                  'Color',      color, ...
                  'LineWidth',  2, ...
                  'Marker',     '.', ...
                  'MarkerSize', 8);
        end
        
    
        finalErrs = arrayfun(@(e) ...
            T.(modName)( find(T.external_err==e,1,'last') ), ...
            extVals );
        plot3(ax, repmat(T_offset,size(extVals)), extVals, finalErrs, ...
              'o-', 'LineWidth',  2, 'MarkerSize',8, ...
              'MarkerFaceColor', baseColors(m,:), ...
              'MarkerEdgeColor',baseColors(m,:),'Color',baseColors(m,:));
    end
    
    view(ax,3);
    hold(ax,'off');
    
   
    if k==1
        legend(ax, arrayfun(@(e) sprintf('ext=%.2f',e), extVals,'uni',0), ...
               'Location','northeastoutside','FontSize',8);
    end
end


axC = subplot(1,4,4);

imagesc(axC, (1:4)');
colormap(axC, baseColors);
%cb = colorbar(axC, 'Ticks', 1:4, 'TickLabels', moduleLabels, ...
%              'Location','eastoutside');
title(axC, 'Module Colors');
axis(axC,'off');
