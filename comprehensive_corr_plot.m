%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% @author Annice Najafi
%
% Description:
%   Produces a 3×3 grid of 3D “stacked” plots for MAE, MSE, and RMSE.
%   Reads from CSVs named 'error_prop_mae.csv', 'error_prop_mse.csv',
%   'error_prop_rmse.csv'.  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

metrics      = {'mae','mse','rmse'};
metricNames  = {'Mean Absolute Error','Mean Squared Error','Root Mean Squared Error'};
moduleNames  = {'Module1','Module2','Module3','Module4'};
pairs        = [1 2; 2 3; 3 4];  

figure('Position',[100 100 1200 900]);

for m = 1:3
  
    T       = readtable(sprintf('~/Downloads/error_prop_%s.csv', metrics{m}));
    extVals = unique(T.external_err);
    nExt    = numel(extVals);

    startC = [0, 0.50196, 0.50196];
    endC   = [0.45,0.31,0.588];
    colors = interp1([1 nExt],[startC; endC],1:nExt);

    for p = 1:3
        ax = subplot(3,3,(m-1)*3 + p);
        hold(ax,'on'); grid(ax,'on');

        xlabel(ax, sprintf('%s (%s)', moduleNames{pairs(p,1)}, metricNames{m}));
        ylabel(ax, 'External Error');
        zlabel(ax, sprintf('%s (%s)', moduleNames{pairs(p,2)}, metricNames{m}));
        title(ax, sprintf('%s: %s → %s', ...
              metricNames{m}, ...
              moduleNames{pairs(p,1)}, ...
              moduleNames{pairs(p,2)}));

        for i = 1:nExt
            ext = extVals(i);
            S   = T(T.external_err==ext,:);
            xD  = S{:, sprintf('error_m%d', pairs(p,1))};
            zD  = S{:, sprintf('error_m%d', pairs(p,2))};
            yP  = repmat(ext, size(xD));

           
            [xS, idx] = sort(xD);
            zS        = zD(idx);
            yS        = yP(idx);

            if strcmp(metrics{m}, 'mse')
                
                plot3(ax, xS, yS, zS, '-*', ...
                      'Color',      colors(i,:), ...
                      'MarkerSize', 6, ...
                      'LineWidth',  1.2);

               
                pp   = polyfit(xD, zD, 1);
                %xFit = linspace(min(xS), max(xS), 100);
                %zFit = pp(1)*xFit.^2 + pp(2)*xFit + pp(3);
                %plot3(ax, xFit, repmat(ext, size(xFit)), zFit, '-', ...
                %      'Color',     colors(i,:), ...
                %      'LineWidth', 1.5);

                eqStr = sprintf('z = -%.2f x + %.2f', pp(1), pp(2));
            else
              
                pp   = polyfit(xD, zD, 1);
                plot3(ax, xS, yS, zS, '-*', ...
                      'Color',      colors(i,:), ...
                      'LineWidth',  1.5, ...
                      'MarkerSize', 6);

                eqStr = sprintf('z = %.2f x + %.2f', pp(1), pp(2));
            end

       
            text(ax, 0.02, 0.95-(i-1)*0.06, eqStr, ...
                 'Units',               'normalized', ...
                 'HorizontalAlignment', 'left', ...
                 'FontSize',            8, ...
                 'Color',               colors(i,:), ...
                 'BackgroundColor',     'white', ...
                 'EdgeColor',           colors(i,:), ...
                 'LineWidth',           0.5);
        end

        view(ax, 3);
        hold(ax, 'off');

        if m==1 && p==1
            legend(ax, arrayfun(@(e) sprintf('ext=%.2f',e), extVals,'uni',0), ...
                   'Location','northeastoutside','FontSize',8);
        end
    end
end
