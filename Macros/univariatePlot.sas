/*----------------------- Copyright 2014, Rho, Inc.  All rights reserved. ------------------------\

  Project:      Statistical data checks

    Program:    univariatePlot.sas

      Purpose:  Generate bee swarm plots with outliers identified by a general linear model

      Macros:   %dataExist, %variableExist, %directoryExist, %argumentCheck, %stackRTFs, %RTF2PDF7

    /---------------------------------------------------------------------------------------------\
      Parameters:
    \---------------------------------------------------------------------------------------------/

      [REQUIRED]

        [ data ]        - one- or two-level input dataset name
        [ outcomeVar ]  - outcomeVariable to be plotted on the y-axis
        [ IDvar ]       - subject identifier variable
                            + defaults to USUBJID
        [ XY ]          - specifies which axis &outcomeVar will be plotted on
                            + defaults to Y

      [optional]

        [ discVar ]     - discrete covariate(s), effects in the GLM
        [ contVar ]     - continuous covariate(s), effects in the GLM
        [ groupVar ]    - within-plot grouping variable, each value of which plots on the x-axis
        [ panelVar ]    - panelling variable, each value of which generates a separate panel
        [ byVar ]       - by variable(s), each value of which generates a separate plot
        [ outPath ]     - output directory (where plots and outlier data will be sent)
                            + defaults to directory of program which calls %univariatePlot
        [ outName ]     - output name
                            + defaults to 'Univariate Plot - &outcomeVar by
                              <first variable in &discVar/&contVar>< - &byVar = &byVar value>'
                                > If specified and &byVar is also specified, output files
                                  will take the form '&outName - &byVar = &byVar value'
        [ cutoff ]      - outlier detection threshold, preferably an integer
                            + defaults to 95
                                > cutoff identifies studentized residuals outside the
                                  &cutoff percentile of the distribution (one-sided)
        [ orientation ] - figure orientation, LANDSCAPE or PORTRAIT
                            + defaults to LANDSCAPE
        [ panelCols ]   - number of cells in panel columns
                            + defaults to 2
        [ panelRows ]   - number of cells in panel rows
                            + defaults to 2
        [ width ]       - figure width in inches
                            + defaults to 9.5 in LANDSCAPE, 7 in PORTRAIT
        [ height ]      - figure height in inches
                            + defaults to 7 in LANDSCAPE, 9.5 in PORTRAIT

    /---------------------------------------------------------------------------------------------\
      Examples:
    \---------------------------------------------------------------------------------------------/

        %univariatePlot
            (data = DERIVE.ADSL,
            ,outcomeVar = AGE);

/-------------------------------------------------------------------------------------------------\
  Program history:
\-------------------------------------------------------------------------------------------------/

    Date        Programmer          Description
    ----------  ------------------  --------------------------------------------------------------
    2014-06-16  Spencer Childress   Create
    2014-12-16  Spencer Childress   Add outName parameter
    2014-12-23  Spencer Childress   Update to identify one-sided outliers
    2015-01-15  Spencer Childress   Update to identify outliers with studentized residuals with
                                    the current observation deleted
    2015-04-23  Spencer Childress   Update to print different value in title statement than in
                                    file name

\------------------------------------------------------------------------------------------------*/

%macro univariatePlot
    (data        = 
    ,outcomeVar  = 
    ,IDvar       = USUBJID
    ,XY          = Y
    ,discVar     = 
    ,contVar     = 
    ,groupVar    = 
    ,panelVar    = 
    ,byVar       = 
    ,outPath     = 
    ,outName     = 
    ,cutoff      = 90
    ,orientation = LANDSCAPE
    ,panelRows   = 2
    ,panelCols   = 2
    ,width       = 
    ,height      = ) / minoperator;

  %*Point SASAUTOS to helper macros.;
    options sasautos = ('S:\BASESTAT\RhoUtil\sas-statisticalDataChecks\Macros\helperMacros' sasautos);

  %*Determine option settings to return their value at the end of the macro.;
    %local xWaitOption notesOption dateOption numberOption lsOption i;

    %let  xWaitOption = %sysfunc(getoption(xwait));
    %let  notesOption = %sysfunc(getoption(notes));
    %let   dateOption = %sysfunc(getoption(date));
    %let numberOption = %sysfunc(getoption(number));
    %let     lsOption = %sysfunc(getoption(ls));

    options noxwait nonotes nodate nonumber
        ls = 150;

  %*Clear titles and footnotes.;
    title;
    footnote;

    %put;
    %put %nrstr(/-------------------------------------------------------------------------------------------------\);
    %put %nrstr(  %univariatePlot beginning execution);
    %put %nrstr(\-------------------------------------------------------------------------------------------------/);
    %put;

        /***--------------------------------------------------------------------------------------\
          Parameter error checks
        \--------------------------------------------------------------------------------------***/

          %*[ data ];
            %dataExist(data);
                %if not &dataExist %then %goto exit;

          %*[ IDvar/outcomeVar/byVar/panelVar/groupVar/discVar/contVar ];
            %let coVar = %sysfunc(prxchange(%str(s/\*+/ /), -1, %nrbquote(&discVar &contVar)));
            %variableExist(&data, outcomeVar, 1);
                %if not &variableExist %then %goto exit;
            %variableExist(&data, IDvar, 1);
                %if not &variableExist %then %goto exit;
            %variableExist(&data, coVar, 1, Model requires at least one covariate.  Execution terminating.);
                %if not &variableExist %then %goto exit;
            %variableExist(&data, groupVar);
                %if not &variableExist %then %goto exit;
            %variableExist(&data, panelVar);
                %if not &variableExist %then %goto exit;
            %variableExist(&data, byVar);
                %if not &variableExist %then %goto exit;

          %*[ XY ];
            %argumentCheck(XY, X Y, Y);

          %*[ outPath ];
            %directoryExist(outPath,_plots_);
                %if not &directoryExist %then %goto exit;

          %*[ outName ];
            %if %nrbquote(&outName) = %then %do;
                %let outName = Univariate Plot - &outcomeVar;

                %put %str(    --> [ outName ] defaulting to &outName..);
            %end;

          %*[ cutoff ];
            %if %nrbquote(&cutoff) ne %then %do;
                %do i = 1 %to %length(&cutoff);
                    %let _char_ = %substr(&cutoff, &i, 1);

                    %if ^(%sysfunc(anydigit(%bquote(&_char_))) or %bquote(&_char_) = .) %then %do;
                        %put %str(    --> [ cutoff ] contains a value of "&_char_".);
                        %put %str(    -->     It can only take non-negative numeric arguments.);

                        %goto badCutoff;
                    %end;
                %end;

                %if %sysevalf(&cutoff gt 100) or %sysevalf(&cutoff lt 1) %then %do;
                    %put %str(    --> [ cutoff ] is outside the accepted percentile range of 1-100.);

                    %goto badCutoff;
                %end;
            %end;
            %else %do;
                %put %str(    --> [ cutoff ] unspecified.);

                %goto badCutoff;
            %end;

            %goto goodCutoff;

            %badCutoff:
                %put %str(    -->     Defaulting to 90, which identifies studentized residuals outside the 90th percentile.);

                %let cutoff = 90;

            %goodCutoff:
                %let alpha = %sysfunc(strip(%sysevalf(1 - &cutoff/100)));

          %*[ orientation ];
            %argumentCheck(orientation, LANDSCAPE PORTRAIT, LANDSCAPE);

          %*[ panelRows ];
            %if &panelVar ne %then %do;
                %argumentCheck(panelRows, 1 2 3 4 5 6 7 8 9 10, 2);
            %end;

          %*[ panelCols ];
            %if &panelVar ne %then %do;
                %argumentCheck(panelCols, 1 2 3 4 5 6 7 8 9 10, 2);
            %end;

          %*[ width ];
            %if %nrbquote(&width) ne %then %do;
                %if %sysfunc(prxmatch(/[^\d.]/, %nrbquote(&width))) %then %do;
                    %if &orientation = LANDSCAPE %then %do;
                        %put %str(    --> [ width ] contains non-numeric value.  Defaulting to 9.5.);
                        %let width = 9.5;
                    %end;
                    %else %do;
                        %put %str(    --> [ width ] contains non-numeric value.  Defaulting to 7.);
                        %let width = 7;
                    %end;
                %end;
            %end;
            %else %do;
                %if &orientation = LANDSCAPE
                    %then %let width = 9.5;
                    %else %let width = 7;
            %end;

          %*[ height ];
            %if %nrbquote(&height) ne %then %do;
                %if %sysfunc(prxmatch(/[^\d.]/, %nrbquote(&height))) %then %do;
                    %if &orientation = LANDSCAPE %then %do;
                        %put %str(    --> [ height ] contains non-numeric value.  Defaulting to 7.);

                        %let height = 7;
                    %end;
                    %else %do;
                        %put %str(    --> [ height ] contains non-numeric value.  Defaulting to 9.5.);

                        %let height = 9.5;
                    %end;
                %end;
            %end;
            %else %do;
                %if &orientation = LANDSCAPE
                    %then %let height = 7;
                    %else %let height = 9.5;
            %end;

    %put;
    %put %nrstr(/-------------------------------------------------------------------------------------------------\);
    %put %nrstr(  PROC GLM running);
    %put %nrstr(\-------------------------------------------------------------------------------------------------/);
    %put;

        /***--------------------------------------------------------------------------------------\
          Print PROC GLM statements to log
        \--------------------------------------------------------------------------------------***/

            %put %str(    --> proc glm);
            %put %str(    -->     data = inputDataset;);
                %if %nrbquote(&byVar &panelVar) ne %then
            %put %str(    -->         by %sysfunc(strip(&byVar &panelVar)););
                %if %nrbquote(&groupVar &discVar) ne %then
            %put %str(    -->         class %sysfunc(strip(&groupVar &discVar)););
            %put %str(    -->         model);
            %put %str(    -->             &outcomeVar = %sysfunc(strip(&groupVar &discVar &contVar)););
            %put %str(    -->         output);
            %put %str(    -->             out = residuals);
            %put %str(    -->             rStudent = rStudentResid;);
            %put %str(    -->         ods output);
            %put %str(    -->             overallANOVA = DF %();
            %put %str(    -->                 keep = source DF &byVar &panelVar);
            %put %str(    -->                 where = (source = 'Error')%););
            %put %str(    --> quit;);

        /***--------------------------------------------------------------------------------------\
          Input data manipulation
        \--------------------------------------------------------------------------------------***/

          %*Keep relevant variables, subset on non-missing &outcomeVar, and sort.;
            %let grouping = %sysfunc(prxchange(%str(s/( +)/, /), -1, &byVar &panelVar &groupVar));
            %let sorting  = %sysfunc(prxchange(%str(s/( +)/, /), -1, &byVar &panelVar &groupVar &outcomeVar));

            proc sql;
                create table inputDataset as
                    select %sysfunc(prxchange(%str(s/( +)/, /), -1, &IDvar &byVar &panelVar &groupVar &outcomeVar &discVar &contVar)),
                        count(1) as GroupNObs,
                            %if %nrbquote(&groupVar) ne %then &groupVar as groupVar; %else 'NULL' as groupVar;
                        from &data (where = (not missing(&outcomeVar)))
                    %if %nrbquote(&grouping) ne %then group by &grouping;
                order by &sorting;
            quit;

            data _null_;
                if _n_ = 1 then do;
                    set inputDataset;

                    call symputx('xLabel', coalescec(vlabel(   groupVar), vname(   groupVar)));
                    call symputx('yLabel', coalescec(vlabel(&outcomeVar), vname(&outcomeVar)));
                end;
                stop;
            run;


            %put %str(    --> Number of observations with non-missing &outcomeVar: &sqlobs..);
            %put ;

        /***--------------------------------------------------------------------------------------\
          PROC GLM
        \--------------------------------------------------------------------------------------***/

            proc glm
                data = inputDataset;
                %if %nrbquote(&byVar &panelVar) ne %then
                    by &byVar &panelVar;;
                %if %nrbquote(&groupVar &discVar) ne %then
                    class &groupVar &discVar;;
                    model
                        &outcomeVar = &groupVar &discVar &contVar;
                    output
                        out = residuals
                        rStudent = rStudentResid;
                    ods output
                        overallANOVA = DF (
                            keep = source DF &byVar &panelVar
                            where = (source = 'Error'));
            quit;

        /***--------------------------------------------------------------------------------------\
          Output data manipulation
        \--------------------------------------------------------------------------------------***/

            %if %nrbquote(&byVar &panelVar) = %then %do;
                proc sql;
                    create table GLMdataset as
                        select Residuals.*, DF.DF
                            from Residuals,
                                 DF;
                quit;
            %end;
            %else %do;
                data GLMdataset;
                    merge
                        Residuals
                        DF;
                    by &byVar &panelVar;
                run;
            %end;

          %*Define outlier cutoffs, variables, and labels.;
            %macro outlierDataset;

                %global nGroups;

              %*Define numeric group variable, cutoff, and outlier label.;
                proc sort
                    data = GLMdataset;
                    by groupVar &outcomeVar;
                data outlierDataset;
                    set GLMdataset (where = (&bySubset));
                    by groupVar &outcomeVar;

                    if first.groupVar then do;
                                                     groupVarN + 1;
                        if missing("&groupVar") then groupVarN +  .25;
                    end;

                    RStud_cutoff  = tinv(&cutoff/100, DF - 1);

                    if .z lt RStud_cutoff le abs(RStudentResid) then do;
                        outlier = &outcomeVar;

                        outlierLabel = catx('/', %if &IDvar ne %then &IDvar,;
                                            putn(&outcomeVar, vformat(&outcomeVar)));
                    end;

                    label
                        outlier = "Suspected outlier"
                    ;
                run;

                proc sql noprint;
                  %*Calculate minimum, maximum, range, and total number of observations.;
                    create table preSwarm as
                        select *,
                            min(&outcomeVar) as ymin,
                                max(&outcomeVar) as ymax,
                                    max(&outcomeVar) - min(&outcomeVar) as r,
                                        count(1) as NObs
                            from outlierDataset
                    order by &sorting;

                  %*Capture number of records in largest group.;
                    select  max(GroupNObs)
                      into :NObs separated by ' '
                        from preSwarm;

                  %*Create format for displaying group variable.;
                    create table formats as
                        select distinct put(groupVarN - .5, 16.1) as start,
                                        put(groupVarN + .5, 16.1) as end,
                                        groupVar                  as label,
                                        "groupVarN"               as fmtname
                            from preSwarm (where = (1));
                    %let nGroups = &sqlobs;

                    select  type
                      into :groupType
                        from dictionary.columns
                          where libname = 'WORK'
                            and memname = 'FORMATS'
                            and    name = 'label';

                    insert into formats
                        set start = '**OTHER**', end = '**OTHER**', label = %if &groupType = num %then .; %else ' ';, fmtname = "groupVarN";
                quit;

                proc format
                    cntlin = formats;
                run;

              %*Sequence records within group variable in order to merge later.;
                data preSwarm;
                    set preSwarm;
                    by %sysfunc(tranwrd(%bquote(&grouping), %str(, ), %str( ))) groupVarN;

                    if first.groupVarN then seq = 0;
                                            seq + 1;
                run;

              %*Transpose to one variable per observation within each group.;
                proc transpose
                    data   = preSwarm
                    out    = preSwarm_T
                    prefix = y;
                    by  %sysfunc(tranwrd(%bquote(&grouping), %str(, ), %str( ))) groupVarN GroupNObs NObs ymin ymax r;
                    var &outcomeVar;
                run;

              %*Adjust x-values so every point can be displayed.;
                data beeSwarm_T;
                    set preSwarm_T;
                    by %sysfunc(tranwrd(%bquote(&grouping), %str(, ), %str( ))) groupVarN;

                    xsh       = .0166*&nGroups;
                    ysh       = 30;

                    threshold = 0.8*sqrt(1**2 + 1**2);
                    imax      = n(of y1-y&nobs);

                    array x         {&nobs};
                    array y         {&nobs};

                    array distleft  {&nobs};
                    array distright {&nobs};

                    *--- first data point stays home always ---;
                    i = 1;
                    x[i] = groupVarN;

                    *--- process subsequent data points one at a time ---;
                    do i = 2 to imax;

                        *--- start by trying to place the data point on the center line ---;
                        offleft  = 0;
                        offright = 0;

                        *--- reset distances for all previous ---;
                        do j = 1 to &nobs;
                            distleft[j]  = .;
                            distright[j] = .;
                        end;

                        *--- want to get outside of threshold either on the right or the left ---;
                        if not missing(y[i]) then do until (dleft ge threshold or dright ge threshold);

                            *--- evaluate all previous points at a given offleft/offright ---;
                            do j = 1 to (i - 1);

                                *--- look left ---;
                                xleft       = groupVarN - offleft;
                                xdl         = (x[j] - xleft)/xsh;
                                ydl         = (y[j] - y[i])/(r/ysh);
                                distleft[j] = sqrt(xdl**2 + ydl**2);

                                *--- look right ---;
                                xright       = groupVarN + offright;
                                xdr          = (x[j] - xright)/xsh;
                                ydr          = (y[j] - y[i])/(r/ysh);
                                distright[j] = sqrt(xdr**2 + ydr**2);

                            end;

                            *--- what is the worst result for any previous data point ---;
                            dleft  = min(of  distleft1-distleft&nobs);
                            dright = min(of distright1-distright&nobs);

                            *--- extend offleft/offright if worst result is not good enough ---;
                            if dleft  lt threshold then offleft  = offleft  + 0.05*xsh;
                            if dright lt threshold then offright = offright + 0.05*xsh;

                        end;

                        *--- pick side to put the data point on, choosing randomly if a tie ---;
                        posneg = 2*round(ranuni(1),1) - 1;

                             if offleft   = offright then x[i] = groupVarN + posneg*offright;
                        else if offleft  lt offright then x[i] = groupVarN - offleft;
                        else if offright lt offleft  then x[i] = groupVarN + offright;
                    end;
                run;

              %*Transpose x-values to one record per variable within each group.;
                proc transpose
                    data   = beeSwarm_T
                    out    = x
                    prefix = x;
                    by  %sysfunc(tranwrd(%bquote(&grouping), %str(, ), %str( ))) groupVarN;
                    var x1-x&nobs;
                run;

              %*Transpose y-values to one record per variable within each group.;
                proc transpose
                    data   = beeSwarm_T
                    out    = y
                    prefix = y;
                    by  %sysfunc(tranwrd(%bquote(&grouping), %str(, ), %str( ))) groupVarN;
                    var y1-y&nobs;
                run;

                proc sql;
                  %*Merge x- and y-values.;
                    create table beeSwarm as
                        select x.*, input(substr(x._name_, 2), 8.) as seq, y1
                            from x
                                    inner join
                                 y
                                    on %do j = 1 %to %sysfunc(countw(%bquote(&grouping groupVarN)));
                                        x.%scan(%bquote(&grouping groupVarN), &j) = y.%scan(%bquote(&grouping groupVarN), &j)
                                   and %end;               substr(x._name_, 2) = substr(y._name_, 2);

                  %*Merge bee swarm dataset with main dataset.;
                    create table outputDataset as
                        select a.*, x1, y1
                            from preSwarm a
                                    inner join
                                 beeSwarm b
                                    on %do j = 1 %to %sysfunc(countw(%bquote(&grouping groupVarN)));
                                        a.%scan(%bquote(&grouping groupVarN), &j) = b.%scan(%bquote(&grouping groupVarN), &j)
                                   and %end;                       a.seq = b.seq;
                quit;

              %*Output dataset with outliers.;
                data stack;
                    set stack
                        outputDataset;
                run;

            %mend outlierDataset;

    %if %nrbquote(&panelVar) ne %then %do;
        %put;
        %put %nrstr(/-------------------------------------------------------------------------------------------------\);
        %put %nrstr(  PROC SGPANEL running);
        %put %nrstr(\-------------------------------------------------------------------------------------------------/);
        %put;
    %end;
    %else %do;
        %put;
        %put %nrstr(/-------------------------------------------------------------------------------------------------\);
        %put %nrstr(  PROC SGPANEL running);
        %put %nrstr(\-------------------------------------------------------------------------------------------------/);
        %put;
    %end;

        /***--------------------------------------------------------------------------------------\
          Print PROC SGPANEL/SGPLOT statements to log
        \--------------------------------------------------------------------------------------***/

            proc sql noprint;
                select count(distinct groupVar) + .5
                  into :maxGroups separated by ' '
                    from inputDataset;
            quit;

            %if %nrbquote(&panelVar) ne %then %do;
                %put %str(    --> proc sgpanel);
                %put %str(    -->     data = outputDataset;);
                %put %str(    -->     format x1 groupVarN.;);
                %put %str(    -->     panelby &panelVar / novarname);
                %put %str(    -->         rows = &panelRows);
                    %if %sysfunc(countw(&panelVar)) = 2 %then %do;
                %put %str(    -->         columns = &panelCols);
                %put %str(    -->         layout = lattice;);
                    %end;
                    %else
                %put %str(    -->         columns = &panelCols;);
                %put %str(    -->     scatter);
                %put %str(    -->         x = x1);
                %put %str(    -->         y = y1 /);
                %put %str(    -->             markerattrs = %();
                %put %str(    -->                 color = black);
                %put %str(    -->                 symbol = circlefilled);
                %put %str(    -->                 size = 2mm%););
                %put %str(    -->     scatter);
                %put %str(    -->         x = x1);
                %put %str(    -->         y = outlier /);
                %put %str(    -->             datalabel = outlierLabel);
                %put %str(    -->             markerattrs = %();
                %put %str(    -->                 color = red);
                %put %str(    -->                 symbol = circlefilled);
                %put %str(    -->                 size = 2mm%));
                %put %str(    -->             datalabelattrs = %();
                %put %str(    -->                 weight = bold%););
                %put %str(    -->     colaxis integer);
                %put %str(    -->         min = .5);
                %put %str(    -->         max = &maxGroups);
                %put %str(    -->         fitpolicy = rotate);
                %put %str(    -->         offsetmax = 0);
                    %if %nrbquote(&groupVar) = %then %do;
                %put %str(    -->         offsetmax = 0);
                %put %str(    -->         display = none;);
                    %end;
                    %else
                %put %str(    -->         offsetmax = 0;);
                %put %str(    -->     label);
                %put %str(    -->         x1 = "&xLabel");
                %put %str(    -->         y1 = "&yLabel";);
                %put %str(    --> run;);
            %end;
            %else %do;
                %put %str(    --> proc sgplot);
                %put %str(    -->     data = outputDataset;);
                %put %str(    -->     format x1 groupVarN.;);
                %put %str(    -->     scatter);
                %put %str(    -->         x = x1);
                %put %str(    -->         y = y1 /);
                %put %str(    -->             markerattrs = %();
                %put %str(    -->                 color = black);
                %put %str(    -->                 symbol = circlefilled);
                %put %str(    -->                 size = 2mm%););
                %put %str(    -->     scatter);
                %put %str(    -->         x = x1);
                %put %str(    -->         y = outlier /);
                %put %str(    -->             datalabel = outlierLabel);
                %put %str(    -->             markerattrs = %();
                %put %str(    -->                 color = red);
                %put %str(    -->                 symbol = circlefilled);
                %put %str(    -->                 size = 2mm%));
                %put %str(    -->         datalabelattrs = %();
                %put %str(    -->             weight = bold%););
                %put %str(    -->     xaxis integer);
                %put %str(    -->         min = .5);
                %put %str(    -->         max = &maxGroups);
                %put %str(    -->         fitpolicy = rotate);
                    %if %nrbquote(&groupVar) = %then %do;
                %put %str(    -->         offsetmax = 0);
                %put %str(    -->         display = none;);
                    %end;
                    %else
                %put %str(    -->         offsetmax = 0;);
                %put %str(    -->     label);
                %put %str(    -->         x1 = "&xLabel");
                %put %str(    -->         y1 = "&yLabel";);
                %put %str(    --> run;);
            %end;

        /***--------------------------------------------------------------------------------------\
          PROC SGPANEL/SGPLOT
        \--------------------------------------------------------------------------------------***/

            %let univariatePlotFiles = ;

          %*Generate scatter plot.;
            %macro SGPLOT;

                %local groupText panelText;

                %if %nrbquote(&groupVar) ne %then %let groupText = , Grouped by &groupVar;
                %if %nrbquote(&panelVar) ne %then %let panelText = , Paneled by %sysfunc(prxchange(s/ +/%str(,) /, -1, &panelVar));

                title1 j = c "&outcomeVar&groupText&panelText";;

                %if %nrbquote(&byVar) ne %then
                    title2 j = c "%nrbquote(&byTitle)";;

                footnote1 j = c " ";
                footnote2 j = l "Outliers are labeled &IDvar/%sysfunc(lowcase(&XY))-value and identified by their studentized residual, a measure of a data point%str(%')s distance from the regression line divided by its standard deviation.";

                options
                    orientation = &orientation;
                ods listing
                    gpath = "%sysfunc(pathname(work))";

                ods results off;
                    ods graphics on /
                        reset     = all
                        border    = no
                        width     = &width  in
                        height    = &height in
                        imagename = "&outName";
                    %if %nrbquote(&byVar) ne %then %do;
                        ods rtf
                            file = "&outPath\&outName&i..rtf";
                        %let univariatePlotFiles = &univariatePlotFiles|&outName&i;
                    %end;
                    %else
                        ods rtf
                            file = "&outPath\&outName..rtf";;
                            %if %nrbquote(&panelVar) ne %then %do;
                                proc sgpanel
                                    data = outputDataset;
                                    format x1 groupVarN.;
                                    panelby &panelVar / novarname
                                        rows = &panelRows
                                        columns = &panelCols
                                            %if %sysfunc(countw(&panelVar)) = 2 %then
                                        layout = lattice;;
                                    scatter
                                        x = x1
                                        y = y1 /
                                            markerattrs = (
                                                color = black
                                                symbol = circlefilled
                                                size = 2mm);
                                    scatter
                                        x = x1
                                        y = outlier /
                                            datalabel = outlierLabel
                                            markerattrs = (
                                                color = red
                                                symbol = circlefilled
                                                size = 2mm)
                                            datalabelattrs = (
                                                weight = bold);
                                    colaxis integer
                                        min = .5
                                        max = %sysevalf(&nGroups + .5)
                                        fitpolicy = rotate
                                        offsetmax = 0
                                            %if %nrbquote(&groupVar) = %then
                                        display = none;;
                                    label
                                        x1 = "&xLabel"
                                        y1 = "&yLabel";
                                run;
                            %end;
                            %else %do;
                                proc sgplot
                                    data = outputDataset;
                                    format x1 groupVarN.;
                                    scatter
                                        x = x1
                                        y = y1 /
                                            markerattrs = (
                                                color = black
                                                symbol = circlefilled
                                                size = 2mm);
                                    scatter
                                        x = x1
                                        y = outlier /
                                            datalabel = outlierLabel
                                            markerattrs = (
                                                color = red
                                                symbol = circlefilled
                                                size = 2mm)
                                        datalabelattrs = (
                                            weight = bold);
                                    xaxis integer
                                        min = .5
                                        max = %sysevalf(&nGroups + .5)
                                        fitpolicy = rotate
                                        offsetmax = 0
                                            %if %nrbquote(&groupVar) = %then
                                        display = none;;
                                    label
                                        x1 = "&xLabel"
                                        y1 = "&yLabel";
                                run;
                            %end;
                        ods rtf close;
                    ods graphics  off;
                ods results;

            %mend  SGPLOT;

        /***--------------------------------------------------------------------------------------\
          By-group processing
        \--------------------------------------------------------------------------------------***/

            data stack;
                delete;
            run;

          %*Generate scatter plot for each combination of By-values.;
            %if %nrbquote(&byVar) ne %then %do;
                proc sql noprint;
                    select       distinct catx('~', %sysfunc(prxchange(%str(s/( +)/, /), -1, &byVar))),
                           count(distinct catx('~', %sysfunc(prxchange(%str(s/( +)/, /), -1, &byVar))))
                      into      :byVals  separated by '|',
                                :nByVals
                        from inputDataset;
                quit;

                %put %str(    --> Number of BY values: %sysfunc(left(&nByVals)));
                %put ;

                %do i = 1 %to &nByVals;
                    %let byVal = %qscan(%nrbquote(&byVals), &i, |);

                    data _null_;
                        if _n_ = 1 then do;
                            set GLMdataset;

                            length bySubset $1000;
                            call missing(bySubset);

                            %do j = 1 %to %sysfunc(countw(&byVar));
                                if vtype(%scan(&byVar, &j)) = 'N'
                                    then bySubset = catx(' and ',
                                        bySubset,
                                        catx(' = ',
                                            scan("&byVar", &j),
                                            scan("&byVal", &j, '~')));
                                    else bySubset = catx(' and ',
                                        bySubset,
                                        catx(' = ',
                                            scan("&byVar", &j),
                                            quote(scan("&byVal", &j, '~'))));

                                 byTitle =
                                    prxchange('s/ and/,/', -1,
                                    prxchange('s/"//'    , -1, bySubset));
                            %end;

                            call symputx('bySubset', bySubset);
                            call symputx('byTitle ', byTitle );
                        end;
                        stop;
                    run;

                    %let ByMessage = ;

                    %put %str(    --> Processing data subset on &bySubset..);

                    %outlierDataset;

                    %SGPLOT;

                    %if &i = &nByVals %then %put ;
                %end;
            %end;
          %*Otherwise, generate a single plot.;
            %else %do;
                %let i = ;
                %let bySubset = 1;
                %outlierDataset;

                %SGPLOT;
            %end;

            data _plots_.&outcomeVar;
                set stack;
            run;

            %if &byVar ne %then %do;
                %stackRTFs
                    (directory = &activeDirectory
                    ,files = &univariatePlotFiles
                    ,out = &outName);
            %end;

            %rtf2pdf7
                (in  = &outPath\&outName..rtf
                ,out = &outPath\&outName..pdf);

    %exit:

    %put;
    %put %nrstr(/-------------------------------------------------------------------------------------------------\);
    %put %nrstr(  %univariatePlot ending execution);
    %put %nrstr(\-------------------------------------------------------------------------------------------------/);
    %put;

  %*Reset environment.;
    %if ^%sysfunc(libref(_plots_)) %then libname _plots_ clear;;
    options &xWaitOption &notesOption &dateOption &numberOption 
        ls = &lsOption;
    title;
    footnote;

%mend  univariatePlot;
