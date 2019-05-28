/*----------------------- Copyright 2015, Rho, Inc.  All rights reserved. ------------------------\

  Project:      Statistical data checks

    Program:    groupComparisons.sas

      Purpose:  Generate grouped box plots and heat maps to compare the variances of an outcome
                between groups

      Macros:   %dataExist, %variableExist, %directoryExist, %argumentCheck, %stackRTFs, %RTF2PDF7

    /---------------------------------------------------------------------------------------------\
      Parameters:
    \---------------------------------------------------------------------------------------------/

      [REQUIRED]

        [ data ]        - one- or two-level input dataset
        [ groupVar ]    - group variable
        [ outcomeVar ]  - outcome variable
        [ discVar ]     - discrete covariates
                            > if [discVar] is not specified, [ contVar ] is required
        [ contVar ]     - continuous covariates
                            > if [contVar] is not specified, [ discVar ] is required

      [optional]

        [ byVar ]       - by variable
        [ outPath ]     - output directory
        [ outName ]     - output file name
        [ cutoff ]      - group comparison significance threshold
                            + defaults to 90
        [ multTest ]    - multiple comparisons method
                            + defaults to HOLM
        [ orientation ] - figure orientation [ LANDSCAPE/PORTRAIT ]
                            + defaults to LANDSCAPE

    /---------------------------------------------------------------------------------------------\
      Examples:
    \---------------------------------------------------------------------------------------------/

        %groupComparisons
            (data = DERIVE.ADSL
            ,groupVar = SITEID
            ,outcomeVar = AGE);

/-------------------------------------------------------------------------------------------------\
  Program history:
\-------------------------------------------------------------------------------------------------/

    Date        Programmer          Description
    ----------  ------------------  --------------------------------------------------------------
    2015-08-01  Spencer Childress   Create
    2017-06-30  Spencer Childress   Don't calculate number of combinations with macro language (
                                    overflow will occur)

\------------------------------------------------------------------------------------------------*/

%macro groupComparisons
    (data        = 
    ,groupVar    = 
    ,outcomeVar  = 
    ,discVar     = 
    ,contVar     = 
    ,byVar       = 
    ,outPath     = 
    ,outName     = 
    ,cutoff      = 90
    ,multTest    = HOLM
    ,orientation = LANDSCAPE
    ) / minoperator;

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

  %*Instantiate macro variable to contain list of output files.;
    %let groupComparisonsFiles = ;

    %put;
    %put %nrstr(/-------------------------------------------------------------------------------------------------\);
    %put %nrstr(  %groupComparisons beginning execution);
    %put %nrstr(\-------------------------------------------------------------------------------------------------/);
    %put;

        /***--------------------------------------------------------------------------------------\
          Parameter error checks
        \--------------------------------------------------------------------------------------***/

          %*[ data ];
            %dataExist(data);
                %if not &dataExist %then %goto exit;

          %*[ groupVar/outcomeVar/coVar/byVar ];
            %let coVar = &discVar &contVar;
            %variableExist(&data,   groupVar, 1);
                %if not &variableExist %then %goto exit;
            %variableExist(&data, outcomeVar, 1);
                %if not &variableExist %then %goto exit;
            %variableExist(&data,      coVar, 1, Model requires at least one covariate.  Execution terminating.);
                %if not &variableExist %then %goto exit;
            %variableExist(&data,      byVar);
                %if not &variableExist %then %goto exit;

          %*[ outPath ];
            %directoryExist(outPath,_plots_);
                %if not &directoryExist %then %goto exit;

          %*[ outName ];
            %if %nrbquote(&outName) = %then %do;
                %let outName = Group Comparisons - &outcomeVar by &groupVar;

                %put %str(    --> outName defaulting to &outName..);
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
                %put %str(    -->     Defaulting to a significance threshold of 90.);

                %let cutoff = 90;

            %goodCutoff:
                %let alpha = %sysfunc(strip(%sysevalf(1 - &cutoff/100)));

          %*[ orientation ];
            %let orientation = %upcase(%nrbquote(&orientation));

            %if not (%nrbquote(&orientation) in LANDSCAPE PORTRAIT) or %nrbquote(&orientation) = %then %do;
                %put %str(    --> [ orientation ] parameter unspecified or specified incorrectly.);
                %put %str(    -->     Defaulting to landscape.);

                %let orientation = LANDSCAPE;
            %end;

            %if &orientation = LANDSCAPE %then %do;
                %let width  = 10;
                %let height = 7.5;
            %end;
            %else %if &orientation = PORTRAIT %then %do;
                %let width  = 7.5;
                %let height = 10;
            %end;

        /**-------------------------------------------------------------------------------------------\
          Data manipulation
        \-------------------------------------------------------------------------------------------**/

          %*Determine &groupVar variable type.;
            data _null_;
                if _n_ = 1 then do;
                    set &data;

                    %if %nrbquote(&byVar) ne %then %do;
                        byVar = &byVar;
                        call symputx('byVarType'    , vtype(&byVar));
                        call symputx('byVarLabel'   , coalescec(      vlabel(&byVar),
                                                                upcase(vname(&byVar))));
                    %end;
                    %else %do;
                        byVar = '';
                        call symputx('byVarType'    , vtype(byVar));
                        call symputx('byVarLabel'   , coalescec(      vlabel(byVar),
                                                                upcase(vname(byVar))));
                    %end;
                        call symputx('groupVarType' , vtype(&groupVar));
                        call symputx('groupVarLabel', coalescec(      vlabel(&groupVar),
                                                                upcase(vname(&groupVar))));
                        call symputx('varLabel'     , coalescec(      vlabel(&outcomeVar),
                                                                upcase(vname(&outcomeVar))));
                        call symputx
                            ('CovariateLabels'
                            ,catx
                                (', '
                                %do i = 1 %to %sysfunc(countw(&coVar));
                                    %let covariate = %scan(&coVar, &i);
                                    ,coalescec(vlabel(&covariate), upcase(vname(&covariate)))
                                %end;
                                ));
                end;
                stop;
            run;

          %*Sort by &groupVar and derive numeric group ordering variable.;
            proc sort
                data = &data (
                    keep = &groupVar &outcomeVar &discVar &contVar &byVar
                    where = (&outcomeVar gt .z))
                out = inputDataset;
                by &groupVar;
            data inputDataset;
                set inputDataset;

                %if %nrbquote(&byVar) ne
                    %then byVar = &byVar;
                    %else byVar = '';;
            run;

        /***--------------------------------------------------------------------------------------\
          By-group processing
        \--------------------------------------------------------------------------------------***/

            proc sql noprint;
                select distinct  byVar
                  into          :byValues separated by %if &byVarType = C %then '"|"'; %else '|';
                    from inputDataset;
                %let nbys = &SQLObs;
            quit;

            %if &byVarType = C %then %let byValues = "&byValues";

            %do by = 1 %to &nbys;
                %let byValue = %scan(%nrbquote(&byValues), &by, |);

                %if %nrbquote(&byVar) ne %then %put %str(    --> Processing &byVar = &byValue..);

                proc sql noprint;
                    select  count(distinct &groupVar)
                      into :nGroups
                        from inputDataset
                          where byVar = &byValue;
                quit;

                %if &nGroups = 1 %then %goto oneGroup;

                data _&by (keep = &outcomeVar byVar &groupVar &groupVar._Ordered &discVar &contVar);
                    set inputDataset (
                            where = (byVar = &byValue))
                            end = eof;
                     by &groupVar;

                    length groupValues $1000 groupOrder $100;
                    retain groupValues       groupOrder     ;

                    if first.&groupVar then do;
                        &groupVar._Ordered + 1;

                        %if &groupVarType = C
                            %then groupValues = catx('|', groupValues, quote(strip(&groupVar)));
                            %else groupValues = catx('|', groupValues, put(&groupVar, best.));;
                                  groupOrder  = catx(' ', groupOrder , put(&groupVar._Ordered, 8.));
                    end;

                    if eof then do;
                        nCombinations = fact(&groupVar._Ordered)/(2*fact(&groupVar._Ordered - 2));

                        call symputx('nGroups'      , strip(put(&groupVar._Ordered, 8.)));
                        call symputx('groupValues'  , groupValues);
                        call symputx('groupOrder'   , groupOrder);
                        call symputx('nCombinations', strip(put(nCombinations, 8.)));
                        call symputx('YLabel'       , vlabel(&outcomeVar));
                        call symputx('XLabel'       , catx(' ', vlabel(&groupVar), '(n)'));

                        put 'Number of groups      : ' &groupVar._Ordered;
                        put '          Groups      : ' groupValues;
                        put 'Number of combinations: ' nCombinations;

                        do i = 1 to &groupVar._Ordered;
                            if (&groupVar._Ordered - i) then do j = (i + 1) to &groupVar._Ordered;
                                GroupValue1   = scan(groupValues, i, '|');
                                GroupValue2   = scan(groupValues, j, '|');
                                GroupValue1_2 = catx('|', GroupValue1, GroupValue2);

                                call symputx(catx('_', 'Comparison',
                                                       put(i, 8.)  ,
                                                       put(j, 8.)  ),
                                             GroupValue1_2);

                                if i = 1 and j = 2 then put '          Combinations: ' GroupValue1_2;
                                                   else put '                        ' GroupValue1_2;
                            end;
                        end;
                    end;
                run;

              %*Capture group standard deviations.;
                proc means noprint nway
                    data = _&by;
                    class  &groupVar;
                    var    &outcomeVar;
                    output out = nSD
                             n = n
                           std = SD;
                run;

              %*Generate format to label groups.;
                proc sort
                    data = nSD;
                    by SD &groupVar;
                run;

                data SDOrder_n_;
                    set nSD;

                    SDOrder = _n_;
                    start   = _n_ - .5;
                    end     = _n_ + .5;
                    label   = catx(' (', &groupVar, cats(put(n, 8.), ')'));
                    fmtname = 'SDOrder_n_';
                run;

                data SDOrder;
                    length label $200;
                    set nSD end = eof;

                    start   = _n_;
                    end     = _n_;
                    label   = &groupVar;
                    fmtname = 'SDOrder';
                    output;

                    if eof then do;
                            start = 0;
                            end   = 0;
                            label = 'All other groups';
                        output;
                    end;
                run;

                data Formats;
                    set SDOrder_n_
                        SDOrder;
                run;

                proc format
                    cntlin = Formats;
                    value $Group
                        %do i = 1 %to &nGroups;
                            %scan(&groupValues, &i, |) = "%scan(&groupOrder, &i)"
                        %end;
                    ;
                    value Group
                        %do i = 1 %to &nGroups;
                            %sysevalf(%scan(&groupOrder, &i) - .5) - <
                            %sysevalf(%scan(&groupOrder, &i) + .5) = %scan(&groupValues, &i, |)
                        %end;
                        0 = 'All other groups'
                    ;
                run;

              %*Determine number of combinations.;
                %*let nCombinations = %eval(%sysfunc(fact(&nGroups))/(2*%sysfunc(fact(&nGroups - 2))));
                %*put &=nCombinations;

            /****---------------------------------------------------------------------------------\
              Statistics
            \---------------------------------------------------------------------------------****/

              %*Capture scaled residuals.;
                proc sort
                    data = _&by;
                    by &groupVar &discVar;
                proc mixed
                    data = _&by;
                    class &groupVar &discVar;
                    model
                        &outcomeVar = &discVar &contVar / vciry
                            outpm = Mixeddataset;
                    random intercept /
                        subject = &groupVar;
                run;

                proc sql noprint;
                    create table ScatterPlot as
                        select a.*, SDOrder,
                            SDOrder + ranuni(2357)*(-1)**monotonic()/4 as Jitter label = "&XLabel"
                                                                                format = SDOrder_n_.
                            from    Mixeddataset a
                                left join
                                    SDOrder_n_ b
                                on a.&groupVar = b.&groupVar;
                    select distinct
                            SDOrder,
                            %if &groupVarType = C
                                %then quote(strip(&groupVar));
                                %else             &groupVar  ;
                      into
                            :SDgroupOrders separated by ' ',
                            :SDgroupValues separated by '|'
                        from ScatterPlot
                    order by SDOrder;
                quit;

              %*Output one record per comparison.;
                data BinaryGroups;
                    length GroupC $1000 Group $100;
                    set ScatterPlot;

                    %do i = 1 %to &nGroups;
                        %let SDGroupValue = %scan(&SDgroupValues, &i, |);

                            GroupC = catx(' | ', &SDGroupValue, 'All other groups');
                            Group  = "&i";
                            if &groupVar ne &SDGroupValue then Binary = 0;
                                                          else Binary = 1;
                        output;
                          call missing(Group, Binary);

                        %if %eval(&nGroups - &i) %then %do j = %eval(&i + 1) %to &nGroups;
                            %let SDGroupValue1 = %scan(&SDgroupValues, &i, |);
                            %let SDGroupValue2 = %scan(&SDgroupValues, &j, |);

                                if &groupVar in (&SDGroupValue1 &SDGroupValue2) then do;
                                        GroupC = catx(' | ', &SDGroupValue1, &SDGroupValue2);
                                        Group  = "&i._&j";
                                        if &groupVar = &SDGroupValue2 then Binary = 0;
                                                                      else Binary = 1;
                                    output;
                                      call missing(Group, Binary);
                                end;
                        %end;
                    %end;
                run;

                proc sort
                    data = BinaryGroups;
                    by Group descending Binary;
                run;

              %*Capture homogeneity of variance statistics.;
                proc glm
                    data = BinaryGroups;
                    by Group GroupC;
                    class Binary;
                    model ScaledResid = Binary;
                    means Binary /
                        hovtest = bf;
                    ods output
                        HoVFTest = HoVFTest1 (where = (Source = 'Binary'));
                run;

                proc multTest &multTest
                    inpvalues = HoVFtest1 (rename = (ProbF = Raw_P));
                    ods output
                        pValues = multTest;
                run;

                proc sql noprint;
                    select  name
                      into :pValueVar
                        from dictionary.columns
                          where memname = 'MULTTEST'
                            and    name not in ('Raw' 'Test');
                quit;

                data HoVFTest2;
                    merge
                        multTest (rename = (&pValueVar = pValue))
                        HoVFTest1;
                run;

              %*Capture within group standard deviations.;
                proc means noprint nway
                    data = BinaryGroups;
                    by Group;
                    Class Binary;
                    var &outcomeVar;
                    output
                        out = SD
                        std = SD;
                run;

              %*Collapse standard deviation dataset to one record per group.;
                data SD1 (keep = Group SD1 SD2);
                    set SD;
                    by Group;

                    retain SD2;

                    if first.Group then SD2 = SD;
                    else do;
                            SD1 = SD;
                        output;
                    end;
                run;

              %*Merge HoV and SD datasets.;
                proc sql;
                    create table HoVFTest_SD as
                        select a.*, SD1, SD2
                            from    HoVFTest2 a
                                left join
                                    SD1 b
                                on a.Group = b.Group;
                quit;

              %*Generate heat map dataset.;
                data HeatMap (
                    keep   = _x_ _y_ pValue SDa SDb
                    rename = (pValue = Heat));
                    set HoVFTest_SD (in = a)
                        HoVFTest_SD (in = b);

                    if (a and ^index(Group, '_')) or
                       (b and  index(Group, '_')) then call missing(pValue, SD1, SD2);

                    format _x_ _y_ SDOrder.;

                    if a then do;
                        _x_  = input(scan(Group, 1, '_'), 8.);
                        _y_  = input(scan(Group, 2, '_'), 8.);
                        SDa = SD2;
                        SDb = SD1;
                    end;
                    else do;
                        _x_  = input(scan(Group, 2, '_'), 8.);
                        _y_  = input(scan(Group, 1, '_'), 8.);
                        SDa = SD1;
                        SDb = SD2;
                    end;

                    if missing(_x_) then _x_ = 0;
                    if missing(_y_) then _y_ = 0;
                run;

              %*Generate annotation dataset.;
                data Annotate1 (drop = Heat SDa SDb);
                    length label $100;
                       set HeatMap;

                    id         = 'pSD';
                    drawspace  = 'datavalue';
                    function   = 'text';
                    textcolor  = 'black';
                    textweight = 'bold';
                    textsize   = 8;
                    x1         = _x_;

                        y1         = _y_ + .25;
                        label      = ifc(SDa gt .z, strip(put(SDa, 8.2)), 'NA');
                    output;
                        y1         = _y_;
                        label      = ifc(SDb gt .z, strip(put(SDb, 8.2)), 'NA');
                    output;
                        y1         = _y_ - .25;
                        label      = cats(ifc(.z lt Heat, strip(put(Heat, 8.3)), 'NA'), ifc(.z lt Heat lt &alpha, '*', ''));
                    output;
                run;

                data Annotate2;
                    length label $100;

                    id        = 'pSD';
                    drawspace = 'datavalue';
                    function  = 'text';
                    textsize  = 8;

                        x1         = &nGroups - 1;
                        y1         = 1.25;
                        textweight = 'bold';
                        label      = "(*ESC*){unicode '03C3'x} row";
                    output;
                        x1         = &nGroups - 1;
                        y1         = 1;
                        textweight = 'bold';
                        label      = "(*ESC*){unicode '03C3'x} col";
                    output;
                        x1         = &nGroups - 1;
                        y1         = .75;
                        textweight = 'bold';
                        label      = "p-value";
                    output;
                run;

                data Annotate;
                    set Annotate1 (where = (_x_ lt &nGroups and _x_ le _y_))
                        Annotate2;
                run;

                data ForSGRender;
                    set ScatterPlot
                        HeatMap (where = (_x_ lt &nGroups and _x_ le _y_));
                run;

            /****---------------------------------------------------------------------------------\
              Figure generation
            \---------------------------------------------------------------------------------****/

                proc template;
                    define statgraph BoxPlot_HeatMap;
                        dynamic _X _Y _Z;

                        begingraph;
                            entrytitle                  "&varLabel Grouped by &groupVarLabel";
                            %if %nrbquote(&byVar) ne %then
                            entrytitle                  "&byVarLabel = %nrbquote(&byValue)";;
                            entryfootnote halign = left "&groupVarLabel displays from left to right in order of ascending variance.";
                            entryfootnote halign = left "Asterisks indicate significant group variance comparisons after controlling for &CovariateLabels and multiple comparisons.";

                            rangeattrmap name = 'p';
                                %if %sysevalf(&alpha lt .5) %then %do;
                                    range missing /
                                        rangecolormodel = (white white);
                                    range 0 <-< &alpha /
                                        rangecolormodel = (darkred red);
                                    range &alpha -< %sysevalf(2*&alpha) /
                                        rangecolormodel = (red lightred);
                                    range %sysevalf(2*&alpha) <- 1 /
                                        rangecolormodel = (lightred white);
                                %end;
                                %else %do;
                                    range missing /
                                        rangecolormodel = (white white);
                                    range 0 <-< &alpha /
                                        rangecolormodel = (darkred red);
                                    range &alpha -< 1 /
                                        rangecolormodel = (red white);
                                %end;
                            endrangeattrmap;

                            rangeattrvar
                                attrvar = asdf
                                var = _Z
                                attrmap = 'p';

                            layout lattice /
                                rows          = %sysfunc(ifn(&orientation = PORTRAIT, 2, 1))
                                columns       = %sysfunc(ifn(&orientation = PORTRAIT, 1, 2))
                                rowgutter     = 10
                                columngutter  = 10
                                columnweights = (0.4 0.6);

                              %*Box-and-whisker plots;
                                layout overlay /
                                    xaxisopts = (
                                        linearopts = (
                                            tickvaluelist      = (%do i = 1 %to &nGroups; &i %end;)
                                            tickvaluefitpolicy = rotatethin));
                                    scatterplot
                                        x = Jitter
                                        y = &outcomeVar;
                                    boxplot
                                        x = SDOrder
                                        y = &outcomeVar /
                                            display   = (caps mean median)
                                            meanattrs = (symbol = diamondfilled);
                                endlayout;
                              %*Box-and-whisker plots;

                              %*Heat map;
                                layout overlay /
                                    xaxisopts = (
                                        display = (ticks tickvalues line)
                                        linearopts = (
                                            tickvaluelist = (%do i = 0 %to &nGroups; &i %end;)
                                            tickvaluefitpolicy = rotatethin))
                                    yaxisopts = (
                                        display = (ticks tickvalues line)
                                        linearopts = (
                                            tickvaluelist = (%do i = 0 %to &nGroups; &i %end;)));
                                    heatmapparm
                                        x = _X
                                        y = _Y
                                        colorresponse = asdf /
                                            name              = "heatmap"
                                            primary           = true
                                            xbinaxis          = false
                                            ybinaxis          = false
                                            reversecolormodel = true;
                                    continuouslegend "heatmap";
                                    annotate /
                                        id = 'pSD';
                                endlayout;
                              %*Heat map;

                            endlayout;
                        endgraph;
                    end;
                run;

                options
                    orientation = &orientation;
                ods listing
                    gpath = "%sysfunc(pathname(work))";

                title;
                footnote;

                ods results off;
                    ods graphics /
                        reset     = all
                        border    = off
                        scale     = off
                        width     = &width  in
                        height    = &height in
                        outputfmt = emf;

                    %if &byVar ne %then %do;
                        ods rtf
                            file = "&activeDirectory\&outName&by..rtf";
                        %let groupComparisonsFiles = &groupComparisonsFiles|&outName&by;
                    %end;
                    %else
                        ods rtf
                            file = "&activeDirectory\&outName..rtf";;
                            proc sgrender
                                data = ForSGRender
                                template = BoxPlot_HeatMap
                                sganno = Annotate;
                                dynamic
                                    _x = '_X_'
                                    _y = '_Y_'
                                    _z = 'Heat';
                            run;
                        ods rtf close;

                    ods graphics off;
                ods results;

                %goto endBy;

            %oneGroup:

                %if &byVar ne
                    %then %put %str(    --> &data contains only one group where &byVar = &byValue..  No output will be generated.);
                    %else %put %str(    --> &data contains only one group.  No output will be generated.);

            %endBy:

        %end;

        %if &byVar ne %then %do;
            %stackRTFs
                (directory = &activeDirectory
                ,files = &groupComparisonsFiles
                ,out = &outName);
        %end;

        %rtf2pdf7
            (in  = &activeDirectory\&outName..rtf
            ,out = &activeDirectory\&outName..pdf);

    %exit:

    %put;
    %put %nrstr(/-------------------------------------------------------------------------------------------------\);
    %put %nrstr(  %groupComparisons ending execution);
    %put %nrstr(\-------------------------------------------------------------------------------------------------/);
    %put;

  %*Reset environment.;
    %if ^%sysfunc(libref(_plots_)) %then libname _plots_ clear;;
    options &xWaitOption &notesOption &dateOption &numberOption 
        ls = &lsOption;
    title;
    footnote;

%mend  groupComparisons;
