%macro variableExist
    (data
    ,vars
    ,required
    ,message);

    %local var;
    %global variableExist;
    %let variableExist = 1;

    %if &required ne and %nrbquote(&&&vars) = %then %do;
        %if %nrbquote(&Message) =
            %then %put %str(    --> <&vars> parameter unspecified.  Execution terminating.);
            %else %put %str(    --> &Message);
        %let variableExist = 0;
    %end;
    %else %if %nrbquote(&&&vars) ne %then %do;
        %let &vars = %upcase(%sysfunc(prxchange(%str(s/\s{2,}/ /), -1, %nrbquote(&&&vars))));
        %let dataID = %sysfunc(open(&data));

        %do i = 1 %to %sysfunc(countw(&&&vars));
            %let var = %scan(&&&vars, &i, %str( *));

            %if %sysfunc(prxmatch(%str(/^[_a-z][_a-z0-9]* *$/i), &var)) = 0 %then %do;
                %put %str(    --> &var contains an invalid character.  Execution terminating.);
                %let variableExist = 0;
            %end;
            %else %if %sysfunc(varnum(&dataID, &var)) = 0 %then %do;
                %put %str(    --> &data does not contain &var..  Execution terminating.);
                %let variableExist = 0;
            %end;
        %end;

        %let rc = %sysfunc(close(&dataID));
    %end;

%mend  variableExist;
