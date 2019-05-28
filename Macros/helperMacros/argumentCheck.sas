%macro argumentCheck
    (parameterName
    ,values
    ,default) / minoperator;

    %global argumentCheck;
    %let argumentCheck = 1;

    %if %nrbquote(&&&parameterName) ne %then %do;
        %let &parameterName = %upcase(%nrbquote(&&&parameterName));

        %if not (%nrbquote(&&&parameterName) in &values) %then %do;
            %put %str(    --> [ &parameterName ] can only take values of);
            %put %str(    -->     &values..);

            %if %nrbquote(&default) ne %then %do;
                %put %str(    -->     Defaulting to &default..);
                %let &parameterName = &default;
            %end;
            %else %let argumentCheck = 0;
        %end;
    %end;
    %else %if %nrbquote(&default) ne %then %do;
        %put %str(    --> [ &parameterName ] unspecified.  Defaulting to &default..);
        %let &parameterName = &default;
    %end;
    %else %let argumentCheck = 0;

%mend  argumentCheck;
