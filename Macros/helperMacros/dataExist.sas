%macro dataExist(_data_);

    %global dataExist;
    %let dataExist = 1;

    %if %nrbquote(&&&_data_) = %then %do;
        %put %str(    --> <&_data_> parameter unspecified.  Execution terminating.);
        %let dataExist = 0;
    %end;
    %else %if not %sysfunc(exist(&&&_data_)) %then %do;
        %put %str(    --> &&&_data_ does not exist.  Execution terminating.);
        %let dataExist = 0;
    %end;

%mend  dataExist;
