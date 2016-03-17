/*Code modified on 09/19/2013 to use the new coordinates for the patents (based on Zip Code Centroids)*/
/***************************************************************************************************************************************************/
/*********************   								       NBER DATA					 								   *********************/
/***************************************************************************************************************************************************/
libname x 'C:\Jerry\Bob Hunt\CA SAS\Data';

PROC IMPORT OUT= work.cite76_06
            DATAFILE= "C:\Jerry\Bob Hunt\cite76_06.dta" 
            DBMS=DTA REPLACE;
RUN;
proc sort;
by cited citing;
run;
PROC IMPORT OUT= work.pat76_06_assg 
            DATAFILE= "C:\Jerry\Bob Hunt\pat76_06_assg.dta" 
            DBMS=DTA REPLACE;
RUN;
proc sort;
by patent;
run;

data x.pat76_06_assg;
	set pat76_06_assg;
run;

data x.cite76_06;
	set cite76_06;
run;

/***************************************************************************************************************************************************/
/*********************   								CLUSTER PATENTS						 								   *********************/
/***************************************************************************************************************************************************/
%macro in(dat);
PROC IMPORT OUT= WORK.&dat 
            DATAFILE= "C:\Jerry\Bob Hunt\Zip Codes\CA buffer shapes files\&dat..dbf" 
            DBMS=DBF REPLACE;
     GETDELETED=NO;
RUN;
data &dat (keep = Appyear Patent);
retain patent appyear;
set &dat;
run;
proc sort nodupkey;
by patent;
run;
data &dat;
	set &dat;
	&dat=1;
run;
%mend;
%in(SD1);
%in(LA1A);
%in(SF1A);
%in(SF1B);
%in(SF1C);
%in(SF1D);
%in(SF2A);
%in(SF2B);
%in(SF2C);
%in(SF2D);
%in(SF2E);
%in(SB2);
%in(LA2A);
%in(LA2B);
%in(SD2A);
%in(SD2B);
%in(LA5);
%in(SB5);
%in(SD5);
%in(SF5A);
%in(SF5B);
%in(SF5C);
%in(LA10A);
%in(LA10B);
%in(SB10);
%in(SD10);
%in(SF10);
%in(SD15);
%in(LA15);
%in(SF15);
%in(SB15);
%in(SF20);
%in(SB20);
%in(SD20);

data one;
set SD1 LA1A SF1A SF1B SF1C SF1D;
run;
proc sort nodupkey;
by patent;
run;

data two;
set SF2A SF2B SF2C SF2D SF2E SB2 LA2A LA2B SD2A SD2B;
run;
proc sort nodupkey;
by patent;
run;
	
data five;
set LA5 SB5 SD5 SF5A SF5B SF5C;
run;
proc sort nodupkey;
by patent;
run;

data ten;
set LA10A LA10B SB10 SD10 SF10;
run;
proc sort nodupkey;
by patent;
run;

data fifteen;
set SD15 LA15 SF15 SB15;
run;
proc sort nodupkey;
by patent;
run;

data twenty;
set SF20 SB20 SD20;
run;
proc sort nodupkey;
by patent;
run;

data clustpatents (drop=appyear);
merge one two five ten fifteen twenty;
by patent;
array nums _numeric_;
do over nums;
if nums=. then nums=0;
end;
run;
proc sort nodupkey;
by patent;
run;

data x.clustpatents;
	set clustpatents;
run;

/***************************************************************************************************************************************************/
/*********************   								ORIGINATING PATENTS					 								   *********************/
/***************************************************************************************************************************************************/

data bigmerge;
retain patent appyear gyear allcites pdpass nclass;
merge clustpatents (in=a keep=patent) pat76_06_assg(keep = patent appyear gday gmonth gyear allcites pdpass nclass);
by patent;
if a; 
sasdate=mdy(gmonth,gday,gyear);
run;
proc sort nodupkey;
by patent;
run;
data originating;
set bigmerge;
if APPYEAR GT 1995 and APPYEAR LT 1998;
if pdpass=0 then delete;
if pdpass=. then delete;
rename patent = cited; label patent = ' ';
rename appyear=o_appyear; label appyear = ' ';
rename gyear=o_gyear; label gyear = ' ';
rename gmonth=o_gmonth; label gmonth = ' ';
rename gday=o_gday; label gday = ' ';
rename pdpass=o_pdpass;	label pdpass = ' ';
rename nclass=o_nclass;	label nclass = ' ';
rename sasdate=o_sasdate;
label allcites=' ';
run;
proc sort;
by cited;
run;

data x.originating;
	set originating;
run;

/***************************************************************************************************************************************************/
/*********************   								       CITATIONS					 								   *********************/
/***************************************************************************************************************************************************/

data origcites (keep = cited patent o_pdpass o_appyear o_gday o_gmonth o_gyear o_sasdate);
retain cited citing o_pdpass o_appyear o_gday o_gmonth o_gyear o_sasdate;
merge originating (in = a) cite76_06;
by cited;
if a;
if allcites GT 0;
rename citing = patent;
run;
proc sort nodupkey;
by patent cited;
run;
proc sort;
by patent;
run;

data citations;
retain cited patent o_pdpass pdpass o_appyear o_gday o_gmonth o_gyear o_sasdate appyear gyear nclass;
merge origcites (in=a) pat76_06_assg (keep = pdpass nclass patent appyear gday gmonth gyear);
by patent;
if a;
if o_pdpass NE pdpass;
if pdpass=. then delete;
if pdpass=0 then delete;
sasdate=mdy(gmonth,gday,gyear);
label appyear = ' ';
label gyear = ' ';
label gmonth = ' ';
label gday = ' ';
label pdpass = ' ';
label nclass = ' ';
label cited = ' ';
label patent = ' ';
run;
proc sort data = citations nodupkey;
by patent cited;
run;
proc sort;
by patent;
run;

data x.citations;
	set citations;
run;

