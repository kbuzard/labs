libname x '/sasr/Scavette/CA Good/Data Subclass';

/*Import Inventor Dataset from Harvard Dataverse*/

proc import datafile='/sasr/Scavette/inventors5s_7595.tab'
     out=inventors
     dbms=dlm
     replace;
     delimiter='09'x;
     datarow=2;
run;

proc import datafile='/sasr/Scavette/inventors5s_9608.tab'
     out=inventors_2
     dbms=dlm
     replace;
     delimiter='09'x;
     datarow=2;
run;

proc datasets library=work;
	append base=inventors data=inventors_2 force;
run;

data inventor (keep= patent invnum);
	set inventors;
	if inv_seq~=1 then delete;
	rename invnum_n=invnum;
run;

proc sort;
	by patent;
run;

data x.inventor;
	set inventor;
run;

/*Generate Dataset of Potential Control Patents
PROC IMPORT OUT= WORK.pat76_06_assg 
            DATAFILE= "C:\Jerry\Bob Hunt\pat76_06_assg.dta" 
            DBMS=DTA REPLACE;
RUN;
proc sort;
by patent;
run;
*/

data possible;
set x.pat76_06_assg;
*if country ~= "US" then delete;
*drop country;
run;
proc sort;
by patent;
run;

data possiblenclass (keep=patent appyear gyear gmonth gday nclass pdpass invnum sasdate);
retain patent appyear gyear gmonth gday nclass pdpass invnum;
merge possible inventor;
if APPYEAR GT 1995;
label patent=' ';
sasdate=mdy(gmonth,gday,gyear);
run;
proc sort nodupkey;
by patent;
run; 

/*Bring in Citing Dataset to see what patents the possiblenclass patents cited*/
/*We are merging this possiblenclass dataset with the citing set

PROC IMPORT OUT= WORK.cite76_06
            DATAFILE= "C:\Jerry\Bob Hunt\cite76_06.dta" 
            DBMS=DTA REPLACE;
RUN;*/


data cite76_06;
	set x.cite76_06;
	rename citing=patent;
run;
proc sort;
by patent cited;
run;

data controls_cites (keep= patent cited);
	merge cite76_06 (in=a) possiblenclass (in=b);
	by patent;
	if a and b;
	label patent= ' ';
	label cited= ' ';
run;

proc sort nodupkey;
	by patent cited;
run;

data controls_cites;
	set controls_cites;
	count + 1;
	by patent;
	if first.patent then count=1;
run;

data controls_cites (keep = patent cite1--cite137);
array cite(1000);
do until (last.patent);
set controls_cites;
by patent;
cite(count) = cited;
end;
run;

proc sort;
	by patent;
run;

data possiblenclass;
	merge possiblenclass (in=a) controls_cites (in=b);
	by patent;
	if a;
run;

/* */

data possiblenclass;
	set possiblenclass;
	rename appyear=c_appyear;
	rename gyear=c_gyear;
	rename patent=control;
	rename pdpass=c_pdpass;
	rename invnum=c_invnum;
	rename sasdate=c_sasdate;
	label appyear=' ';
	label gyear=' ';
	label nclass=' ';
	label pdpass=' ';
	x_control=RAND('UNIFORM');
run;

/*Bring in the citations dataset*/

data citations;
	set x.citations;
	label cited = ' ';
	label patent = ' ';
	x_citat=RAND('UNIFORM');
run;

/* Add Inventor Data for Citing Patents to Citation Dataset*/
proc sort data=citations;
	by patent;
run;

data citations;
	retain cited patent o_pdpass pdpass o_appyear o_gyear o_sasdate sasdate appyear gyear nclass;
	merge citations (in=a) inventor;
	by patent;
	if a;
run;

/*Add Inventor Data for Cited Patents to Citation Dataset*/

data inventor;
	set inventor;
	rename patent=cited;
	rename invnum=o_invnum;
run;

proc sort data=citations;
	by cited;
run;

data citations;
	merge citations (in=a) inventor;
	by cited;
	if a;
run;

/*Sort Potential Controls & Citations datasets by nclass and a randomly generated variable*/

proc sort data=possiblenclass;
	by nclass x_control;
run;

proc sort data=citations;
	by nclass x_citat;
run;

/*Merge these datasets according to specified criteria below:*/
/* 1. Assignees (PDPASS) for control, citing, and originating cannot match*/
/* 2. Inventor (invnum) for control, citing, and originating cannot match*/
/* 3. Patent numbers (Patent/Control/Cited) for control, citing, and originating cannot match*/

data matching;
	merge citations (in=a) possiblenclass (in=b);
	by nclass;
	if a and b;
	if pdpass~=c_pdpass~=o_pdpass;
	if invnum~=c_invnum~=o_invnum;
	if patent~=control~=cited;
	if cited~=cite1--cite137;
	/*Statement below is 1st date restriction 
	(Control Patent Application Date is same or 
	later than year of Issue date for originating patent)*/
	if c_appyear>=o_gyear;
	if abs(sasdate-c_sasdate)<=182;
run;

proc sort nodupkey data=matching;
	by cited patent;
run;

/*Begin WITH REPLACEMENT Matching: #1*/

data possiblenclass;
	set possiblenclass (drop=x_control);
	x_control=RAND('UNIFORM');
run;

proc sort data=possiblenclass;
	by nclass x_control;
run;
	
data matching1 (keep= cited patent binvar);
	set matching;
	binvar=1;
run;

proc sort data=citations;
	by cited patent;
run;

data matching2;
	merge citations (in=a) matching1;
	by cited patent;
	if a;
run;

data matching2;
	set matching2;
	if binvar=1 then delete;
run;

proc sort data=matching2;
	by nclass;
run;

data matching2;
	merge matching2 (in=a) possiblenclass (in=b);
	by nclass;
	if a and b;
	if pdpass~=c_pdpass~=o_pdpass;
	if invnum~=c_invnum~=o_invnum;
	if patent~=control~=cited;
	if cited~=cite1--cite137;
	if c_appyear>=o_gyear;
	if abs(sasdate-c_sasdate)<=182;
run;

proc sort nodupkey data=matching2;
	by cited patent;
run;

proc datasets library=work;
	append base=matching data=matching2 force;
run;

proc sort;
	by cited patent;
run;

proc datasets library=work;
   delete matching1 matching2;
run;

/*Begin WITH REPLACEMENT Matching: #2*/

data possiblenclass;
	set possiblenclass (drop=x_control);
	x_control=RAND('UNIFORM');
run;

proc sort data=possiblenclass;
	by nclass x_control;
run;
	
data matching1 (keep= cited patent binvar);
	set matching;
	binvar=1;
run;

proc sort data=citations;
	by cited patent;
run;

data matching2;
	merge citations (in=a) matching1;
	by cited patent;
	if a;
run;

data matching2;
	set matching2;
	if binvar=1 then delete;
run;

proc sort data=matching2;
	by nclass;
run;

data matching2;
	merge matching2 (in=a) possiblenclass (in=b);
	by nclass;
	if a and b;
	if pdpass~=c_pdpass~=o_pdpass;
	if invnum~=c_invnum~=o_invnum;
	if patent~=control~=cited;
	if cited~=cite1--cite137;
	if c_appyear>=o_gyear;
	if abs(sasdate-c_sasdate)<=182;
run;

proc sort nodupkey data=matching2;
	by cited patent;
run;

proc datasets library=work;
	append base=matching data=matching2 force;
run;

proc sort;
	by cited patent;
run;

proc datasets library=work;
   delete matching1 matching2;
run;

/*Begin WITH REPLACEMENT Matching: #3*/

data possiblenclass;
	set possiblenclass (drop=x_control);
	x_control=RAND('UNIFORM');
run;

proc sort data=possiblenclass;
	by nclass x_control;
run;
	
data matching1 (keep= cited patent binvar);
	set matching;
	binvar=1;
run;

proc sort data=citations;
	by cited patent;
run;

data matching2;
	merge citations (in=a) matching1;
	by cited patent;
	if a;
run;

data matching2;
	set matching2;
	if binvar=1 then delete;
run;

proc sort data=matching2;
	by nclass;
run;

data matching2;
	merge matching2 (in=a) possiblenclass (in=b);
	by nclass;
	if a and b;
	if pdpass~=c_pdpass~=o_pdpass;
	if invnum~=c_invnum~=o_invnum;
	if patent~=control~=cited;
	if cited~=cite1--cite137;
	if c_appyear>=o_gyear;
	if abs(sasdate-c_sasdate)<=182;
run;

proc sort nodupkey data=matching2;
	by cited patent;
run;

proc datasets library=work;
	append base=matching data=matching2 force;
run;

proc sort;
	by cited patent;
run;

proc datasets library=work;
   delete matching1 matching2;
run;

/*Begin WITH REPLACEMENT Matching: #4*/

data possiblenclass;
	set possiblenclass (drop=x_control);
	x_control=RAND('UNIFORM');
run;

proc sort data=possiblenclass;
	by nclass x_control;
run;
	
data matching1 (keep= cited patent binvar);
	set matching;
	binvar=1;
run;

proc sort data=citations;
	by cited patent;
run;

data matching2;
	merge citations (in=a) matching1;
	by cited patent;
	if a;
run;

data matching2;
	set matching2;
	if binvar=1 then delete;
run;

proc sort data=matching2;
	by nclass;
run;

data matching2;
	merge matching2 (in=a) possiblenclass (in=b);
	by nclass;
	if a and b;
	if pdpass~=c_pdpass~=o_pdpass;
	if invnum~=c_invnum~=o_invnum;
	if patent~=control~=cited;
	if cited~=cite1--cite137;
	if c_appyear>=o_gyear;
	if abs(sasdate-c_sasdate)<=182;
run;

proc sort nodupkey data=matching2;
	by cited patent;
run;

proc datasets library=work;
	append base=matching data=matching2 force;
run;

proc sort;
	by cited patent;
run;

proc datasets library=work;
   delete matching1 matching2;
run;

/*Begin WITH REPLACEMENT Matching: #5*/

data possiblenclass;
	set possiblenclass (drop=x_control);
	x_control=RAND('UNIFORM');
run;

proc sort data=possiblenclass;
	by nclass x_control;
run;
	
data matching1 (keep= cited patent binvar);
	set matching;
	binvar=1;
run;

proc sort data=citations;
	by cited patent;
run;

data matching2;
	merge citations (in=a) matching1;
	by cited patent;
	if a;
run;

data matching2;
	set matching2;
	if binvar=1 then delete;
run;

proc sort data=matching2;
	by nclass;
run;

data matching2;
	merge matching2 (in=a) possiblenclass (in=b);
	by nclass;
	if a and b;
	if pdpass~=c_pdpass~=o_pdpass;
	if invnum~=c_invnum~=o_invnum;
	if patent~=control~=cited;
	if cited~=cite1--cite137;
	if c_appyear>=o_gyear;
	if abs(sasdate-c_sasdate)<=182;
run;

proc sort nodupkey data=matching2;
	by cited patent;
run;

proc datasets library=work;
	append base=matching data=matching2 force;
run;

proc sort;
	by cited patent;
run;

proc datasets library=work;
   delete matching1 matching2;
run;

/*Begin WITH REPLACEMENT Matching: #6*/

data possiblenclass;
	set possiblenclass (drop=x_control);
	x_control=RAND('UNIFORM');
run;

proc sort data=possiblenclass;
	by nclass x_control;
run;
	
data matching1 (keep= cited patent binvar);
	set matching;
	binvar=1;
run;

proc sort data=citations;
	by cited patent;
run;

data matching2;
	merge citations (in=a) matching1;
	by cited patent;
	if a;
run;

data matching2;
	set matching2;
	if binvar=1 then delete;
run;

proc sort data=matching2;
	by nclass;
run;

data matching2;
	merge matching2 (in=a) possiblenclass (in=b);
	by nclass;
	if a and b;
	if pdpass~=c_pdpass~=o_pdpass;
	if invnum~=c_invnum~=o_invnum;
	if patent~=control~=cited;
	if cited~=cite1--cite137;
	if c_appyear>=o_gyear;
	if abs(sasdate-c_sasdate)<=182;
run;

proc sort nodupkey data=matching2;
	by cited patent;
run;

proc datasets library=work;
	append base=matching data=matching2 force;
run;

proc sort;
	by cited patent;
run;

proc datasets library=work;
   delete matching1 matching2;
run;

/*Begin WITH REPLACEMENT Matching: #7*/

data possiblenclass;
	set possiblenclass (drop=x_control);
	x_control=RAND('UNIFORM');
run;

proc sort data=possiblenclass;
	by nclass x_control;
run;
	
data matching1 (keep= cited patent binvar);
	set matching;
	binvar=1;
run;

proc sort data=citations;
	by cited patent;
run;

data matching2;
	merge citations (in=a) matching1;
	by cited patent;
	if a;
run;

data matching2;
	set matching2;
	if binvar=1 then delete;
run;

proc sort data=matching2;
	by nclass;
run;

data matching2;
	merge matching2 (in=a) possiblenclass (in=b);
	by nclass;
	if a and b;
	if pdpass~=c_pdpass~=o_pdpass;
	if invnum~=c_invnum~=o_invnum;
	if patent~=control~=cited;
	if cited~=cite1--cite137;
	if c_appyear>=o_gyear;
	if abs(sasdate-c_sasdate)<=182;
run;

proc sort nodupkey data=matching2;
	by cited patent;
run;

proc datasets library=work;
	append base=matching data=matching2 force;
run;

proc sort;
	by cited patent;
run;

proc datasets library=work;
   delete matching1 matching2;
run;

/*Begin WITH REPLACEMENT Matching: #8*/

data possiblenclass;
	set possiblenclass (drop=x_control);
	x_control=RAND('UNIFORM');
run;

proc sort data=possiblenclass;
	by nclass x_control;
run;
	
data matching1 (keep= cited patent binvar);
	set matching;
	binvar=1;
run;

proc sort data=citations;
	by cited patent;
run;

data matching2;
	merge citations (in=a) matching1;
	by cited patent;
	if a;
run;

data matching2;
	set matching2;
	if binvar=1 then delete;
run;

proc sort data=matching2;
	by nclass;
run;

data matching2;
	merge matching2 (in=a) possiblenclass (in=b);
	by nclass;
	if a and b;
	if pdpass~=c_pdpass~=o_pdpass;
	if invnum~=c_invnum~=o_invnum;
	if patent~=control~=cited;
	if cited~=cite1--cite137;
	if c_appyear>=o_gyear;
	if abs(sasdate-c_sasdate)<=182;
run;

proc sort nodupkey data=matching2;
	by cited patent;
run;

proc datasets library=work;
	append base=matching data=matching2 force;
run;

proc sort;
	by cited patent;
run;

proc datasets library=work;
   delete matching1 matching2;
run;

/*Begin WITH REPLACEMENT Matching: #9*/

data possiblenclass;
	set possiblenclass (drop=x_control);
	x_control=RAND('UNIFORM');
run;

proc sort data=possiblenclass;
	by nclass x_control;
run;
	
data matching1 (keep= cited patent binvar);
	set matching;
	binvar=1;
run;

proc sort data=citations;
	by cited patent;
run;

data matching2;
	merge citations (in=a) matching1;
	by cited patent;
	if a;
run;

data matching2;
	set matching2;
	if binvar=1 then delete;
run;

proc sort data=matching2;
	by nclass;
run;

data matching2;
	merge matching2 (in=a) possiblenclass (in=b);
	by nclass;
	if a and b;
	if pdpass~=c_pdpass~=o_pdpass;
	if invnum~=c_invnum~=o_invnum;
	if patent~=control~=cited;
	if cited~=cite1--cite137;	
	if c_appyear>=o_gyear;
	if abs(sasdate-c_sasdate)<=182;
run;

proc sort nodupkey data=matching2;
	by cited patent;
run;

proc datasets library=work;
	append base=matching data=matching2 force;
run;

proc sort;
	by cited patent;
run;

proc datasets library=work;
   delete matching1 matching2;
run;

/*Begin WITH REPLACEMENT Matching: #10*/

data possiblenclass;
	set possiblenclass (drop=x_control);
	x_control=RAND('UNIFORM');
run;

proc sort data=possiblenclass;
	by nclass x_control;
run;
	
data matching1 (keep= cited patent binvar);
	set matching;
	binvar=1;
run;

proc sort data=citations;
	by cited patent;
run;

data matching2;
	merge citations (in=a) matching1;
	by cited patent;
	if a;
run;

data matching2;
	set matching2;
	if binvar=1 then delete;
run;

proc sort data=matching2;
	by nclass;
run;

data matching2;
	merge matching2 (in=a) possiblenclass (in=b);
	by nclass;
	if a and b;
	if pdpass~=c_pdpass~=o_pdpass;
	if invnum~=c_invnum~=o_invnum;
	if patent~=control~=cited;
	if cited~=cite1--cite137;
	if c_appyear>=o_gyear;
	if abs(sasdate-c_sasdate)<=182;
run;

proc sort nodupkey data=matching2;
	by cited patent;
run;

proc datasets library=work;
	append base=matching data=matching2 force;
run;

proc sort;
	by cited patent;
run;

proc datasets library=work;
   delete matching1 matching2;
run;

/*Begin WITH REPLACEMENT Matching: #11*/

data possiblenclass;
	set possiblenclass (drop=x_control);
	x_control=RAND('UNIFORM');
run;

proc sort data=possiblenclass;
	by nclass x_control;
run;
	
data matching1 (keep= cited patent binvar);
	set matching;
	binvar=1;
run;

proc sort data=citations;
	by cited patent;
run;

data matching2;
	merge citations (in=a) matching1;
	by cited patent;
	if a;
run;

data matching2;
	set matching2;
	if binvar=1 then delete;
run;

proc sort data=matching2;
	by nclass;
run;

data matching2;
	merge matching2 (in=a) possiblenclass (in=b);
	by nclass;
	if a and b;
	if pdpass~=c_pdpass~=o_pdpass;
	if invnum~=c_invnum~=o_invnum;
	if patent~=control~=cited;
	if cited~=cite1--cite137;
	if c_appyear>=o_gyear;
	if abs(sasdate-c_sasdate)<=182;
run;

proc sort nodupkey data=matching2;
	by cited patent;
run;

proc datasets library=work;
	append base=matching data=matching2 force;
run;

proc sort;
	by cited patent;
run;

proc datasets library=work;
   delete matching1 matching2;
run;

/*Begin WITH REPLACEMENT Matching: #12*/

data possiblenclass;
	set possiblenclass (drop=x_control);
	x_control=RAND('UNIFORM');
run;

proc sort data=possiblenclass;
	by nclass x_control;
run;
	
data matching1 (keep= cited patent binvar);
	set matching;
	binvar=1;
run;

proc sort data=citations;
	by cited patent;
run;

data matching2;
	merge citations (in=a) matching1;
	by cited patent;
	if a;
run;

data matching2;
	set matching2;
	if binvar=1 then delete;
run;

proc sort data=matching2;
	by nclass;
run;

data matching2;
	merge matching2 (in=a) possiblenclass (in=b);
	by nclass;
	if a and b;
	if pdpass~=c_pdpass~=o_pdpass;
	if invnum~=c_invnum~=o_invnum;
	if patent~=control~=cited;
	if cited~=cite1--cite137;
	if c_appyear>=o_gyear;
	if abs(sasdate-c_sasdate)<=182;
run;

proc sort nodupkey data=matching2;
	by cited patent;
run;

proc datasets library=work;
	append base=matching data=matching2 force;
run;

proc sort;
	by cited patent;
run;

proc datasets library=work;
   delete matching1 matching2;
run;

/*Begin WITH REPLACEMENT Matching: #13*/

data possiblenclass;
	set possiblenclass (drop=x_control);
	x_control=RAND('UNIFORM');
run;

proc sort data=possiblenclass;
	by nclass x_control;
run;
	
data matching1 (keep= cited patent binvar);
	set matching;
	binvar=1;
run;

proc sort data=citations;
	by cited patent;
run;

data matching2;
	merge citations (in=a) matching1;
	by cited patent;
	if a;
run;

data matching2;
	set matching2;
	if binvar=1 then delete;
run;

proc sort data=matching2;
	by nclass;
run;

data matching2;
	merge matching2 (in=a) possiblenclass (in=b);
	by nclass;
	if a and b;
	if pdpass~=c_pdpass~=o_pdpass;
	if invnum~=c_invnum~=o_invnum;
	if patent~=control~=cited;
	if cited~=cite1--cite137;
	if c_appyear>=o_gyear;
	if abs(sasdate-c_sasdate)<=182;
run;

proc sort nodupkey data=matching2;
	by cited patent;
run;

proc datasets library=work;
	append base=matching data=matching2 force;
run;

proc sort;
	by cited patent;
run;

proc datasets library=work;
   delete matching1 matching2;
run;

/*Begin WITH REPLACEMENT Matching: #14*/

data possiblenclass;
	set possiblenclass (drop=x_control);
	x_control=RAND('UNIFORM');
run;

proc sort data=possiblenclass;
	by nclass x_control;
run;
	
data matching1 (keep= cited patent binvar);
	set matching;
	binvar=1;
run;

proc sort data=citations;
	by cited patent;
run;

data matching2;
	merge citations (in=a) matching1;
	by cited patent;
	if a;
run;

data matching2;
	set matching2;
	if binvar=1 then delete;
run;

proc sort data=matching2;
	by nclass;
run;

data matching2;
	merge matching2 (in=a) possiblenclass (in=b);
	by nclass;
	if a and b;
	if pdpass~=c_pdpass~=o_pdpass;
	if invnum~=c_invnum~=o_invnum;
	if patent~=control~=cited;
	if cited~=cite1--cite137;
	if c_appyear>=o_gyear;
	if abs(sasdate-c_sasdate)<=182;
run;

proc sort nodupkey data=matching2;
	by cited patent;
run;

proc datasets library=work;
	append base=matching data=matching2 force;
run;

proc sort;
	by cited patent;
run;

proc datasets library=work;
   delete matching1 matching2;
run;

/*Begin WITH REPLACEMENT Matching: #15*/

data possiblenclass;
	set possiblenclass (drop=x_control);
	x_control=RAND('UNIFORM');
run;

proc sort data=possiblenclass;
	by nclass x_control;
run;
	
data matching1 (keep= cited patent binvar);
	set matching;
	binvar=1;
run;

proc sort data=citations;
	by cited patent;
run;

data matching2;
	merge citations (in=a) matching1;
	by cited patent;
	if a;
run;

data matching2;
	set matching2;
	if binvar=1 then delete;
run;

proc sort data=matching2;
	by nclass;
run;

data matching2;
	merge matching2 (in=a) possiblenclass (in=b);
	by nclass;
	if a and b;
	if pdpass~=c_pdpass~=o_pdpass;
	if invnum~=c_invnum~=o_invnum;
	if patent~=control~=cited;
	if cited~=cite1--cite137;
	if c_appyear>=o_gyear;
	if abs(sasdate-c_sasdate)<=182;
run;

proc sort nodupkey data=matching2;
	by cited patent;
run;

proc datasets library=work;
	append base=matching data=matching2 force;
run;

proc sort;
	by cited patent;
run;

proc datasets library=work;
   delete matching1 matching2;
run;

/*Begin WITH REPLACEMENT Matching: #16*/

data possiblenclass;
	set possiblenclass (drop=x_control);
	x_control=RAND('UNIFORM');
run;

proc sort data=possiblenclass;
	by nclass x_control;
run;
	
data matching1 (keep= cited patent binvar);
	set matching;
	binvar=1;
run;

proc sort data=citations;
	by cited patent;
run;

data matching2;
	merge citations (in=a) matching1;
	by cited patent;
	if a;
run;

data matching2;
	set matching2;
	if binvar=1 then delete;
run;

proc sort data=matching2;
	by nclass;
run;

data matching2;
	merge matching2 (in=a) possiblenclass (in=b);
	by nclass;
	if a and b;
	if pdpass~=c_pdpass~=o_pdpass;
	if invnum~=c_invnum~=o_invnum;
	if patent~=control~=cited;
	if cited~=cite1--cite137;
	if c_appyear>=o_gyear;
	if abs(sasdate-c_sasdate)<=182;
run;

proc sort nodupkey data=matching2;
	by cited patent;
run;

proc datasets library=work;
	append base=matching data=matching2 force;
run;

proc sort;
	by cited patent;
run;

proc datasets library=work;
   delete matching1 matching2;
run;

/*Begin WITH REPLACEMENT Matching: #17*/

data possiblenclass;
	set possiblenclass (drop=x_control);
	x_control=RAND('UNIFORM');
run;

proc sort data=possiblenclass;
	by nclass x_control;
run;
	
data matching1 (keep= cited patent binvar);
	set matching;
	binvar=1;
run;

proc sort data=citations;
	by cited patent;
run;

data matching2;
	merge citations (in=a) matching1;
	by cited patent;
	if a;
run;

data matching2;
	set matching2;
	if binvar=1 then delete;
run;

proc sort data=matching2;
	by nclass;
run;

data matching2;
	merge matching2 (in=a) possiblenclass (in=b);
	by nclass;
	if a and b;
	if pdpass~=c_pdpass~=o_pdpass;
	if invnum~=c_invnum~=o_invnum;
	if patent~=control~=cited;
	if cited~=cite1--cite137;
	if c_appyear>=o_gyear;
	if abs(sasdate-c_sasdate)<=182;
run;

proc sort nodupkey data=matching2;
	by cited patent;
run;

proc datasets library=work;
	append base=matching data=matching2 force;
run;

proc sort;
	by cited patent;
run;

proc datasets library=work;
   delete matching1 matching2;
run;

/*Begin WITH REPLACEMENT Matching: #18*/

data possiblenclass;
	set possiblenclass (drop=x_control);
	x_control=RAND('UNIFORM');
run;

proc sort data=possiblenclass;
	by nclass x_control;
run;
	
data matching1 (keep= cited patent binvar);
	set matching;
	binvar=1;
run;

proc sort data=citations;
	by cited patent;
run;

data matching2;
	merge citations (in=a) matching1;
	by cited patent;
	if a;
run;

data matching2;
	set matching2;
	if binvar=1 then delete;
run;

proc sort data=matching2;
	by nclass;
run;

data matching2;
	merge matching2 (in=a) possiblenclass (in=b);
	by nclass;
	if a and b;
	if pdpass~=c_pdpass~=o_pdpass;
	if invnum~=c_invnum~=o_invnum;
	if patent~=control~=cited;
	if cited~=cite1--cite137;
	if c_appyear>=o_gyear;
	if abs(sasdate-c_sasdate)<=182;
run;

proc sort nodupkey data=matching2;
	by cited patent;
run;

proc datasets library=work;
	append base=matching data=matching2 force;
run;

proc sort;
	by cited patent;
run;

proc datasets library=work;
   delete matching1 matching2;
run;

/*Begin WITH REPLACEMENT Matching: #19*/

data possiblenclass;
	set possiblenclass (drop=x_control);
	x_control=RAND('UNIFORM');
run;

proc sort data=possiblenclass;
	by nclass x_control;
run;
	
data matching1 (keep= cited patent binvar);
	set matching;
	binvar=1;
run;

proc sort data=citations;
	by cited patent;
run;

data matching2;
	merge citations (in=a) matching1;
	by cited patent;
	if a;
run;

data matching2;
	set matching2;
	if binvar=1 then delete;
run;

proc sort data=matching2;
	by nclass;
run;

data matching2;
	merge matching2 (in=a) possiblenclass (in=b);
	by nclass;
	if a and b;
	if pdpass~=c_pdpass~=o_pdpass;
	if invnum~=c_invnum~=o_invnum;
	if patent~=control~=cited;
	if cited~=cite1--cite137;
	if c_appyear>=o_gyear;
	if abs(sasdate-c_sasdate)<=182;
run;

proc sort nodupkey data=matching2;
	by cited patent;
run;

proc datasets library=work;
	append base=matching data=matching2 force;
run;

proc sort;
	by cited patent;
run;

proc datasets library=work;
   delete matching1 matching2;
run;

/*Begin WITH REPLACEMENT Matching: #20*/

data possiblenclass;
	set possiblenclass (drop=x_control);
	x_control=RAND('UNIFORM');
run;

proc sort data=possiblenclass;
	by nclass x_control;
run;
	
data matching1 (keep= cited patent binvar);
	set matching;
	binvar=1;
run;

proc sort data=citations;
	by cited patent;
run;

data matching2;
	merge citations (in=a) matching1;
	by cited patent;
	if a;
run;

data matching2;
	set matching2;
	if binvar=1 then delete;
run;

proc sort data=matching2;
	by nclass;
run;

data matching2;
	merge matching2 (in=a) possiblenclass (in=b);
	by nclass;
	if a and b;
	if pdpass~=c_pdpass~=o_pdpass;
	if invnum~=c_invnum~=o_invnum;
	if patent~=control~=cited;
	if cited~=cite1--cite137;
	if c_appyear>=o_gyear;
	if abs(sasdate-c_sasdate)<=182;
run;

proc sort nodupkey data=matching2;
	by cited patent;
run;

proc datasets library=work;
	append base=matching data=matching2 force;
run;

proc sort;
	by cited patent;
run;

proc datasets library=work;
   delete matching1 matching2;
run;

/*Add Matching to X Library*/
data x.matching (drop=x_citat x_control cite1--cite137);
	set matching;
run;
