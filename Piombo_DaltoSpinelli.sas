/*
D'alto Jacopo S4952059
Spinelli Sonia S5176304

PIOMBEMIA
piombo.txt

***********************************************************************************************
Organizzo il dataset;
*/
LIBNAME L "/home/u62460393/PIOMBO/";

PROC IMPORT OUT=piombo
	DATAFILE="/home/u62460393/PIOMBO/piombo.xlsx" DBMS=XLSX REPLACE;
	GETNAMES=YES;
	DATAROW=2;
RUN;

**apro il file e inserisco le variabili richieste
setto le label
rinomino quetel;
	
DATA L.piombo;
	SET piombo (KEEP=GENDER QUETEL ETAPRE ABIALC DURESP EMATOC MTINTE MTMODE MTSCAR	ESPBTR PIAABI PBEMIA);
	RENAME QUETEL=BMI;
	ATTRIB QUETEL LABEL="Body Mass Index"
    ETAPRE LABEL= "Età del primo prelievo (anni) "
    ABIALC LABEL ="Abitudine all'alcol (grammi/giorno) "
    DURESP LABEL = "Durata esposizione al piombo"
    EMATOC LABEL = "Ematocrito (mg/dL)"
    MTINTE LABEL = "Minuti al giorno passati nel traffico intenso"
    MTMODE LABEL = "Minuti al giorno passati nel traffico moderato"
    MTSCAR LABEL = "Minuti al giorno passati nel traffico scarso"
    ESPBTR LABEL = "Minuti al giorno di esposizione al traffico"
    PIAABI LABEL = "Piano abitato"
  	PBEMIA LABEL = "Concentrazione di piombo nel sangue (µg/dL)"
    GENDER LABEL = "Sesso";
    *preferiamo avere bmi piuttosto che quetel;
PROC PRINT; RUN;


**stampo le prime 5 osservazioni e i dettagli delle variabili;
DATA stampa;
	SET L.piombo (OBS=5);
PROC PRINT;
	TITLE "Le prime 5 osservazioni";
	RUN;
	TITLE;

PROC CONTENTS DATA=L.piombo;
	TITLE "LE VARIABILI";
	RUN;
	TITLE;

**creo delle variabili ausiliarie;
DATA piomb;
	SET L.piombo;
	**divido per età;
	if .<ETAPRE<18 then FasciaEta=1; **"minorenni";
	ELSE IF 18<=ETAPRE<50 THEN FasciaEta=2; **"under50";
	ELSE IF ETAPRE>=50 THEN FasciaEta=3; **"over50";
	
	**divido per fasce di peso;	
	**NB: i valori di riferimento sono diversi a seconda dell'età, quindi le fasce vengono diverse;
	IF FASCIAETA = 1 THEN FasciaPeso=0; **Minorenni;
	ELSE IF (18<=ETAPRE<25 AND .<BMI<19) OR (25<=ETAPRE<35 AND .<BMI<20) OR (35<=ETAPRE<45 AND .<BMI<21) OR (45<=ETAPRE<55 AND .<BMI<22) OR (55<=ETAPRE<65 AND .<BMI<23) OR (ETAPRE>=65 AND .<BMI<24) THEN FasciaPeso=1; **"sottopeso";
	ELSE IF (18<=ETAPRE<25 AND 19<=BMI<24) OR (25<=ETAPRE<35 AND 20<=BMI<25) OR (35<=ETAPRE<45 AND 21<=BMI<26) OR (45<=ETAPRE<55 AND 22<=BMI<27) OR (55<=ETAPRE<65 AND 23<=BMI<28) OR (ETAPRE>=65 AND 24<=BMI<29) THEN FasciaPeso=2; **"normopeso";
	ELSE IF (18<=ETAPRE<25 AND 24<=BMI<29) OR (25<=ETAPRE<35 AND 25<=BMI<30) OR (35<=ETAPRE<45 AND 26<=BMI<31) OR (45<=ETAPRE<55 AND 27<=BMI<32) OR (55<=ETAPRE<65 AND 28<=BMI<33) OR (ETAPRE>=65 AND 29<=BMI<34) THEN FasciaPeso=3; **"sovrappeso";
	ELSE IF (18<=ETAPRE<25 AND 29<=BMI) OR (25<=ETAPRE<35 AND 30<=BMI) OR (35<=ETAPRE<45 AND 31<=BMI) OR (45<=ETAPRE<55 AND 32<=BMI) OR (55<=ETAPRE<65 AND 33<=BMI) OR (ETAPRE>=65 AND 34<=BMI) THEN FasciaPeso=4; **"obesità";
	
	*divido per fasce di esposizione al traffico;
	IF ESPBTR=0 THEN FasciaTr=0;**"nulla";
	IF 0<ESPBTR<=60 THEN FasciaTr=1;**"bassa";
	ELSE IF 60<ESPBTR<=120 THEN FasciaTr=2;**"media";
	ELSE IF ESPBTR>120 THEN FasciaTr=3;**"alta";
	
	**Divido per fasce di ematocrito.
	**NB: i valori di riferimento sono diversi a seconda del genere, quindi le fasce vengono diverse;
	*DONNE;
	IF GENDER = 1 AND .<EMATOC<36 THEN FasciaEma=1; **"basso";
	ELSE IF  GENDER = 1 AND 36<=EMATOC<46 THEN FasciaEma=2;**"medio";
	ELSE IF GENDER = 1 AND EMATOC>=46 THEN FasciaEma=3;**"alto";
	*UOMINI;
	IF GENDER = 2 AND .<EMATOC<40 THEN FasciaEma=1;**"basso";
	ELSE IF GENDER = 2 AND 40<=EMATOC<50 THEN FasciaEma=2; **"medio";
	ELSE IF GENDER = 2 AND EMATOC>=50 THEN FasciaEma=3;**"alto";
	
	*divido per bicchieri al giorno di vino;
	IF ABIALC=0 THEN BicchieriAlG=0; **astemi;
	ELSE IF 0<ABIALC<=30 THEN BicchieriAlG=2; **bevono due bicchieri di vino al giorno;
	ELSE IF 30<ABIALC<=60 THEN BicchieriAlG=4; **bevono 4 bicchieri di vino al giorno;
	ELSE IF 60<ABIALC<=90 THEN BicchieriAlG=6; **bevono 6 bicchieri di vino al giorno;
	ELSE IF ABIALC>90 THEN BicchieriAlG=7; **bevono più di 7 bicchieri di vino al giorno;
	
RUN;

*Divido per livelli di piombemia usando i quartili perchè non ci sono ripartizioni standard;
proc univariate data=l.piombo;
output out=pct P25=P25 P50=P50 p75=P75;
var pbemia;
run;quit;

data piomb;
set piomb;
if _n_=1 then set pct;
if .<pbemia<=p25 then FasciaPb=1;
else if p25<pbemia<=p50 then FasciaPb=2;
else if p50<pbemia<=p75 then FasciaPb=3;
else if pbemia>p75 then FasciaPb=4;
drop p25 p50 p75;
proc print;run;

**Imposto il formato dei valori ;
PROC FORMAT;
**formato delle fasce di età;
	VALUE FasciaEta
		1=Minorenni
		2=Under50
		3=Over50;
**formato delle fasce di peso;
	VALUE FasciaPeso
		0=Minorenni
		1=Sottopeso
		2=Normopeso
		3=Sovrappeso
		4=Obesità;
**formato delle fasce di esposizione al traffico;
	VALUE FasciaTr
		0=Nulla
		1=Fino a un ora
		2=Fino a due ore
		3=Più di due ore;
*formato del genere;
	VALUE gender
		1=Femmine
		2=Maschi;
*formato dell'ematocrito;
	VALUE FasciaEma
		1=Basso
		2=Medio
		3=Alto;
*formato piombemia;
	VALUE FasciaPb
		1=Bassa
		2=Media
		3=Alta
		4=Molto alta;
	
DATA stampa2;
	SET piomb(OBS=5);
PROC PRINT; 
	TITLE "Le fascie qualitiative delle prime 5 osservazioni";
	RUN;
	Title;
	
	
*significato delle nostre variabili qualitative: minimo massimo media e osservazioni;
%LET FASC=FasciaEta FasciaPb  FasciaPeso  FasciaTr  FasciaEma BicchieriAlG;
%LET VARIABILI=ETAPRE PBEMIA  BMI  ESPBTR EMATOC ABIALC;
%LET NUM=6;
%MACRO DescrVar;
	%DO i=1 %TO &NUM;
		%LET fascia=%SCAN(&FASC, &i);
		%LET VAR=%SCAN(&VARIABILI, &i);
		PROC SQL;
			title "Minimo, massimo e media di &var. nei singoli livelli di &fascia.";
			SELECT DISTINCT &fascia FORMAT=&fascia..,
					MIN(&VAR) AS Min&VAR.,
					MAX(&VAR) AS Max&VAR.,
					MEAN(&VAR) AS Media&VAR.,
					COUNT(*) AS OBS
			FROM piomb
			GROUP BY  &fascia;
			QUIT;
			title;
		proc sort data = piomb;
			by &fascia;
			run;
		proc boxplot data = piomb;
			title "Boxplot di &VAR. per &FASCIA.";
			format &FASCIA &FASCIA..;
			plot &VAR*&FASCIA/ boxconnect=mean;
   				insetgroup mean ;
			run;
			title ;
	%END;
%MEND DescrVar;
%DescrVar;


**DIMOSTRIAMO COME E' STATA COSTRUITA ESPBTR;
proc reg DATA=l.piombo;
    model ESPBTR= MTINTE MTMODE MTSCAR / cli clm p r;
    output out=b p=stima r=residui;
run; quit;

goption ftext=swissb;
proc gplot data=b;
     plot (ESPBTR  stima )* PBEMIA /overlay;
run; quit; 


PROC PRINT DATA=piomb; RUN;

**definizione del BMI;	
DATA BMIAnni;
INPUT Anni $ Sottopeso $ Normopeso $ Sovrappeso $ Obesità $;
datalines;
18-25 <19 19-24 24-29 >29
25-35 <20 20-25 25-30 >30
35-45 <21 21-26 26-31 >31
55-65 <22 22-27 27-32 >32
>60 <23 23-28 28-33 >33
;
proc print noobs;
TITLE "Definizione del BMI secondo l'età";
run;
TITLE;

*************************************************************************************************************
*************************************************************************************************************;
**Calcolo delle medie senza outlier;

**divido in dataset secondo l'età;
DATA Minorenni Under50 Over50 ;
	SET piomb;
	IF FasciaEta=1 THEN OUTPUT Minorenni;
	IF FasciaEta=2 THEN OUTPUT Under50;
	IF FasciaEta=3 THEN OUTPUT Over50;

*Macro per escludere gli outlier;
**Per ogni variabile creo un dataset in cui gli outlier vengono esclusi, su cui calcola la media;

%let variabili = BMI ETAPRE ABIALC EMATOC ESPBTR PBEMIA;
%let FasciaEta= Minorenni Under50 Over50;

%MACRO Outlier;
	%do i=1 %to &num;
	%let var = %scan(&variabili,&i);
		%do j=1 %to 3;
		%let eta= %scan(&FasciaEta, &j);
		
		proc means data = &eta.(keep = &var) p25 p75 QRANGE;
		title "Percentili della variabile &var. in &eta.";
		output out = index&i(drop = _FREQ_ _TYPE_) p25 = Q1 QRANGE = IQR p75 = Q3;
		run;
		title;
		
		data aux;
		if _n_ = 1 then set index&i;
		set &eta.(keep=&var);
		run;

		data &eta.senzaOutlier&var;
			set aux;
			if Q1-1.5*IQR<=&var<=IQR*1.5+Q3;
			drop Q1 IQR Q3;
		run;
		
		proc means data = &eta.senzaOutlier&var;
		title "Statistiche senza outlier della variabile &var. in &eta. ";
		output out = &eta.mean&var(drop = _FREQ_ _TYPE_) mean = mean&var;
		run;
		title;
		
		data &eta.Outlier&var;
			set aux;
			if Q1-1.5*IQR>&var or &var>Q3+1.5*IQR;
			drop Q1 IQR Q3;
		run;
		%end;
	%end;

%mend outlier;
%outlier;

**dataset col profilo medio di ciascun età;
%MACRO ProfMedio;
	%do j=1 %to 3;
		%let eta= %scan(&FasciaEta, &j);
		Data ProfiloMedio&eta.;
			FasciaEta=&j;
			%do i=1 %to &num;
			%let var = %scan(&variabili,&i); 
				SET &eta.mean&var.;
			%end;
			run;	
	%end;
%MEND ProfMedio;
%ProfMedio;

**unione dei profili medi;
Data ProfiloMedio;
set ProfiloMedioMinorenni ProfiloMedioUnder50 ProfiloMedioOver50;
PROC PRINT noobs;
	format FasciaEta FasciaEta.;
	TITLE "Profilo Medio senza OUTLIERS per Fasce di eta'";
	run;

*plot delle  variabili più interessanti rispetto all'età;
**EMATOC;
goption reset=all vsize=10cm hsize=20cm ;
proc gplot data = profilomedio;
format FasciaEta FasciaEta.;
TITLE "Media di Ematocrito rispetto all'eta'" ;
label meanEMATOC=" ";
plot meanEMATOC*FasciaEta / legend   grid; 
symbol c=bib line=1 value=dot interpol=join;
run; 
TITLE;

**pbemia;
goption reset=all vsize=10cm hsize=20cm;
proc gplot data = profilomedio;
format FasciaEta FasciaEta.;
TITLE "Media di Piombemia rispetto all'eta'" ;
plot meanPBEMIA*FasciaEta/ legend grid ;
symbol c=red line = 1 value=dot interpol=join;
run;quit;
TITLE;

**espbtr;
goption reset=all vsize=10cm hsize=20cm;
proc gplot data = profilomedio;
format FasciaEta FasciaEta.;
TITLE "Media di EPBTR rispetto all'eta'" ;
plot meanESPBTR*FasciaEta/ legend grid ;
symbol c=bib line = 1 value=dot interpol=join;
run;quit;
TITLE;

**abialc;
goption reset=all vsize=10cm hsize=20cm;
proc gplot data = profilomedio;
format FasciaEta FasciaEta.;
TITLE "Media di ABIALC rispetto all'eta'" ;
plot meanABIALC*FasciaEta/ legend grid ;
symbol c=red line = 1 value=dot interpol=join;
run;quit;
TITLE;

**profilo medio con outliers;
proc sort data = piomb;
by fasciaeta;
run;

proc means data = piomb(keep= fasciaeta BMI ETAPRE ABIALC EMATOC ESPBTR PBEMIA);
by fasciaeta;
format fasciaeta fasciaeta.;
output out=profmediotutto(drop = _TYPE_ _FREQ_) mean(bmi)=meanBMI mean(ETAPRE)=meanETAPRE mean(ABIALC)=meanABIALC mean(EMATOC)=meanEMATOC mean(ESPBTR)=meanESPBTR mean(PBEMIA)=meanPBEMIA;
proc print;run;

proc print noobs data = profmediotutto;
TITLE "Profilo Medio con OUTLIERS per Fasce di eta'";
run;
TITLE;

*************************************************************************************************************
*************************************************************************************************************
**DESCRIZIONE UNIVARIATA DELLA POPOLAZIONE DIVISA PER ETA';

**Popolazione divisa per fasce d'età;
GOPTIONS RESET=(PATTERN);
PATTERN1 C=KHAKI; PATTERN2 C=big; PATTERN3 C=indianred;
PROC GCHART DATA=piomb;
	FORMAT FasciaEta FasciaEta.;
	PIE3D FasciaEta/DISCRETE  TYPE=percent VALUE=ARROW EXPLODE=1;
	TITLE "Popolazione divisa per età";
	RUN;QUIT; 
TITLE;

**Numero di maschi e femmine divisi per età;
proc freq data=piomb;
tables FasciaEta*gender;
run;

	
**Maschi e femmine divisi per età;

PROC SGPANEL DATA=piomb NOAUTOLEGEND PCTLEVEL=CELL;
	PANELBY FasciaEta / NOVARNAME COLUMNS=3 layout=columnlattice ;
	FORMAT gender gender. FasciaEta FasciaEta.;
	VBAR gender/ barwidth=0.5  group=gender STAT=PERCENT;
	LABEL gender=' ';
	STYLEATTRS DATACOLORS= (LIGHTCORAL CORNFLOWERBLUE)  DATACONTRASTCOLORS=(LIGHTCORAL CORNFLOWERBLUE);
	ROWAXIS grid;
	TITLE "Sesso diviso per fascia di età";
	RUN;QUIT; 
TITLE;
	
**Studio ciascuna variabile condizionatamente alla fascia d'età;
**Creo una macro che date due variabili qualitative plotta la distribuzione della seconda rispetto alla prima;
%MACRO Grafico(a,b);
	PROC GCHART DATA=piomb;
		FORMAT &a &a.. &b &b..;
		TITLE "Distribuzione di &B. condizionata rispetto a &A. ";
		VBAR &a/DISCRETE SUBGROUP=&b DISCRETE GROUP=&a
		WIDTH=8 INSIDE=percent G100 TYPE=percent NOZERO;
	RUN;QUIT;
	TITLE;
%MEND;

*Creo una macro che, specificatamente a fasciaeta, ne studia la relazione con le altre variabili carattere;
%LET VAR1= FasciaPeso FasciaEma BicchieriAlG FasciaTr FasciaPb PIAABI DURESP;
%LET NUM1=7;
%MACRO GraficiEta;
	%DO i=1 %TO &NUM1;
		%LET v=%SCAN(&VAR1, &i);
		%Grafico(FasciaEta,&v);
	%END;
%MEND GraficiEta;

goptions reset=all device=png  gsfname=mypng vsize=10cm hsize=25cm;
goption reset=(pattern);
pattern1 c=darkseagreen; 
pattern2 c=navajowhite;
pattern3 c=stypk;
pattern4 c=firebrick;
pattern5 c=moybr;
%GraficiEta;


**Macro che studia la distribuzione di una variabile quantitativa rispetto a fasciaeta;
%MACRO BoxplotFasciaEta;
	%do i=1 %to &num.;
	%let var = %scan(&variabili,&i);
	
	goption vsize=10cm hsize=20cm; 
	proc boxplot data = piomb;
	format fasciaeta fasciaeta.;
	plot &var.*fasciaeta/ grid ;
	insetgroup min q2 max;
	%end;
%MEND BoxplotFasciaEta;

proc sort data=piomb;
by FasciaEta;
run;
%boxplotfasciaeta;

**LEGAME PIOMBEMIA E VARIABILI;
*Creo una macro che, specificatamente a fasciapb, ne studia la relazione con le altre variabili carattere;
%LET VAR2= FasciaPeso FasciaEma BicchieriAlG FasciaTr PIAABI DURESP;
%MACRO GraficiPBEMIA;
	%DO i=1 %TO &NUM;
		%LET v=%SCAN(&VAR2, &i);
		**%Grafico(&v,FasciaPb);
		%Grafico(FasciaPb, &v);
	%END;
%MEND GraficiPBEMIA;

goption vsize=10cm hsize=20cm; 
goption reset=(pattern);
pattern1 c=darkseagreen; 
pattern2 c=navajowhite;
pattern3 c=stypk;
pattern4 c=firebrick;
pattern5 c=moybr;
%GraficiPBEMIA;
ods pdf close;
quit;


********************************************************************************************************
REGRESSIONE LINEARE generale;

proc corr data = piomb nosimple;
var pbemia ematoc;
run;

goption reset=all vsize=10cm hsize=15cm; 
proc gplot data=piomb;
plot ematoc*pbemia;
format FasciaEta FasciaEta.;
symbol1 v=dot c=darkslategray i=rl ci=grg;
TITLE "Regressione Lineare tra PBEMIA ed EMATOC";
run;quit;
title;


proc reg data=piomb plots(only)=residuals;
model ematoc = pbemia;
run;

**CORRELAZIONE tra ematoc e pbemia per ciascun livello delle variabili carattere;

%let qualit=Gender DURESP PIAABI FasciaEta FasciaPeso FasciaTr FasciaEma FasciaPb BicchieriAlG;
%let num9=9;
%macro correlazione; 
    %do i=1 %to &num9;
        %let p=%SCAN(&qualit, &i);
        proc sort data = piomb;
            by &p;
        proc corr data = piomb outp=CorrIn&p. noprint ;
            var ematoc pbemia;
            by &p;
        run;
        data aux&p. numero&p.;
            set CorrIn&p.;
            format &p &p..;
            if _TYPE_ = "N" then output numero&p.;
            if _TYPE_ = "CORR" and _NAME_ = "PBEMIA" then output aux&p.;
        data A&p.;
            set numero&p.(keep=PBEMIA &p.);
            rename PBEMIA=OBS;
        proc sql;
        create table finale&p. as
        select &p., Ematoc as PBEMIAxEMATOC, OBS
        from A&p. natural full join aux&p.;
        quit;
        proc print data = finale&p.; 
        	format &p &p..;
            title "Correlazione PBEMIA-EMATOC per &p.";
        run;
        title;
        proc gplot data = piomb;
            format &p &p..;
            title "Regressione lineare tra PBEMIA ed EMATOC per &p.";
            plot ematoc*pbemia=&p;
            symbol v = diamondfilled i=rl;
        run;quit;
        title;
    %end;
%MEND CORRELAZIONE;

goption reset=all vsize=10cm hsize=15cm; 
symbol1 v=dot c=gold i=r ci=gold;
symbol2 v=dot c=big i=r ci=big;
symbol3 v=dot c=indianred i=r ci=indianred;
symbol4 v=dot c=cornflowerblue i=r ci=cornflowerblue;
symbol6 v=dot c=bippk i=r ci=bippk;
symbol5 v=dot c=mool i=r ci=mool;
%correlazione;

**altri modelli;
data piomb;
set piomb;
log_pbemia=log(pbemia);
square_pbemia=pbemia*pbemia;
cub_pbemia=square_pbemia*pbemia;
run;

**logaritmico;
proc reg data=piomb plots(only)=residuals;
model ematoc = log_pbemia;
title "Residui del modello logaritmico";
run;
title;

**quadratico;
proc reg data=piomb plots(only)=residuals;
model ematoc = square_pbemia;
title "Residui del modello quadratico";
run;
title;

**cubico;
proc reg data=piomb plots(only)=residuals;
model ematoc = cub_pbemia;
title "Residui del modello cubico";
run;
title;
run;

**matrice di correlazione tra le variabili quantitative;
proc corr data=piomb nosimple noprob;
var BMI ETAPRE ABIALC ESPBTR EMATOC PBEMIA;
run;

		