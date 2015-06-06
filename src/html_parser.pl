
% parse_document(...) will take a html file (intput) argument
% and following 10 (output) arguments:
% pelna nazwa emitenta
% skrocona nazwa emitenta
% sektor
% NIP
% Regon
% Data sporzadzenia
% nr komunikatu
% temat
% podstawa prawna
% tresc raportu


:- module(html_parser, [parse_sample/0, parse_report/12, parse_contents/7]).


:- use_module(library(sgml)).
:- use_module(library(xpath)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Common
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%parse_sample
parse_sample :-
	parse_report('sample-2.html', Symbol, FullName, ShortName, Sector, Nip, Regon, Date, ReportNumber, Title, LegalBasis, Content),

	writeln(''), writeln('** Results: **'),
	write('Symbol type (eng/non-eng): '), writeln(Symbol),
	write('Full name: '), writeln(FullName),
	write('Short name: '), writeln(ShortName),
	write('Sector: '), writeln(Sector),
	write('Nip: '), writeln(Nip),
	write('Regon: '), writeln(Regon),
	write('Date: '), writeln(Date),
	write('Report number: '), writeln(ReportNumber),
	write('Title: '), writeln(Title),
	write('Legal basis: '), writeln(LegalBasis),
	write('Content: '), writeln(Content),
	parse_contents(LegalBasis, IsLegalBasisSuitable, Content, InitialLyrics, IsPacketTransaction, Shares, Price),
	(
	    IsLegalBasisSuitable =:= 1 ->
	    writeln('Legal basis matching the filter'),
	    write('Initial lyrics: '), writeln(InitialLyrics),
	    (
	        IsPacketTransaction =:= 1 ->
		writeln('The transaction consisted of multiple packets')
		;
	        writeln('The transaction consisted of a single packet')
	     ),
	    write('Total number of shares exchanged during the transaction'), writeln(Shares),
	    write('Total price of shares exchanged during the transaction'), writeln(Price)
	    ;
	    writeln('Legal basis not matching the filter')
	).

parse_contents(LegalBasis, IsLegalFlag_OUT, TextToParse, InitialLyrics_OUT, IsAPacket_OUT, Shares_OUT, Price_OUT) :-
	(
	    a_substring(LegalBasis,'Art. 160 ust 4 Ustawy o obrocie') ->
	    IsLegalFlag_OUT = 1
	    ;
    write('Legal basis: '),writeln(LegalBasis),
	    IsLegalFlag_OUT = 0
	).

% a substring DCG
substr(String) --> ([_|_];[]), String,  ([_|_];[]).
% a wrapper for a substring DCG
substring(X,Y) :- phrase(substr(X),Y).

% a substring function for atoms
a_substring(X,Y) :-
	sub_atom(X,_,_,_,Y).


% parse_report(-Html, +,+,+...)
parse_report(Html, Symbol, FullName, ShortName, Sector, Nip, Regon, Date, ReportNumber, Title, LegalBasis, Content) :-

	write('Parsing '), writeln(Html),
	load_html(Html, DOM, []),

	find_summary_data_table(DOM, SummaryTable),

	symbol_report(SummaryTable, Symbol),
	full_issuer_name(SummaryTable, FullName),
	short_name(SummaryTable, ShortName),
	sector(SummaryTable, Sector),
	nip(SummaryTable, Nip),
	regon(SummaryTable, Regon),
	number_report(SummaryTable, ReportNumber),
	title(SummaryTable, Title),
	full_issuer_name(SummaryTable, FullName),

	find_report_table(DOM, MessageTableBody),

	legal_basis(MessageTableBody, LegalBasis),
	content(MessageTableBody, Content),
	date(MessageTableBody, Date).

	% structure varied in different docs, so using date from
        % the report content table
	% find_represent_table(DOM, RepresentTable).
	% date_from_representative_table(RepresentTable, Date).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Tools
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% finds data inside a cell specified by Row, Col, contained in table
% body TBody
%
% data_at_index_path(-TBody, ?Row, ?Cell, +Data)
data_at_index_path(TBody, Row, Cell, Data) :-
	xpath(TBody, //tr(Row), Tr2),
	xpath(Tr2, //td(Cell), Temp),
	Temp = element(td, _ , DataList),
	extract_data_from_list(DataList, Data).

% if the list is empty, i.e. there was no data inside a cell, whole
% parsing fails, hence unify Data to empty string, if it is the case.
extract_data_from_list(DataList, Data) :-
       %( if(  data exists  ) -> ...  else  unify    ).
	( DataList = [Data|_] -> true   ;   Data=''  ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Report
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% find_report_table(-DOM, +TableBody)
% finds table body with the stock message
find_report_table(DOM, TableDataBody) :-
	xpath(DOM, //div((@class=dane)), Div),
	xpath(Div, //table(3), Table1),
	xpath(Table1, //tbody, TBody1),
	xpath(TBody1, //tr(1), Tr1),
	xpath(Tr1, //td(1), Td1),
	xpath(Td1, //table(1), Table2),
	xpath(Table2, //tbody , TableDataBody).

content(TableBody, Data) :-
	xpath(TableBody, //tr(12), Tr2),
	xpath(Tr2, //td(2), Temp),
	Temp = element(td, _ , Data).
	%writeln(Temp),
	%Data = [Temp|_].
	%extract_data_from_list(DataList, Data),
	%writeln(Data).
	%data_at_index_path(TableBody, 12, 2, Data).

legal_basis(TableBody, Data) :-
	data_at_index_path(TableBody, 9, 2, Data).

date(TableBody, Date) :-
	data_at_index_path(TableBody, 3,3, Date).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Summary data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%  depracated, the structure of this table varied
%%
% find_message_table(-DOM, +Table)
% finds table body with info about PERSONS REPRESENTING THE COMPANY
% find_represent_table(DOM, TableDataBody) :-
%	xpath(DOM, //div((@class=dane)), Div),
%	xpath(Div, //table(5), Table1),
%	xpath(Table1, //tbody, TBody1),
%	xpath(TBody1, //tr(1), Tr1),
%	xpath(Tr1, //td(1), Td1),
%	xpath(Td1, //table(1), Table2),
%	xpath(Table2, //tbody , TableDataBody).


% find_summary_data_table(-DOM, +SummaryTable)
% finds table body with company data
find_summary_data_table(DOM, SummaryTable) :-
	xpath(DOM, //div((@class=dane)), Div),
	xpath(Div, //table(@class=nDokument), SummaryTable).

%%
%% summary_data_predicates(-Table, +Data)
%%

% depracated, use date from ReportTable.
% in different docs it had different structure
% date_from_representative_table(RepresentTable, Date) :-
%	data_at_index_path(RepresentTable, 4, 2, Date).

full_issuer_name(SummaryTable, Name) :-
	data_at_index_path(SummaryTable, 4, 2, Name).

short_name(SummaryTable, Shortname) :-
	data_at_index_path(SummaryTable, 5, 2, Shortname).

title(SummaryTable, Title) :-
	data_at_index_path(SummaryTable, 6, 2, Title).

sector(SummaryTable, Sector) :-
	data_at_index_path(SummaryTable, 7, 2, Sector).

nip(SummaryTable, NIP) :-
	data_at_index_path(SummaryTable, 15, 2, NIP).

regon(SummaryTable, Regon) :-
	data_at_index_path(SummaryTable, 16, 2, Regon).

symbol_report(SummaryTable, Symbol) :-
	data_at_index_path(SummaryTable, 3, 2, Symbol).

number_report(SummaryTable, Number) :-
	data_at_index_path(SummaryTable, 19, 2, Number).

