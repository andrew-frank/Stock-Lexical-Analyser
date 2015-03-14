% final export
% :- module(html_parser, [parse_document/11, parse_sample/10]).

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

% temp export
:- module(html_parser, [parse_sample/2]).


:- use_module(library(sgml)).
:- use_module(library(xpath)).


% example: parse_sample(DOM,_).
% for now will print out Full issuer name and it's NIP

%parse_sample(-DOM, ?Element)
parse_sample(DOM, Element) :-
	parse_report('sample-page.html', DOM, Element).
	%write(Element).


% Main data is contained in:
% body, div, table, (tbody), tr, td, table, (tbody), 14 <tr>s of data we
% want with many <td>s (number varies), but always 1st <td> is empty,
% data is most likely in second td

% parse_report(-Html, ?DOM, +Element)
parse_report(Html, DOM, Element) :-
	write('Parsing '), writeln(Html),
	load_html(Html, DOM, []),
	writeln('Html loaded and converted to list. Searching for fields'),
	find_message_table(DOM, MessageTableBody),
	%data_at_index_path(MessageTableBody, Data),
	%writeln('Found message data cell: '),
	%writeln(Data).
	find_company_data_table(DOM, CompanyDataTable),
	full_issuer_name(CompanyDataTable, FullName),
	write('Full issuer name: '),
	writeln(FullName),
	nip(CompanyDataTable, Nip),
	write('NIP: '),
	writeln(Nip).

% find_message_table(-DOM, +Table)
% finds table body with the stock message
find_message_table(DOM, TableDataBody) :-
	xpath(DOM, //div(table), Div),
	xpath(Div, //table, Table1),
	xpath(Table1, //tbody, TBody1),
	xpath(TBody1, //tr, Tr1),
	xpath(Tr1, //td, Td1),
	xpath(Td1, //table, Table2),
	xpath(Table2, //tbody(tr(@class=nTekst)), TableDataBody).


% (-DOM, +CompanyDataTable)
% finds table body with company data
find_company_data_table(DOM, CompanyDataTable) :-
	xpath(DOM, //div(table), Div),
	xpath(Div, //table(@class=nDokument), CompanyDataTable).

% (-, +)
full_issuer_name(CompanyDataTable, Name) :-
	xpath(CompanyDataTable, //tr(4), Tr),
	xpath(Tr, //td(2), Kupa),
	Kupa = element(td, _, Name).

% (-, +)
nip(CompanyDataTable, NIP) :-
	xpath(CompanyDataTable, //tr(15), Tr),
	xpath(Tr, //td(2), Kupa),
	Kupa = element(td, _, NIP).

% temp - passing arguments to //tr(Row) and //td(Col) does not work :(
% this language is stupid as shit and we'll have to type manually cell
% index paths
data_at_index_path(TBody, Data) :-
	xpath(TBody, //tr(5), Tr2),
	xpath(Tr2, //td(2), Kupa),
	Data = element(td, [],Kupa).
