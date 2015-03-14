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


% example:
% parse_sample(DOM,_).

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
	xpath(DOM, //div(table), Div),
	xpath(Div, //table, Table1),
	xpath(Table1, //tbody, TBody1),
	xpath(TBody1, //tr, Tr1),
	xpath(Tr1, //td, Td1),
	xpath(Td1, //table, Table2),
	xpath(Table2, //tbody(tr), TBody2),
        %writeln('Table 2'), writeln(Tbody2),
	xpath(TBody2, //tr, Tr2),
	xpath(Tr2, //td(2), Td2),
	writeln(' *************** ELEMENT ***************'), writeln(Td2).
	%writeln('Element'), writeln(Element).
	%Element = element(Element, [], E),
	%write('Element: '), writeln(Element).









