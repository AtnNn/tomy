:- use_module(library('http/http_open')).
:- use_module(library('http/json')).
:- use_module(library('readutil')).
:- use_module(library('sgml')).

:- maplist(add_allowed_predicate,
           [+writes/1, +writelns/1, http_get/2, match/2, expect_match/2, query_escape/2,
            google(_,_,_),+google(_),http_get_json/2,+into(_,_),translate/4,
            detect_language/2]).

writes(S) :- writef("%s",[S]).

writelns(S) :- writes(S), nl.

http_get(Url, Codes) :-
	http_get(Url, Codes, [], []).

http_get(Url, Codes, HttpOpts, StreamOpts) :-
	http_open(Url, Stream, HttpOpts),
	maplist(set_stream(Stream),StreamOpts),
	read_stream_to_codes(Stream, CodesT),
	(append(Codes, [timeout(_)], CodesT)
	;Codes=CodesT).

http_get_json(Url, Json) :-
    http_open(Url, Stream, []),
    json_read(Stream, Json).

http_get_json(Url, Json, HttpOpts, StreamOpts) :-
    http_open(Url, Stream, HttpOpts),
    maplist(set_stream(Stream),StreamOpts),
    json_read(Stream, Json).

match(S, L) :- exact_match(S, L, _).
match([_|S], L) :- match(S, L).

exact_match(S, L) :- exact_match(S, L, []).

exact_match(S, [], S).
exact_match(S, [P|L], R) :-
	\+var(P),
	P=once(K),
	exact_match(S, K, T),
	!,
	exact_match(T, L, R).
exact_match(S, [P|L], R) :-
	single_match(S, P, T),
	exact_match(T, L, R).

single_match(S, [], S).
single_match([C|S], [C|P], R) :-
	single_match(S, P, R).

query_escape(Q, E) :-
	atom_chars(Q, S),
	maplist(query_escape_char,S,L),
	concat_atom(L,E).

query_escape_char(' ', '+') :- !.
query_escape_char('&', '%24') :- !.
query_escape_char('|', '%7C') :- !.
query_escape_char(C, E) :-
        atom_codes(C,[N]),
        (N > 127; N < 32),
	!,
	(N=0, E='%00'; percent_hex(N, E)).
query_escape_char(C,C).

percent_hex(0, '').
percent_hex(N, E) :-
	M is N mod 256,
        (M < 16, format(atom(C), "%0~16r", [M]);
                 format(atom(C), "%~16r", [M])),
	L is floor(N/256),
	percent_hex(L,D),
	concat_atom([C,D],E).

google(Q,X,T) :-
	query_escape(Q, E),
	concat_atom(['http://www.google.com/m/search?q=',E],U),
	http_get(U, C, [], [encoding(utf8)]),
	%match(C, ["<a title=\"",once([T,"\" href=",X,">"])]).
	match(C, [";u=",once([X,"\">",T,"<"])]).

:- op(1150, fx, google).

google(Q) :-
	R=r(fail),
	((google(Q,X,T),
	  nb_setarg(1,R,true),
	  append([X, " - ", T], M),
	  writelns(M),
	  fail);
	 (R=r(X), X)).

detect_language(Text, Lang) :-
        query_escape(Text, E),
        concat_atom(['http://ajax.googleapis.com/ajax/services/language/detect?v=1.0&q=',E], U),
        http_get_json(U, json([responseData=json([language=Lang|_])|_]), [], [encoding(utf8)]).

translate(Text, From, To, Ret) :-
        query_escape(Text, E),
        concat_atom(['http://ajax.googleapis.com/ajax/services/language/translate?v=1.0&q=',E,'&langpair=',From,'%7C',To], U),
        http_get_json(U, json([responseData=json([translatedText=Ret|_])|_]), [], [encoding(utf8)]).

:- op(1150, xfx, into).

into(Text, To) :-
        detect_language(Text, From),
        translate(Text, From, To, Ret),
        writeln(Ret).

:- op(1150, fx, weather).

weather(Loc) :-
        query_escape(Loc, LocE),
        concat_atom(['http://www.google.com/ig/api?weather=', LocE], Url),
        http_open(Url, Stream, []),
        load_structure(Stream, [element(xml_api_reply, _, [element(weather,_,Cond)])], []),
        write(Cond),
        member(element(current_conditions, _, Resp), Cond),
        write(Resp),
        member(element(condition,[data=Cond],_), Resp),
        member(element(temp_f,[data=Temp],_), Resp),
        member(element(mumidity,[data=Hum],_), Resp),
        member(element(wind_condition,[data=Wind],_), Resp),
        append([Cond, ", ", Temp, "F, ", Hum, ", ", Wind], Msg),
        writeln(Msg).
