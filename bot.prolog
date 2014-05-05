%% Tomy, the Prolog bot
%% Copyright (C) 2007-2010 AtnNn (atnnn@atnnn.com)
%% http://www.atnnn.com/tomy/


%% *** Please read the README file before running this bot ***


%% CONFIGURATION

%% List of allowed clauses for the code-walker
allowed_predicates_file('allowed-predicates').

%% Initial clauses are read from this file, and new ones are appended to it
stored_predicates_file('stored-predicates').

%% server, port, nickname and channel to use
%:- dynamic server/4.
server('irc.freenode.net',6667,'Tomy','##prolog').

%% Maximum number of characters per line
max_line_length(240).


%% TODO

% refactor
% use swi's dwim
% trace/0
% safe_format: ~@
% multiline predicates
% fold assertz, retract and safe_op clauses in the stored file
% make sure code is safe for async exceptions
% passing multiline arguments to send_line (make send_lines?)
% identify with nickserv


%% CODE

% main predicate
run_bot :- load_data, connect, irc_master_loop.

% connect to the IRC server and start the handshake
connect :- server(Server, Port, Nick, _),
        tcp_socket(Socket),
        tcp_connect(Socket, Server:Port),
        tcp_open_socket(Socket, In, Out),
        assert(streams(In, Out)),
        format(Out,'NICK ~a~n',[Nick]),
        % I always send USER like this, but it might not be the right way
        format(Out,'USER ~a ~a ~a :~a~n',[Nick,Nick,Nick,Nick]),
        flush_output(Out).

% streams(-In, -Out)
% the connection with the server
:- dynamic streams/2.

% disconnect from the server (this predicate is never used)
% to kill the bot, Ctrl+c then e
disconnect :- streams(I, O), close(I), close(O), retract(streams(_,_)).

% load stored predicates and allowed predicates
load_data :- allowed_predicates_file(F),
        read_file_to_terms(F,L,[]), 
        % add each predicate individually (for maplist, see below)
        maplist(add_allowed_predicate, L),
        load_lib(lib),
        stored_predicates_file(G),
        open(G,read,S),
        % read each predicate one by one and load it until end of file
        % nb: past this point, load_data/0 cannot fail
        % this is not correct if read can fail (can read fail?)
        % simply ignore if load_stored fails since errors often creep into the file 
        repeat, read(S, T),
        (T = end_of_file; load_stored(T), fail).

% load additional predicates
load_lib(L) :-
        [L],
        (catch(read_file_to_terms(L,T,[]),_,fail);
            concat_atom([L,'.pl'],N), read_file_to_terms(N,T,[])),
        maplist(lib_add_reserved, T).

% reserve a predicate defined in a library
lib_add_reserved(:-_) :- !.
lib_add_reserved(P:-_) :- !, add_reserved_predicate(P).
lib_add_reserved(P) :- add_reserved_predicate(P).


% maplist(_, []) :- !.
% maplist(P, [X|Xs]) :- call(P,[X]), maplist(P, Xs).

% reloads the two files, can be used from IRC
%reload :- prepare_allowed(A,_), catch(retractall(A),_,fail), fail.
%reload :- user_op(O), op(0,fx,O).
%reload :- [lib], load_data.

% onbt(+Goal)
% execute Goal when backtracking
% used for debugging and by many_calls_with_time_limit/2
onbt(_).
onbt(G) :- G, fail.

% call_after(+Goal, +Cleanup)
% always calls Cleanup after Goal is done
% this might be the wrong way to do this
call_after(A, B) :- call_cleanup(A, R, ((R=fail; R=exception(_)), B)), B.

% the top-level loop, an infinite loop that never fails
irc_master_loop :- repeat, catch_new_query(irc_loop).

% block/3 has been replaced by throw/catch
catch_new_query(G) :- catch(G, new_query(Q, B), catch_new_query(new_query(Q, B))).

% the simple loop, this one can fail
irc_loop :- streams(I, O), irc_next(I,O), !, irc_loop.

% IMPORTANT NOTE:
% some of the following predicates should only fail when a
% predicate from an IRC user fails, so that backtracking
% with ; on IRC is possible.

% irc_next(+StreamIn, +StreamOut)
% read, parse and dispatch the next line from the server
irc_next(I,O) :-
        read_line_to_codes(I, Line),
        irc_parse(Line, Parts),
        !, irc_line(Parts, O).

% ircline(+ParsedLine, +StreamOut)

% throw an exception if the connection dies
irc_line([end_of_file], _) :- throw(disconnected).

% for debugging: display the parsed line to stdout
irc_line(L, _) :- show_list(L,' '), nl, fail.

% the eternal game of pingpong between clients and servers
irc_line(["PING",X], O) :- !, format(O, 'PONG :~s~n', [X]), write('Pong!\n'), flush_output(O).

% If the connection was successfull, the server replies with an 001
% and we send the JOIN command
irc_line([_,"001"|_], O) :- !, server(_,_,_,Chan), format(O, 'JOIN ~a~n', [Chan]), flush_output(O).

% not implemented: 443 is invalid nickname error, need to resend NICK
%irc_line([_,"433"|_], O) :- ..

% if it's a message to a channel, try parsing it as a prolog term
% and then pass it to irc_msg
irc_line([W,"PRIVMSG",[35|_],Msg], _) :- atom_codes(A, Msg), !,
        (catch(atom_to_term(A, T, B),E,T=error(E)) -> irc_msg(W,T,B,Msg); true).

% failure is not an option
irc_line(_, _) :- true.

% for consistency, use the same operators that IRC does
:- op(600, fx, ':').
:- op(500, xfx, '!').
:- op(400, xfx, '@').

% irc_parse(+Line, -ParsedLine)
% most IRC message look like this:
% Line = ':nick!ident@host command argu ments :last argument'
%         +--------------+ +-----+ +--------+ +------------+
% the first and last part are optional. For this example the result is:
% ParsedLine = [:Nick!Ident@Host, Command | Arguments]

irc_parse([58|R], [:X|Xs]) :- !, parse_nih(R, X, L), irc_parse_rest(L, Xs).
irc_parse(L, X) :- irc_parse_rest(L, X).

% irc_parse_rest(+Rest, -Splitted)
irc_parse_rest([],[]) :- !.
irc_parse_rest([58|R],[R]) :- !.
irc_parse_rest(L, [X|Xs]) :- append(X,[32|R],L), !, irc_parse_rest(R, Xs).
irc_parse_rest(X,[X]).

% parse_nih(+Line, Nih, Rest)
parse_nih(L, N!I@H, R) :-
        append(N,[33|K], L),
        append(I,[64|J], K),
        append(H,[32|R], J), !.
parse_nih(L, H, R) :-
        append(H,[32|R], L).

% irc_msg(+Who, +Terms, +Bindings, +Line)
% a Line was sent to the channel by Who
% Terms and Bindings is the result of atom_to_terms
% Terms is error(Error) if the line was not parsed
% the code is seperated like this (see irc_line/2) because
% I originally did not use the Bindings and Error (TODO: refactor)

% debugging output
% irc_msg(_,T,_,_) :- write('got msg: '), write(T), nl, fail.

% the line is actually a capitalised word
% either someone knows how to capitalise first words
% or they think caps-lock is cool (LOL)
irc_msg(_,V,_,_) :- var(V), !.

% the user tried to query the bot but there was an error
irc_msg(_,error(E),_,M) :- not(var(E)), !,
	% if it's a query, show the error message
        (   append("?-",_,M),
            message_to_string(E,S),
            wrap(write(S)),
            capture_output,
            send_more(2)
	; true).

% the user wants to backtrack, so we fail
% see note after irc_loop/0
irc_msg(_,;,_,_) :- !, fail.

% the message is a prolog query (see README for examples)
% note: it's hard to indent higher-order predicates in prolog
irc_msg(_,'?-'(Q),B,M) :-
	append("?-",_,M),
        % ignore previous choice points
	throw(new_query(Q, B)).

% there is a one line definition of a predicate prefixed with tomy,
% so safe_assertz it
irc_msg(_,T, _, M) :-
	( T=(Tomy, A :- B), P = (A :- B)
        ; T=(Tomy, P), functor(P,_,_)
	; T=(Tomy: A :- B), P = (A :- B)
        ; T=(Tomy: P), functor(P,_,_)),
	last(M,46),
	(var(Tomy), append("Tomy",[C|_],M), member(C, ":,")
	; ground(Tomy), Tomy=tomy),
	!,
	(tomy_msg(P); true).

% never fail unless ';'
irc_msg(_, _, _, _).

new_query(Q, B) :-
	Ex=ex(false),
        % evaluate the query with a time limit of 3 seconds
        (catch(call_after(many_calls_with_time_limit(5,exited(eval(Q), Done)),
                          % when it's done, even if it failed, send
                          % the output to the channel if there is any
                          (writeln(always), output_strings([]), !; capture_output, send_more(2))),
               % if there is an exception
               E,
               (nb_setarg(1, Ex, true),
	         % assign the variables to their name and try to treat the exception
                 (maplist(ignore,B), (treat_exception(E), !;
                 % if the exception is untreatable, notify the channel
                 message_to_string(E,S), send_line(['Exception: ',S])), fail)));
         % if it failed, send No to the channel
         (Ex=ex(false), send_line('No'), fail)),
        % build the list of variables
        list_variables(B,V),
        % build a reply, send it to the server (and to stdout for debugging)
        build_reply(V, Done, R), writeln(R), send_line(R,', '),
        % restart the irc loop so it backtracks back to the query on ';'
        irc_loop.

tomy_msg(C) :-
	catch(safe_assertz(C,_),E,(
	  message_to_string(E,S),
	  (treat_exception(E), !; send_line(['Error: ',S]))
        )), !.
tomy_msg(_) :- send_line(['No']).



% build_reply(+Variables, +Done, -Reply)
% build the reply to the query
build_reply([], _, ['Yes']) :- !.
build_reply(V, false, V) :- !.
build_reply(V, true, R) :- append(V,['Yes'],R).

% list_variables(+Bindings, -StringList)
% a list of variables for the reply
list_variables([],[]) :- !.
list_variables([V=X|B], R) :- var(X), !, X=V, list_variables(B, R). % TODO: it shows A='B'
list_variables([V=X|B], [V=S|R]) :- with_output_to(string(S), writeq(X)), list_variables(B, R).

% treat_exception(+Exception)
% some exceptions are treated differently by the bot
% commands like more/0 and help/0 throw an exception catched here (it hides the trailing Yes)
treat_exception(illegal(T)) :- send_line(['Error: ',T,' is not allowed']).
treat_exception(illegal_op(P,T,O)) :- send_line(['Error: ',op(P,T,O),' is not allowed']).
treat_exception(show(info)) :- send_line('I am Tomy, the Prolog IRC bot (http://www.atnnn.com/tomy)').
treat_exception(show(more)) :- more_lines([]), send_line('No more'); send_more(3).
treat_exception(unknown(P)) :- 
        ( ( P=F/A, functor(PP,F,A), dwim_predicate(PP,PPP), send_line(['Unknown predicate ',P,', did you mean ',PPP,'?']) )
         ;  send_line(['Unknown predicate ',P])).
treat_exception(invalid(G)) :- send_line(['Invalid goal ',G]).
treat_exception(time_limit_exceeded) :- send_line(['Time limit exceeded']).
treat_exception(reload) :- send_line('Reloading modules and configuration...'), !, reload.
treat_exception(reserved(P)) :- send_line(['Error: ', P, ' can not be redefined']).

% wrap(+Goal)
% "store" output from Goal "into" output_strings/1
% this is used on predicates preceded by a '+' in the allowed predicates file
% this is necessary because always using with_output_to/2 breaks backtracking with ';'
wrap(G) :- with_output_to(string(S), G), add_output_string(S).

% format the captured strings and "put" the result in more_lines/1
capture_output :- output_strings(R), reverse(R,S), with_output_to(codes(T), write_list(S,'')),
        split_lines(T,L), set_more_lines(L), clear_output_strings.

% send_more(+Amount)
% send Amount more lines to the channel (or Amount+1 if there are Amount+1 lines left)
send_more(_) :- more_lines([]), !.
send_more(0) :- more_lines([X]), !, send_line(X).
send_more(0) :- !, more_lines(L), length(L, N), send_line(['(',N,' more lines)']).
send_more(N) :- !, more_lines([X|L]), send_line(X), set_more_lines(L), M is N-1, send_more(M).

% the two "variables"
:- dynamic more_lines/1, output_strings/1.
more_lines([]).
output_strings([]).

% add_output_string(+String)
% "prepend" the string to output_strings/1
add_output_string(S) :- output_strings(Ss), retractall(output_strings(_)), assert(output_strings([S|Ss])), !.

% "empty" output_strings/1
clear_output_strings :- retractall(output_strings(_)), assert(output_strings([])).

% "set" more_lines/1
set_more_lines(L) :- retractall(more_lines(_)), assert(more_lines(L)).

% split_lines(+String, -Lines)
% split a multiline String into a list of Lines
split_lines([],[]) :- !.
split_lines([10|T],L) :- !, split_lines(T,L).
split_lines(T,K) :- append(X,[10|R],T), !, split_line(X,Y), split_lines(R, L), append(Y,L,K).
split_lines(X,Y) :- split_line(X, Y).

% split_line(String, Lines)
% split a long line into a list of strings (see max_line_length/1)
% the first one  prefixed by '| ' and the others by '> '
split_line(X,Y) :- split_line(124,X,Y).

% split_line(+Prefix, +String, -Lines)
split_line(P, X, [S|Y]) :- max_line_length(N), M is N-2, length(L,M), append(L,R,X), !, string_to_list(S, [P,32|L]), split_line(62,R,Y).
split_line(P, X, [S]) :- string_to_list(S, [P,32|X]).

% send_line(+Something)
% send_line is used everywhere in the code to send something to the channel
% but it is not used consistently, so it is a big ball of DWIM
send_line(T) :- send_line(T,[]).

% send_lines(+Something, +SomethingElse)
send_line(T,L) :-
        streams(_,O),
        server(_,_,_,C),
        % write the IRC command to send a line
        format(O,'PRIVMSG ~a :',C),
        with_output_to(string(S),
        % DWIM!
           ((T=[];T=[_|_]), (\+(L=[]), P=L; P=''), write_list(T,P);
           L=[], write(T);
           format(T,L))),
        % cut it if it is too long
        string_length(S,N), max_line_length(M),
        (N=<M, !, write(O,S); X is M-3, sub_string(S,0,X,_,U), write(O,U), write(O,'...')),
        nl(O), flush_output(O).

% prepare_allowed(+Goal, -SafeGoal)
% prepare_allowed/2 is like prepare/2
% it is used to convert stored and user predicates into safe ones

% user_op(-Op)
% Op is an operator defined by a user

:- dynamic prepare_allowed/2, user_op/1.

% add_allowed_predicate(+Predicate)
% allow the use of a built-in predicate
add_allowed_predicate(X) :- write('*** '), writeln(X), add_allowed_predicate(X, '-').

% add_allowed_predicate(+Predicate, +Type)
% Type is '-' for plain or '+' for wrap

% Predicate is Name/Arity with optional '+'
add_allowed_predicate(+P/N,'-') :- !, functor(T,P,N), assertz(prepare_allowed(T,wrap(T))), add_reserved_predicate(P/N).
add_allowed_predicate(P/N,'-') :- !, functor(T,P,N), assertz(prepare_allowed(T,T)), add_reserved_predicate(P/N).

add_allowed_predicate(+T,'-') :- !, add_allowed_predicate(T, '+').

% predicate is Name(Args, ...)
% args are one of _ (variable) or '#'
% fo example, for add_allowed_predicate(foo(_,#), '+')
add_allowed_predicate(T, Out) :-
        T =.. [P|N],               % P = foo, N = [_1, #]
	maplist(allowed_spec_var, N), !, % only _ or #
        unmark(N, L, V, K, W),     % L = [_1, _2], V = [_2], K = [_1, _3], W = [_3]
        C =.. [P|L],               % C = foo(_1, _2)
        D =.. [P|K],               % D = foo(_1, _3)
        gen_prepare(V, W, B),      % B = (prepare(_2, _3),true)
        (Out='+', E=wrap(D); E=D), % E = wrap(foo(_1, _3))
        % so now prepare_allowed(foo(_1, _2), foo(_1, _3)) :- prepare(_2, _3), true.
        assertz(prepare_allowed(C,E) :- B),
	add_reserved_predicate(T).

% error
add_allowed_predicate(_,_) :- write('failed'), nl.

allowed_spec_var(X) :- var(X), !.
allowed_spec_var(#).


% unmark(+Args, L1, L2, L3, L4)
% Args: list of _ or '#'
% L1 and L2: same list with different new variables instead of '#'
% L3 and L4: list with corresponding new variables from L1 and L2
% see add_allowed_predicate/2
unmark([], [], [], [], []).
unmark([A|N], [A|L], V, [A|K], W) :- var(A), !, unmark(N, L, V, K, W).
unmark([#|N], [A|L], [A|V], [B|K], [B|W]) :- unmark(N, L, V, K, W).

% gen_prepare(+From, +To, -Goal)
% creates a Goal that calls prepare on each element of the two lists in parallel
gen_prepare([], [], true).
gen_prepare([V|VS], [W|WS], (prepare(V,W),R)) :- gen_prepare(VS, WS, R).

% load_stored(+PredicateOrQuery)
% loads a predicate or query that comes from the stored predicates file
load_stored(X) :- write('... '), writeln(X), fail.
load_stored(:- T) :- T.
load_stored(C) :- assertz(C), (C=P; C=(P:-_)), functor(P,F,A),
        functor(T,F,A), (prepare_allowed(T,_); assertz(prepare_allowed(T, T))).

% prepare(+Goal, -BotGoal)
% transform Goal into a goal that the bot can run safely
% and change some of the goals that are built-in

% make an unknown goal safe
prepare(X, eval(X)) :- var(X), !.

% some built-in commands just throw an exception
prepare(more, throw(show(more))).
prepare(help, throw(show(info))).
prepare(reload, throw(reload)).
prepare(abort, _) :- throw(abort).

% transform some predicates into safe versions
prepare(apply(F,A), safe_apply(F,A)).
prepare(assert(C), safe_assertz(C,_)).
prepare(asserta(C), safe_asserta(C,_)).
prepare(assertz(C), safe_assertz(C,_)).
prepare(assert(C,R), safe_assertz(C,R)).
prepare(asserta(C,R), safe_asserta(C,R)).
prepare(assertz(C,R), safe_assertz(C,R)).
prepare(T, safe_apply(F, A)) :- T =.. [call, F|A].
prepare(retract(C), safe_retract(C)).
prepare(retractall(C), safe_retractall(C)).
prepare(bagof(A,B,C), safe_bagof(A,B,C)).
prepare(setof(A,B,C), safe_setof(A,B,C)).
prepare(op(A,B,C), safe_op(A,B,C,new)).

% allow only other chosen and user-defined predicates
prepare(T, C) :- prepare_allowed(T, C), !.

% deny other built-in, unknown or invalid predicates
prepare(T, _) :- functor(T, F, N), functor(C, F, N),
        catch(clause(C,_),error(permission_error(access, private_procedure, _),_),true),
        throw(illegal(F/N)).
prepare(T, _) :- functor(T,F,A), throw(unknown(F/A)).
prepare(T, _) :- throw(invalid(T)).

% eval(+Goal)
% goals from the user are prepared before being run
eval(T) :- prepare(T, Tp), !, Tp.

% safe_apply(+Predicate, +Arguments)
safe_apply(F, A) :- F =..[P|L], append(L,A,R), G=..[P|R], eval(G).

% safe_assert?(+Clause, -Ticket)
% prepare it, eval it, allow it and save it
safe_asserta(C,R) :- ensure_not_reserved(C), prepare_clause(C, T, P), asserta(T,R), assertz(prepare_allowed(P,P)), save_clause(:-asserta(T)).
safe_assertz(C,R) :- ensure_not_reserved(C), prepare_clause(C, T, P), assertz(T,R), assertz(prepare_allowed(P,P)), save_clause(T).

% safe_retract*(+Clause)
% retract the prepared clause and "unsave" it
safe_retract(C) :- prepare_clause(C, T, _), retract(T), save_clause(:-retract(T)).
safe_retractall(C) :- prepare_clause(C, T, _), retractall(T), save_clause(:-retractall(T)).

% safe_op(+Precedence, +Type, +Op, +SavedOrNew)
% suggested by faxinasia
% like op/3, but only allows new operators under 1200
safe_op(P, T, O, S) :- ((user_op(O); \+current_op(_,_,O)), P<1200) -> (
        current_op(P,T,O);
        op(P,T,O),
        (user_op(O); assert(user_op(O))),
        (S = saved; S=new,
        save_clause(:- safe_op(P, T, O, saved))
        )); throw(illegal_op(P,T,O)).

% safe_bagof, safe_setof, safe_aggregate
% cannot just add bagof(_,#,_) to allowed-predicates because of '^'/2
safe_bagof(A, P, B) :- prepare_bagof_goal(P, Psafe), bagof(A, Psafe, B).
safe_setof(A, P, B) :- prepare_bagof_goal(P, Psafe), setof(A, Psafe, B).
safe_aggregate(A, P, B) :- prepare_bagof_goal(P, Psafe), aggregate(A, Psafe, B).
safe_aggregate(A, B, P, C) :- prepare_bagof_goal(P, Psafe), aggregate(A, B, Psafe, C).

prepare_bagof_goal(A^B, A^C) :- !, prepare_bagof_goal(B, C).
prepare_bagof_goal(A, B) :- prepare(A, B), !.

% I almost wrote 30 lines of code before I discovered
% I could just do this to display the help on IRC :)
prolog:show_help_hook(_,File) :- read_file_to_codes(File,Codes,[]), format('~s',[Codes]).

% functor(+Clause, -SafeClause, -EmptyPredicate)
% SafeClause is the same Clause with a prepared body
% EmptyPredicate is the functor of the clause with new variables as arguments
prepare_clause((F :- B), (F :- C), R) :- !, \+reserved(F), functor(F,P,N), functor(R,P,N), prepare(B, C), !.
prepare_clause(F, F, R) :- \+reserved(F), functor(F,P,N), functor(R,P,N).

% reserved(-Predicate)
% these predicates are reserved and cannot be defined by the user
% includes dynamic predicates and built-in operators
:- dynamic reserved/1.
reserved(reserved).
reserved(prepare_allowed(_,_)).
reserved(more_lines(_)).
reserved(streams(_,_)).
reserved(more).
reserved(help).
reserved(server(_,_,_,_)).
reserved(user_op(_)).
reserved(_:_).

% add another reserved predicate
add_reserved_predicate(F/N) :- functor(P,F,N), add_reserved_predicate(P).
add_reserved_predicate(P) :-
	functor(P,N,A), functor(F,N,A), (reserved(F); assertz(reserved(F))).

ensure_not_reserved(A :- _) :- !, (reserved(A), functor(A,F,N), throw(reserved(F/N)) ; true).
ensure_not_reserved(A) :- (reserved(A), functor(A,F,N), throw(reserved(F/N)) ; true).

% write_list(+List, +Seperator)
% I wrote this and sendline because I'm not familiar with format/3 and write/2
write_list(L,S) :- current_output(O), write_list(O,L,S).

% write_list(+Out, +List, +Seperator)
write_list(_,[],_).
write_list(O,[X],_) :- write(O,X).
write_list(O,[X|XS],S) :- write(O,X), write(O,S), write_list(O,XS,S).

% show_list(+List, +Seperator)
% like write_list/2, but different
show_list([],_) :- !.
show_list([[C|Cs]|Xs],S) :- number(C), !, format('~s ',[[C|Cs]]), show_list(Xs,S).
show_list([X|Xs],S) :- !, X =.. [F|A], format('~a(',[F]), show_list(A,', '), format(')~a',[S]), show_list(Xs,S).

% not_implemented(+Feature)
not_implemented(X) :- throw(not_implemented(X)).

% save_clause(+Term)
% add Term to the end of the file
save_clause(C) :- stored_predicates_file(F), add_clause(F, C).

% add_clause(+File, +Term)
add_clause(F, C) :- open(F, append, S), portray_clause(S,C), close(S).

% many_calls_with_time_limit(+Seconds, +Goal)
% limit the time passed in Goal to Seconds and reset
% the timer when prolog comes back when backtracking
% this is a big hack that seems to work
many_calls_with_time_limit(Time, Goal) :- Time > 0, !,
	alarm(Time, throw(time_limit_exceeded), Id1),
        Alarm = alarm(Id1), !,
        onbt(my_remove_alarm(Alarm)),
	call_cleanup(Goal, my_remove_alarm(Alarm)),
        my_remove_alarm(Alarm),
        onbt((alarm(Time, throw(time_limit_exceeded), Id2), nb_setarg(1, Alarm, Id2))).
many_calls_with_time_limit(_, _) :- throw(time_limit_exceeded).

% my_remove_alarm(+AlarmId)
my_remove_alarm(alarm(none)) :- !.
my_remove_alarm(Alarm) :- Alarm = alarm(Id), time:remove_alarm(Id), nb_setarg(1, Alarm, none).

% exited(+Goal, -TrueOrFalse)
% looks for choice points
exited(G, E) :- G, deterministic(E).
