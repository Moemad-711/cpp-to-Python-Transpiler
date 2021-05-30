% Auhtors: 
%   1- Mohammed Emadeldin Mahmoud Radwan Mohammed El-Seoudi (Sec: 3)
%   2- Mariam Mohammed Ibrahim (Sec: 4)
%   3- Mohammed Aly Ahmed Aly (Sec: 3)
%   4- Mahmoud Mamdouh Gaber (Sec: 4)
%   5- Nada Magdy Zaki (Sec: 4)


% Read input Tokenizing 
parse_code_from_file:-
	%%%%%%%%%%%%%%% Edit Source file Path %%%%%%%%%%%%%%%
 	read_file_to_string('.\\test.txt',InputString,[]),
 	string_tokenizer(InputString,TokenizedList),
	maplist(atom_string, Tree, TokenizedList),
	%%%%%%%%%%%%%%% Edit Output File Path %%%%%%%%%%%%%%%
	parse_and_write('.\\output.txt',Tree).

string_tokenizer(Input,Output):-
	normalize_space(atom(S0), Input), % remove spaces in start and end of string %
	change_character_in_string("+"," + ",S0,S1),
	change_character_in_string("-"," - ",S1,S2),
	change_character_in_string("*"," * ",S2,S3),
	change_character_in_string("/"," / ",S3,S4),
	change_character_in_string("{"," { ",S4,S5),
	change_character_in_string("}"," } ",S5,S6),
	change_character_in_string("("," ( ",S6,S7),
	change_character_in_string(")"," ) ",S7,S8),
	change_character_in_string(";"," ; ",S8,S9),
	change_character_in_string("|"," | ",S9,S10),
	change_character_in_string("&"," & ",S10,S11),
	change_character_in_string("="," = ",S11,S12),
	change_character_in_string("<"," < ",S12,S13),
	change_character_in_string(">"," > ",S13,S14),
    change_character_in_string("!"," ! ",S14,S15),
	normalize_space(atom(SF), S15), %make_all_space_one space only Normalize white space in In. All leading and trailing white space is removed. All non-empty sequences for Unicode white space characters are replaced by a single space (\u0020) character%
	split_string(SF, " ", "",Output).

change_character_in_string(Old_Char,New_Char,String,New_String):-
	atomic_list_concat(Atoms,Old_Char, String),
	atomic_list_concat(Atoms,New_Char, New_String).

%%%%%%%%%%%%%%%%% Parse and Write %%%%%%%%%%%%%%%%%%%%%%%
parse_and_write(OutputFile,InputTree):- 
	phrase(parser(Tree),InputTree),
	open(OutputFile,write,OutputStream),
	write(Tree),
	write_new_lang(OutputStream, Tree),
	close(OutputStream).	%write_to_file(Tree).


parse_and_write(OutputFile):- %%%%% for testing Purposes 
	phrase(parser(Tree), [while,'(',x,<,20,')','{',x,=,x,+,1,;,'}']),
	%phrase(parser(Tree),[x,=,x,+,1,;]),
	%write(Tree).
	open(OutputFile,write,OutputStream),
	write_new_lang(OutputStream, Tree),
	close(OutputStream).

%%% Write Statment %%%
write_new_lang(Stream, statment(Tree)):-
	write_new_lang(Stream, Tree).

%%% Write Statments %%%
write_new_lang(Stream, statments(Stmt, Stmts)):-
	%rite('hi stmts'),
	write(Stream, '    '),
	write_new_lang(Stream, Stmt),
	write_new_lang(Stream, Stmts).

write_new_lang(Stream, statments([])):-
	write(Stream,'').

%%% Write Assign_Stmt %%%
write_new_lang(Stream, assign_stmt(ID, Equal, Expr)):-
	write(Stream, ID),
	write(Stream, Equal),
	write_new_lang(Stream, Expr),
	nl(Stream).

%%% Write While_Stmt %%%
write_new_lang(Stream, while(Conds, Stmt)):-
	write(Stream, 'while '),
	write_new_lang(Stream, Conds),
	write(Stream, ':'),
	nl(Stream),
	write_new_lang(Stream, Stmt).

%%% Write Do_While_Stmt %%%
write_new_lang(Stream, do_while(Stmt, Conds)):-
	write(Stream, 'while  True :\n'),
	write_new_lang(Stream, Stmt),
	write(Stream, '    if '),
	write_new_lang(Stream, Conds),
	write(Stream, ':\n'),
	write( Stream,'        continue\n'),
	write(Stream, '    else :\n'),
	write( Stream,'        break\n').
	
%%% Write if_Stmt %%%
write_new_lang(Stream, if(Conds, Stmt)):-
	write(Stream, 'if '),
	write_new_lang(Stream, Conds),
	write(Stream, ':\n'),
	write_new_lang(Stream, Stmt).

write_new_lang(Stream, if(Conds, Stmt1, Else, Stmt2)):-
	write(Stream, 'if '),
	write_new_lang(Stream, Conds),
	write(Stream, ':\n'),
	write_new_lang(Stream, Stmt1),
	(memberchk(Else, ['else']), write(Stream, 'else :\n'), write_new_lang(Stream,Stmt2)).

%%% Write Conditions %%%
write_new_lang(Stream, conditions(Cond1, Op1, _, Cond2)):-
	write_new_lang(Stream, Cond1),
	( memberchk(Op1, ['&']), write(Stream, 'and ') | memberchk(Op1, ['|']), write(Stream, 'or ')),
	write_new_lang(Stream, Cond2).

write_new_lang(Stream, conditions(Cond1)):-
	write_new_lang(Stream, Cond1).

%%% Write Condition %%%
write_new_lang(Stream, condition(Expr1, Op1, Op2, Expr2)):-
	write_new_lang(Stream, Expr1),
	write(Stream, Op1),
	write(Stream, Op2),
	write_new_lang(Stream, Expr2),
	write(Stream, ' ').

write_new_lang(Stream, condition(Expr1, Op1, Expr2)):-
	write_new_lang(Stream, Expr1),
	write(Stream, Op1),
	write_new_lang(Stream, Expr2),
	write(Stream, ' ').


%%% Write Expression %%%
write_new_lang(Stream, expression(Term, Op, Expr)):-
	write_new_lang(Stream, Term),
	write(Stream, Op),
	write_new_lang(Stream, Expr).

write_new_lang(Stream, expression(Bracket1, Term, Bracket2)):-
	write(Stream, Bracket1),
	write_new_lang(Stream, Term),
	write(Stream, Bracket2).

write_new_lang(Stream, expression(Term)):-
	write_new_lang(Stream, Term).

%%% Write Term %%%
write_new_lang(Stream, term(Factor, Op, Term)):-
	write_new_lang(Stream, Factor),
	write(Stream, Op),
	write_new_lang(Stream, Term).

write_new_lang(Stream, term(Factor)):-
	write_new_lang(Stream, Factor).

%%% Write Factor %%%
write_new_lang(Stream, factor(Num)):-
	write(Stream, Num).

write_new_lang(Stream, factor(Bracket1,Expr,Bracket2)):-
	write(Stream, Bracket1),
	write_new_lang(Stream, Expr),
	write(Stream, Bracket2).



%%%%%%%%%%%%%% Tools %%%%%%%%%%%%%%%%
writelist(H|L):-
	write(H), writelist(L).

flatten([], []).
flatten([H|T], R) :-
    (
       flatten(H, FH)
    -> append(FH, FT, R)
    ;  R = [H|FT]
    ),
    flatten(T, FT).


%%%%%%%%%%%%%%%%%%% CFG Parsing Rules %%%%%%%%%%%%%%%%%%%%%%%
parser([]) --> [].
parser(Tree) --> stmt(Tree).

%%%%%%%%%% Statment Rules %%%%%%%%%%%%
stmt(statment(Tree)) --> while_stmt(Tree).
stmt(statment(Tree)) --> assign_stmt(Tree).
stmt(statment(Tree)) --> do_while_stmt(Tree).
stmt(statment(Tree)) --> if_stmt(Tree).
stmt(statment(Tree)) --> ['{'], stmts(Tree), ['}'].

%%%%%%%%%% Statments Rules %%%%%%%%%%%%
stmts(statments([])) --> [].
stmts(statments(Stmt, Stmts)) --> stmt(Stmt), stmts(Stmts).

%%%%%%%%%% If Rules %%%%%%%%%%%%
if_stmt(if(Conds, Stmt)) --> ['if'], ['('], conditions(Conds), [')'], stmt(Stmt).
if_stmt(if(Conds, Stmt1, 'else', Stmt2)) --> ['if'], ['('], conditions(Conds), [')'], stmt(Stmt1), ['else'], stmt(Stmt2).

%%%%%%%%%% Do While Rules %%%%%%%%%%%%
do_while_stmt(do_while(Stmt, Conds)) --> ['do'], stmt(Stmt), ['while'], ['('], conditions(Conds), [')'], [';'].

%%%%%%%%%% While Rules %%%%%%%%%%%%
while_stmt(while(Conds, Stmt)) --> ['while'], ['('], conditions(Conds), [')'], stmt(Stmt).

%%%%%%%%%% Assignment Rules %%%%%%%%%%%%
assign_stmt(assign_stmt(ID, '=', Expr)) --> id(ID), ['='], expression(Expr), [';'].

%%%%%%%%%% Condition Rules %%%%%%%%%%%%
conditions(conditions(Cond, Op1, Op2, Conds)) --> condition(Cond), log_op(Op1), log_op(Op2), conditions(Conds).
conditions(conditions(Cond)) --> condition(Cond).

%%%%%%%%%% Condition Rules %%%%%%%%%%%%
condition(condition(Expr1, Op1, Expr2)) --> expression(Expr1), rel_op1(Op1), expression(Expr2).
condition(condition(Expr1, Op1,Op2, Expr2)) --> expression(Expr1), rel_op1(Op1), rel_op2(Op2), expression(Expr2).

%%%%%%%%%% Expression Rules %%%%%%%%%%%%
expression(expression(Term, Op, Expr)) --> term(Term), add_sub_op(Op), expression(Expr).
expression(expression('(', Term, ')')) --> ['('], term(Term), [')'].
expression(expression(Term)) --> term(Term).

%%%%%%%%%% Term Rules %%%%%%%%%%%%
term(term(Factor, Op, Term)) --> factor(Factor), mul_div_op(Op), term(Term).
term(term(Factor)) --> factor(Factor).

%%%%%%%%%% Factor Rules %%%%%%%%%%%%
factor(factor(Num)) --> num(Num).
factor(factor(ID)) --> id(ID).
factor(factor('(', Expr, ')')) --> ['('], expression(Expr), [')'].

%%%%%%%%%% Terminals Rules %%%%%%%%%%%%
num(Num) --> [Num], {number(Num)}.
id(ID) --> [ID], {atom(ID)}.

rel_op1(Op) --> [Op] , { memberchk(Op, ['>', '<', '!', '='])}.
rel_op2(Op) --> [Op] , { memberchk(Op, ['='])}.

log_op(Op) --> [Op] , { memberchk(Op, ['|', '&']) }.

add_sub_op(Op) --> [Op], {memberchk(Op, ['+', '-'])}.
mul_div_op(Op) --> [Op], {memberchk(Op, ['*', '/'])}. %memberchk on varible on list of elements if match one of them return true or false %