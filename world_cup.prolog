% results
result( 1, 4, 0). % fra v kor
result( 4, 3, 1). % esp v rsa
result( 2, 3, 0). % nor v nga
result( 3, 1, 0). % ger v chn
result( 5, 1, 2). % aus v ita
result( 6, 3, 0). % bra v jam
result( 7, 2, 1). % eng v sco
result( 8, 0, 0). % arg v jpn
result( 9, 1, 0). % can v cmr
result(10, 0, 1). % nzl v ned
result(11, 13, 0). % usa v tha
result(12, 0, 2). % chi v swe
result(15, 1, 0). % ger v esp
result(14, 2, 0). % nga v kor
result(13, 2, 1). % fra v nor
%result(16, 0, 0). % rsa v chn
result(17, 3, 2). % aus v bra
%result(19, 0, 0). % eng v arg
%result(18, 0, 0). % jam v ita
%result(20, 0, 0). % jpn v sco
%result(22, 0, 0). % ned v cmr
%result(21, 0, 0). % can v nzl
%result(23, 0, 0). % usa v chi
%result(24, 0, 0). % swe v tha
%result(28, 0, 0). % chn v esp
%result(26, 0, 0). % kor v nor
%result(25, 0, 0). % nga v fra
%result(27, 0, 0). % rsa v ger
%result(30, 0, 0). % ita v bra
%result(29, 0, 0). % jam v aus
%result(32, 0, 0). % sco v arg
%result(31, 0, 0). % jpn v eng
%result(35, 0, 0). % swe v usa
%result(33, 0, 0). % ned v can
%result(36, 0, 0). % tha v chi
%result(34, 0, 0). % cmr v nzl

% round of 16
%result(38, 0, 0).
%result(37, 0, 0).
%result(39, 0, 0).
%result(40, 0, 0).
%result(41, 0, 0).
%result(42, 0, 0).
%result(44, 0, 0).
%result(43, 0, 0).

% quarter finals
%result(45, 0, 0).
%result(46, 0, 0).
%result(47, 0, 0).
%result(48, 0, 0).

%semi finals
%result(49, 0, 0).
%result(50, 0, 0).

%finals
%result(51, 0, 0).
%result(52, 0, 0).

result(_,_,_)     :- false.
result(_,_,_,_,_) :- false.

% timezones for french locations
timezone(valenciennes, 2).
timezone(lehavre, 2).
timezone(reims, 2).
timezone(paris, 2).
timezone(rennes, 2).
timezone(lyon, 2).
timezone(grenoble, 2).
timezone(nice, 2).
timezone(montpellier, 2).
timezone(minneapolis, -5).

% month_to_ord (for comparison)
month_to_ord(june, 6).
month_to_ord(july, 7).

day_of_week_from_date(june,  7, friday).
day_of_week_from_date(june,  8, saturday).
day_of_week_from_date(june,  9, sunday).
day_of_week_from_date(june, 10, monday).
day_of_week_from_date(june, 11, tuesday).
day_of_week_from_date(june, 12, wednesday).
day_of_week_from_date(june, 13, thursday).
day_of_week_from_date(june, 14, friday).
day_of_week_from_date(june, 15, saturday).
day_of_week_from_date(june, 16, sunday).
day_of_week_from_date(june, 17, monday).
day_of_week_from_date(june, 18, tuesday).
day_of_week_from_date(june, 19, wednesday).
day_of_week_from_date(june, 20, thursday).
day_of_week_from_date(june, 21, friday).
day_of_week_from_date(june, 22, saturday).
day_of_week_from_date(june, 23, sunday).
day_of_week_from_date(june, 24, monday).
day_of_week_from_date(june, 25, tuesday).
day_of_week_from_date(june, 26, wednesday).
day_of_week_from_date(june, 27, thursday).
day_of_week_from_date(june, 28, friday).
day_of_week_from_date(june, 29, saturday).
day_of_week_from_date(june, 30, sunday).
day_of_week_from_date(july,  1, monday).
day_of_week_from_date(july,  2, tuesday).
day_of_week_from_date(july,  3, wednesday).
day_of_week_from_date(july,  4, thursday).
day_of_week_from_date(july,  5, friday).
day_of_week_from_date(july,  6, saturday).
day_of_week_from_date(july,  7, sunday).

% groups
group(a, [ fra, nor, nga, kor ]).
group(b, [ esp, ger, chn, rsa ]).
group(c, [ bra, ita, aus, jam ]).
group(d, [ eng, arg, jpn, sco ]).
group(e, [ can, ned, nzl, cmr ]).
group(f, [ usa, tha, chi, swe ]).

% matches ( group only)
match( 1, fra, kor, june,  7, 2100, paris).
match( 4, esp, rsa, june,  8, 1800, lehavre).
match( 2, nor, nga, june,  8, 2100, reims).
match( 3, ger, chn, june,  8, 1500, rennes).
match( 5, aus, ita, june,  9, 1300, valenciennes).
match( 6, bra, jam, june,  9, 1530, grenoble).
match( 7, eng, sco, june,  9, 1800, nice).
match( 8, arg, jpn, june, 10, 1800, paris).
match( 9, can, cmr, june, 10, 2100, montpellier).
match(10, nzl, ned, june, 11, 1500, lehavre).
match(11, usa, tha, june, 11, 2100, reims).
match(12, chi, swe, june, 11, 1800, rennes).
match(15, ger, esp, june, 12, 1800, valenciennes).
match(14, nga, kor, june, 12, 1500, grenoble).
match(13, fra, nor, june, 12, 2100, nice).
match(16, rsa, chn, june, 13, 2100, paris).
match(17, aus, bra, june, 13, 1800, montpellier).
match(19, eng, arg, june, 14, 2100, lehavre).
match(18, jam, ita, june, 14, 1800, reims).
match(20, jpn, sco, june, 14, 1500, rennes).
match(22, ned, cmr, june, 15, 1500, valenciennes).
match(21, can, nzl, june, 15, 2100, grenoble).
match(23, usa, cli, june, 16, 1800, paris).
match(24, swe, tha, june, 16, 1500, nice).
match(28, chn, esp, june, 17, 1800, lehavre).
match(26, kor, nor, june, 17, 2100, reims).
match(25, nga, fra, june, 17, 2100, rennes).
match(27, rsa, ger, june, 17, 1800, montpellier).
match(30, ita, bra, june, 18, 2100, valenciennes).
match(29, jam, aus, june, 18, 2100, grenoble).
match(32, sco, arg, june, 19, 2100, paris).
match(31, jpn, eng, june, 19, 2100, nice).
match(35, swe, usa, june, 20, 2100, lehavre).
match(33, ned, can, june, 20, 1800, reims).
match(36, tha, chi, june, 20, 2100, rennes).
match(34, cmr, nzl, june, 20, 1800, montpellier).

% RO16 matches
match(38, T1, T2, june, 22, 1730, grenoble) :-
    group_rank(1,b,T1), third_placed_qualifier(acd, T2).
match(37, T1, T2, june, 22, 2100, nice) :-
    group_rank(2,a,T1), group_rank(2,c,T2).
match(39, T1, T2, june, 23, 1730, valenciennes) :-
    group_rank(1,d,T1), third_placed_qualifier(bef, T2).
match(40, T1, T2, june, 23, 2100, lehavre) :-
    group_rank(1,a,T1), third_placed_qualifier(cde, T2).
match(41, T1, T2, june, 24, 1800, reims) :-
    group_rank(2,b,T1), group_rank(1,f,T2).
match(42, T1, T2, june, 24, 2100, paris) :-
    group_rank(2,f,T1), group_rank(2,e,T2).
match(44, T1, T2, june, 25, 2100, rennes) :-
    group_rank(1,e,T1), group_rank(2,d,T2).
match(43, T1, T2, june, 25, 1800, montpellier) :-
    group_rank(1,f,T1), third_placed_qualifier(abf, T2).

% quarter finals
match(45, T1, T2, june, 27, 2100, lehavre) :-
    winner(37,T1), winner(39,T2).
match(46, T1, T2, june, 28, 2100, paris) :-
    winner(40,T1), winner(41,T2).
match(47, T1, T2, june, 29, 1500, valenciennes) :-
    winner(43,T1), winner(44,T2).
match(48, T1, T2, june, 29, 1830, rennes) :-
    winner(38,T1), winner(42,T2).

%semi finals
match(49, T1, T2, july,  2, 2100, lyon) :-
    winner(45,T1), winner(46,T2).
match(50, T1, T2, july,  3, 2100, lyon) :-
    winner(47,T1), winner(48,T2).

%finals
match(51, T1, T2, july,  6, 1700, nice) :-
    loser(49,T1), loser(50,T2).
match(52, T1, T2, july,  7, 1700, lyon) :-
    winner(49,T1), winner(50,T2).


% note this is a different kind of group predicate
group_rank(R,G,T) :- group_stats(G,L), nth1(R,L,[T|_]).

winner(G, T) :- match(G,T,_,_,_,_,_), result(G, Gf, Ga), Gf > Ga.
winner(G, T) :- match(G,_,T,_,_,_,_), result(G, Ga, Gf), Gf > Ga.
winner(G, T) :- match(G,T,_,_,_,_,_), result(G, _G, _G, Pf, Pa), Pf > Pa.
winner(G, T) :- match(G,_,T,_,_,_,_), result(G, _G, _G, Pa, Pf), Pf > Pa.
loser(G, T) :- match(G,T,_,_,_,_,_), result(G, Gf, Ga), Gf < Ga.
loser(G, T) :- match(G,_,T,_,_,_,_), result(G, Ga, Gf), Gf < Ga.
loser(G, T) :- match(G,T,_,_,_,_,_), result(G, _G, _G, Pf, Pa), Pf < Pa.
loser(G, T) :- match(G,_,T,_,_,_,_), result(G, _G, _G, Pa, Pf), Pf < Pa.

group_match(M) :- M =< 34.

mdt(LocalTime, UTCOffset, Result) :-
    Result is LocalTime - (UTCOffset+5)*100.

date_between(Ma,_,Mb,_,Mc,_) :- Ma < Mb, Mb < Mc, !.
date_between(M,Da,M,Db,Mc,_) :- M < Mc, Da =< Db, !.
date_between(Ma,_,M,Db,M,Dc) :- Ma < M, Db =< Dc, !.
date_between(M,Da,M,Db,M,Dc) :- Da =< Db, Db =< Dc, !.
date_between(M,D,M,D,M,D).

schedule(Ms, Ds, Me, De, [N,T1,T2,DoW,M,D,Tm]) :-
    match(N, T1, T2, M, D, Tl, L),
    day_of_week_from_date(M,D,DoW),
    month_to_ord(Ms, NMs),
    month_to_ord(Me, NMe),
    month_to_ord(M, NM),
    date_between(NMs,Ds,NM,D,NMe,De),
    timezone(L, U),
    mdt(Tl,U,Tm).

team_schedule(T, [N,T1,T2,DoW,M,D,Ti]) :-
    member(T, [T1,T2]),
    match(N, T1, T2, M, D, Tm, L),
    day_of_week_from_date(M,D,DoW),
    timezone(L, U),
    mdt(Tm,U,Ti).

game_stat([T1,G1,T2,G2], [T1|S1], [T2|S2]) :-
    G1 < G2,
    G1d is G1-G2, G2d is G2-G1,
    S1 = [1,0,0,1,G1,G2,G1d,0],
    S2 = [1,1,0,0,G2,G1,G2d,3].

game_stat([T1,G1,T2,G2], [T1|S1], [T2|S2]) :-
    G1 > G2,
    G1d is G1-G2, G2d is G2-G1,
    S1 = [1,1,0,0,G1,G2,G1d,3],
    S2 = [1,0,0,1,G2,G1,G2d,0].

game_stat([T1,G,T2,G], [T1|S], [T2|S]) :-
    S = [1,0,1,0,G,G,0,1].

list_head_in(Ts, [T|_]) :- member(T,Ts).

merge_group_stats([], A, A).
merge_group_stats([Rh|Rt], A, S) :-
    game_stat(Rh,[T1|S1],[T2|S2]),
    member([T1|S1o],A),
    member([T2|S2o],A),
    vector_add(S1,S1o,S1n),
    vector_add(S2,S2o,S2n),
    exclude(list_head_in([T1,T2]), A, An),
    merge_group_stats(Rt,[[T1|S1n]|[[T2|S2n]|An]],S).

group_stat_init(T, [T,0,0,0,0,0,0,0,0]).

group_stats(G,S) :-
    group(G, Glist),
    findall(Rn, (
        result(N,G1,G2),
        group_match(N),
        match(N,T1,T2,_,_,_,_),
        subset([T1,T2],Glist),
        Rn=[T1,G1,T2,G2]),
    Rs),
    maplist(group_stat_init, Glist, A),
    merge_group_stats(Rs, A, Sn),
    predsort(stat_cmp, Sn, S), !.

format_group_stat(S) :-
    format("~t~a~6|~t~d~6+~t~d~4+~t~d~4+~t~d~4+~t~d~4+~t~d~4+~t~d~4+~t~d~4+~n", S).

write_group_stats(G) :-
    format("~t~s~6|~t~s~6+~t~s~4+~t~s~4+~t~s~4+~t~s~4+~t~s~4+~t~s~4+~t~s~4+~n",
           ["Team", "Pld", "W", "D", "L", "GF", "GA", "GD", "Pts"]),
    group_stats(G, S),
    maplist(format_group_stat, S).

from_military_hour(0, 12, am).
from_military_hour(12, 12, pm).
from_military_hour(H, R, pm) :- H > 12, R is H-12.
from_military_hour(H, H, am) :- H < 12, H > 0.
from_military_time(T, S) :-
    H is T//100,
    M is T-H*100,
    from_military_hour(H, Hnew, O),
    format(atom(S), "~t~d~2+:~|~`0t~d~2+ ~a", [Hnew,M,O]).

% [11, ger, mex, sunday, june, 17, 1000, f]
write_schedule([N,T1,T2,DoW,M,D,T]) :-
    from_military_time(T,Ti),
    format("~t~d~2+ ~a v ~a ~|~a~t~10+~a ~a ~a~n",
        [N,T1,T2,DoW,M,D,Ti]).

write_schedules(Ma,Da,Mb,Db) :-
    findall(R, schedule(Ma,Da,Mb,Db,R), Rs),
    predsort(sched_cmp, Rs, ToWrite),
    maplist(write_schedule, ToWrite), !.
write_schedules(M, D) :- write_schedules(M, D, M, D).

% signs are such that leader is first in list
stat_cmp('>',[_,_,_,_,_,_,_,_,P1],[_,_,_,_,_,_,_,_,P2]) :- P1<P2.
stat_cmp('<',[_,_,_,_,_,_,_,_,P1],[_,_,_,_,_,_,_,_,P2]) :- P1>P2.
stat_cmp('>',[_,_,_,_,_,_,_,GD1,P],[_,_,_,_,_,_,_,GD2,P]) :- GD1<GD2.
stat_cmp('<',[_,_,_,_,_,_,_,GD1,P],[_,_,_,_,_,_,_,GD2,P]) :- GD1>GD2.
stat_cmp('>',[_,_,_,_,_,Gf1,_,GD,P],[_,_,_,_,_,Gf2,_,GD,P]) :- Gf1<Gf2.
stat_cmp('<',[_,_,_,_,_,Gf1,_,GD,P],[_,_,_,_,_,Gf2,_,GD,P]) :- Gf1>Gf2.
stat_cmp('<',[_,_,_,_,_,Gf,_,GD,P],[_,_,_,_,_,Gf,_,GD,P]).

sched_cmp('<',[G1,_,_,_,M,D,T],[G2,_,_,_,M,D,T]) :- G1<G2.
sched_cmp('>',[G1,_,_,_,M,D,T],[G2,_,_,_,M,D,T]) :- G1>G2.
sched_cmp('<',[_,_,_,_,M,D,Ta],[_,_,_,_,M,D,Tb]) :- Ta<Tb.
sched_cmp('>',[_,_,_,_,M,D,Ta],[_,_,_,_,M,D,Tb]) :- Ta>Tb.
sched_cmp('<',[_,_,_,_,M,Da,_],[_,_,_,_,M,Db,_]) :- Da<Db.
sched_cmp('>',[_,_,_,_,M,Da,_],[_,_,_,_,M,Db,_]) :- Da>Db.
sched_cmp('<',[_,_,_,_,Ma,_,_],[_,_,_,_,Mb,_,_]) :-
    month_to_ord(Ma,M1), month_to_ord(Mb,M2), M1<M2.
sched_cmp('>',[_,_,_,_,Ma,_,_],[_,_,_,_,Mb,_,_]) :-
    month_to_ord(Ma,M1), month_to_ord(Mb,M2), M1>M2.

vector_add([],[],[]).
vector_add([H1|T1],[H2|T2],[HR|TR]) :-
    same_length(T1,T2), same_length(T2,TR),
    HR is H1 + H2,
    vector_add(T1,T2,TR).

group_of(T, G) :- group(G, Ts), member(T, Ts), !.

third_placed_qualifier(Gi, T) :- findall(G, group(G, _), Gs),
                                maplist(group_stats, Gs, Ss),
                                maplist(nth1(3), Ss, S),
                                predsort(stat_cmp, S, R),
                                append(F, _, R), length(F, 4),
                                maplist(nth1(1), F, Ts),
                                maplist(group_of, Ts, G4),
                                sort(G4, G4s),
                                third_placed_qualifier(G4s, Gi, T), !.

third_placed_qualifier(Gs, Gq, T) :- third_placed_qualifier_data(Gs, Gqs),
                                     member(Gq, Gqs),
                                     nth1(N, Gqs, Gq),
                                     nth1(N, Gs, G),
                                     group_rank(3, G, T), !.

third_placed_qualifier_data([a,b,c,d], [abf, bef, cde, acd]).
third_placed_qualifier_data([a,b,c,e], [acd, abf, cde, bef]).
third_placed_qualifier_data([a,b,c,f], [acd, abf, cde, bef]).
third_placed_qualifier_data([a,b,d,e], [acd, abf, cde, bef]).
third_placed_qualifier_data([a,b,d,f], [acd, abf, cde, bef]).
third_placed_qualifier_data([a,b,e,f], [acd, abf, cde, bef]).
third_placed_qualifier_data([a,c,d,e], [abf, cde, acd, bef]).
third_placed_qualifier_data([a,c,d,f], [abf, cde, acd, bef]).
third_placed_qualifier_data([a,c,e,f], [acd, cde, bef, abf]).
third_placed_qualifier_data([a,d,e,f], [acd, cde, bef, abf]).
third_placed_qualifier_data([b,c,d,e], [abf, cde, acd, bef]).
third_placed_qualifier_data([b,c,d,f], [abf, cde, acd, bef]).
third_placed_qualifier_data([b,c,e,f], [abf, acd, cde, bef]).
third_placed_qualifier_data([b,d,e,f], [abf, acd, cde, bef]).
third_placed_qualifier_data([c,d,e,f], [cde, acd, bef, abf]).

% vim: filetype=prolog
