% results
result( 1, 5, 0). % rus v ksa
result( 2, 0, 1). % egy v uru
result( 4, 0, 1). % mar v irn
result( 3, 3, 3). % por v esp
result( 5, 2, 1). % fra v aus
result( 7, 1, 1). % arg v isl
result( 6, 0, 1). % per v den
result( 8, 2, 0). % cro v nga
result(10, 0, 1). % crc v srb
result(11, 0, 1). % ger v mex
result( 9, 1, 1). % bra v sui
result(12, 1, 0). % swe v kor
result(13, 3, 0). % bel v pan
result(14, 1, 2). % tun v eng
result(15, 1, 2). % pol v sen
result(17, 3, 1). % rus v egy
result(16, 1, 2). % col v jpn
result(20, 0, 1). % irn v esp
result(19, 1, 0). % por v mar
result(18, 1, 0). % uru v ksa
result(21, 1, 0). % fra v per
result(23, 0, 3). % arg v cro
result(22, 1, 1). % den v aus
result(26, 1, 2). % srb v sui
result(25, 2, 0). % bra v crc
result(24, 2, 0). % nga v isl
result(29, 5, 2). % bel v tun
result(28, 1, 2). % kor v mex
result(27, 2, 1). % ger v swe
%result(32, 0, 0). % jpn v sen
%result(31, 0, 0). % pol v col
%result(30, 0, 0). % eng v pan
%result(36, 0, 0). % esp v mar
%result(33, 0, 0). % uru v rus
%result(35, 0, 0). % irn v por
%result(34, 0, 0). % ksa v egy
%result(37, 0, 0). % den v fra
%result(40, 0, 0). % isl v cro
%result(39, 0, 0). % nga v arg
%result(38, 0, 0). % aus v per
%result(44, 0, 0). % mex v swe
%result(43, 0, 0). % kor v ger
%result(41, 0, 0). % srb v bra
%result(42, 0, 0). % sui v crc
%result(44, 0, 0). % eng v bel
%result(48, 0, 0). % sen v col
%result(46, 0, 0). % pan v tun
%result(47, 0, 0). % jpn v pol

% timezones for russian locations
timezone(ekaterinburg,5).
timezone(kaliningrad,2).
timezone(kazan,3).
timezone(moscow,3).
timezone(novgorod,3).
timezone(rostov,3).
timezone(stpetersburg,3).
timezone(samara,4).
timezone(saransk,3).
timezone(sochi,3).
timezone(volgograd,3).
timezone(minneapolis, -5).

% month_to_ord (for comparison)
month_to_ord(june, 6).
month_to_ord(july, 7).

day_of_week_from_date(june, 14, thursday).
day_of_week_from_date(june, 15, friday).
day_of_week_from_date(june, 16, saturday).
day_of_week_from_date(june, 17, sunday).
day_of_week_from_date(june, 18, monday).
day_of_week_from_date(june, 19, tuesday).
day_of_week_from_date(june, 20, wednesday).
day_of_week_from_date(june, 21, thursday).
day_of_week_from_date(june, 22, friday).
day_of_week_from_date(june, 23, saturday).
day_of_week_from_date(june, 24, sunday).
day_of_week_from_date(june, 25, monday).
day_of_week_from_date(june, 26, tuesday).
day_of_week_from_date(june, 27, wednesday).
day_of_week_from_date(june, 28, thursday).
day_of_week_from_date(june, 30, saturday).
day_of_week_from_date(july,  1, sunday).
day_of_week_from_date(july,  2, monday).
day_of_week_from_date(july,  3, tuesday).
day_of_week_from_date(july,  6, friday).
day_of_week_from_date(july,  7, saturday).
day_of_week_from_date(july, 10, tuesday).
day_of_week_from_date(july, 11, wednesday).
day_of_week_from_date(july, 14, saturday).
day_of_week_from_date(july, 15, sunday).

% groups
group(a, [ rus, ksa, egy, uru ]).
group(b, [ por, esp, mar, irn ]).
group(c, [ fra, aus, per, den ]).
group(d, [ arg, isl, cro, nga ]).
group(e, [ bra, sui, crc, srb ]).
group(f, [ ger, mex, swe, kor ]).
group(g, [ bel, pan, tun, eng ]).
group(h, [ pol, sen, col, jpn ]).

% matches ( group only)
match( 1, rus, ksa, june, 14, 1800, moscow).
match( 2, egy, uru, june, 15, 1700, ekaterinburg).
match( 4, mar, irn, june, 15, 1800, stpetersburg).
match( 3, por, esp, june, 15, 2100, sochi).
match( 8, cro, nga, june, 16, 2100, kaliningrad).
match( 5, fra, aus, june, 16, 1300, kazan).
match( 7, arg, isl, june, 16, 1600, moscow).
match( 6, per, den, june, 16, 1900, saransk).
match(11, ger, mex, june, 17, 1800, moscow).
match( 9, bra, sui, june, 17, 2100, rostov).
match(10, crc, srb, june, 17, 1600, samara).
match(12, swe, kor, june, 18, 1500, novgorod).
match(13, bel, pan, june, 18, 1800, sochi).
match(14, tun, eng, june, 18, 2100, volgograd).
match(15, pol, sen, june, 19, 1800, moscow).
match(17, rus, egy, june, 19, 2100, stpetersburg).
match(16, col, jpn, june, 19, 1500, saransk).
match(20, irn, esp, june, 20, 2100, kazan).
match(19, por, mar, june, 20, 1500, moscow).
match(18, uru, ksa, june, 20, 1800, rostov).
match(21, fra, per, june, 21, 2000, ekaterinburg).
match(23, arg, cro, june, 21, 2100, novgorod).
match(22, den, aus, june, 21, 1600, samara).
match(26, srb, sui, june, 22, 2000, kaliningrad).
match(25, bra, crc, june, 22, 1500, stpetersburg).
match(24, nga, isl, june, 22, 1800, volgograd).
match(29, bel, tun, june, 23, 1500, moscow).
match(28, kor, mex, june, 23, 1800, rostov).
match(27, ger, swe, june, 23, 2100, sochi).
match(32, jpn, sen, june, 24, 2000, ekaterinburg).
match(31, pol, col, june, 24, 2100, kazan).
match(30, eng, pan, june, 24, 1500, novgorod).
match(36, esp, mar, june, 25, 2000, kaliningrad).
match(33, uru, rus, june, 25, 1800, samara).
match(35, irn, por, june, 25, 2100, saransk).
match(34, ksa, egy, june, 25, 1700, sochi).
match(37, den, fra, june, 26, 1700, moscow).
match(40, isl, cro, june, 26, 2100, rostov).
match(39, nga, arg, june, 26, 2100, stpetersburg).
match(38, aus, per, june, 26, 1700, sochi).
match(44, mex, swe, june, 27, 1900, ekaterinburg).
match(43, kor, ger, june, 27, 1700, kazan).
match(41, srb, bra, june, 27, 2100, moscow).
match(42, sui, crc, june, 27, 2100, novgorod).
match(45, eng, bel, june, 28, 2000, kaliningrad).
match(48, sen, col, june, 28, 1800, samara).
match(46, pan, tun, june, 28, 2100, saransk).
match(47, jpn, pol, june, 28, 1700, volgograd).
    
% RO16 matches
match(50, T1, T2, june, 30, 1700, kazan) :-
    group_rank(1,c,T1), group_rank(2,d,T2).
match(49, T1, T2, june, 30, 2100, sochi) :-
    group_rank(1,a,T1), group_rank(2,b,T2).
match(51, T1, T2, july,  1, 1700, moscow) :-
    group_rank(1,b,T1), group_rank(2,a,T2).
match(52, T1, T2, july,  1, 2100, novgorod) :-
    group_rank(1,d,T1), group_rank(2,c,T2).
match(54, T1, T2, july,  2, 2100, rostov) :-
    group_rank(1,g,T1), group_rank(2,h,T2).
match(53, T1, T2, july,  2, 1800, samara) :-
    group_rank(1,e,T1), group_rank(2,f,T2).
match(56, T1, T2, july,  3, 2100, moscow) :-
    group_rank(1,h,T1), group_rank(2,g,T2).
match(55, T1, T2, july,  3, 1700, stpetersburg) :-
    group_rank(1,f,T1), group_rank(2,e,T2).

% quarter finals
match(58, T1, T2, july,  6, 2100, kazan) :-
    winner(53,T1), winner(54,T2).
match(57, T1, T2, july,  6, 1700, novgorod) :-
    winner(49,T1), winner(50,T2).
match(60, T1, T2, july,  7, 1800, samara) :-
    winner(55,T1), winner(56,T2).
match(59, T1, T2, july,  7, 2100, sochi) :-
    winner(51,T1), winner(52,T2).

%semi finals
match(61, T1, T2, july, 10, 2100, stpetersburg) :-
    winner(57,T1), winner(58,T2).
match(62, T1, T2, july, 11, 2100, moscow) :-
    winner(59,T1), winner(60,T2).

%finals
match(63, T1, T2, july, 14, 1700, stpetersburg) :-
    loser(61,T1), loser(62,T2).
match(64, T1, T2, july, 15, 1800, moscow) :-
    winner(61,T1), winner(62,T2).
    

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

group_match(M) :- M =< 48.

mdt(LocalTime, UTCOffset, Result) :-
    Result is LocalTime - (UTCOffset+5)*100.

schedule(Ms, Ds, Me, De, [N,T1,T2,DoW,M,D,Tm]) :-
    match(N, T1, T2, M, D, Tl, L),
    day_of_week_from_date(M,D,DoW),
    month_to_ord(Ms, NMs),
    month_to_ord(Me, NMe),
    month_to_ord(M, NM),
    NMs =< NM, NM =< NMe,
    Ds =< D, D =< De,
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

format_group_stat([T,_,_,_,_,_,_,_,P]) :-
    format("~a~t~d~8|~n",[T,P]).

write_group_stats(G) :-
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

% vim: filetype=prolog
