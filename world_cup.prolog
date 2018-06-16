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
match(44, eng, bel, june, 28, 2000, kaliningrad).
match(48, sen, col, june, 28, 1800, samara).
match(46, pan, tun, june, 28, 2100, saransk).
match(47, jpn, pol, june, 28, 1700, volgograd).
    
% RO16 matches
match(50, T1, T2, june, 30, 1700, kazan) :-
    group_rank(1,c,T1), group_rank(2,d,T2).
match(49, T1, T2, june, 30, 2100, sochi) :-
    group_rank(1,a,T1), group_rank(2,b,T2).
match(51, T1, T2, june, 31, 1700, moscow) :-
    group_rank(1,b,T1), group_rank(2,a,T2).
match(52, T1, T2, june, 31, 2100, novgorod) :-
    group_rank(1,d,T1), group_rank(2,c,T2).
match(54, T1, T2, july,  1, 2100, rostov) :-
    group_rank(1,g,T1), group_rank(2,h,T2).
match(53, T1, T2, july,  1, 1800, samara) :-
    group_rank(1,e,T1), group_rank(2,f,T2).
match(56, T1, T2, july,  2, 2100, moscow) :-
    group_rank(1,h,T1), group_rank(2,g,T2).
match(55, T1, T2, july,  2, 1700, stpetersburg) :-
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
    

% results
result( 1, rus, 5, ksa, 0).
result( 2, egy, 0, uru, 1).
result( 4, mar, 0, irn, 1).
result( 3, por, 3, esp, 3).
result( 5, fra, 2, aus, 1).
result( 7, arg, 1, isl, 1).
result( 6, per, 0, den, 1).
result( 8, cro, 2, nga, 0).
%result(10, crc, 0, srb, 0).
%result(11, ger, 0, mex, 0).
%result( 9, bra, 0, sui, 0).
%result(12, swe, 0, kor, 0).
%result(13, bel, 0, pan, 0).
%result(14, tun, 0, eng, 0).
%result(15, pol, 0, sen, 0).
%result(17, rus, 0, egy, 0).
%result(16, col, 0, jpn, 0).
%result(20, irn, 0, esp, 0).
%result(19, por, 0, mar, 0).
%result(18, uru, 0, ksa, 0).
%result(21, fra, 0, per, 0).
%result(23, arg, 0, cro, 0).
%result(22, den, 0, aus, 0).
%result(26, srb, 0, sui, 0).
%result(25, bra, 0, crc, 0).
%result(24, nga, 0, isl, 0).
%result(29, bel, 0, tun, 0).
%result(28, kor, 0, mex, 0).
%result(27, ger, 0, swe, 0).
%result(32, jpn, 0, sen, 0).
%result(31, pol, 0, col, 0).
%result(30, eng, 0, pan, 0).
%result(36, esp, 0, mar, 0).
%result(33, uru, 0, rus, 0).
%result(35, irn, 0, por, 0).
%result(34, ksa, 0, egy, 0).
%result(37, den, 0, fra, 0).
%result(40, isl, 0, cro, 0).
%result(39, nga, 0, arg, 0).
%result(38, aus, 0, per, 0).
%result(44, mex, 0, swe, 0).
%result(43, kor, 0, ger, 0).
%result(41, srb, 0, bra, 0).
%result(42, sui, 0, crc, 0).
%result(44, eng, 0, bel, 0).
%result(48, sen, 0, col, 0).
%result(46, pan, 0, tun, 0).
%result(47, jpn, 0, pol, 0).

% note this is a different kind of group predicate
group_rank(R,G,T) :- group_stats(G,L), nth1(R,L,[T|_]).

winner(G, T) :- result(G, T, Gf, _, Ga), Gf > Ga.
winner(G, T) :- result(G, _, Ga, T, Gf), Gf > Ga.
loser(G, T) :- result(G, T, Gf, _, Ga), Gf < Ga.
loser(G, T) :- result(G, _, Ga, T, Gf), Gf < Ga.

mdt(LocalTime, UTCOffset, Result) :-
    Result is LocalTime - (UTCOffset+5)*100.

schedule(Ms, Ds, Me, De, [N,T1,T2,DoW,M,D,Ti,G]) :-
    match(N, T1, T2, M, D, Tm, L),
    day_of_week_from_date(M,D,DoW),
    month_to_ord(Ms, NMs),
    month_to_ord(Me, NMe),
    month_to_ord(M, NM),
    NMs =< NM, NM =< NMe,
    Ds =< D, D =< De,
    group(G, _group),
    member(T1, _group),
    member(T2, _group),
    timezone(L, U),
    mdt(Tm,U,Ti).

team_schedule(T, [T1,T2,DoW,M,D,Ti,G]) :-
    match(_, T1, T2, M, D, Tm, L),
    member(T, [T1,T2]),
    day_of_week_from_date(M,D,DoW),
    group(G, _group),
    member(T1, _group),
    member(T2, _group),
    timezone(L, U),
    mdt(Tm,U,Ti).

goals_for_against_wdl(T,[T,Gf,_,Ga],Gf,Ga,W,D,L) :-
    win_draw_loss(Gf,Ga,[W,D,L]).
goals_for_against_wdl(T,[_,Ga,T,Gf],Gf,Ga,W,D,L) :-
    win_draw_loss(Gf,Ga,[W,D,L]).

win_draw_loss(Gf,Ga,[1,0,0]) :- Gf > Ga.
win_draw_loss(Gf,Ga,[0,1,0]) :- Gf =:= Ga.
win_draw_loss(Gf,Ga,[0,0,1]) :- Gf < Ga.

group_points(W,D,Pts) :- Pts is W*3+D.

group_stat([],A,A).
group_stat([Head|Tail],[T,Pld,W,D,L,Gf,Ga,GD,_],R) :-
    PldNew is Pld+1,
    goals_for_against_wdl(T,Head,_Gf,_Ga,_W,_D,_L),
    WNew is W+_W, DNew is D+_D, LNew is L+_L,
    GfNew is Gf+_Gf, GaNew is Ga+_Ga,
    GDNew is GfNew-GaNew,
    group_points(WNew,DNew,Pts),
    group_stat(Tail,[T,PldNew,WNew,DNew,LNew,GfNew,GaNew,GDNew,Pts],R).
    
group_stat(T, R) :-
    findall(Rn, (
	result(N,T1,G1,T2,G2),
	N =< 48,
	member(T,[T1,T2]),
	Rn=[T1,G1,T2,G2]),
    Rs),
    group_stat(Rs,[T,0,0,0,0,0,0,0,0],R).

group_stats(G, R) :-
    group(G,Gl),
    maplist(group_stat, Gl, Gs),
    predsort(stat_cmp, Gs, R).

write_group_stat([T,_,_,_,_,_,_,_,P]) :-
    format("~a~t~d~8|~n",[T,P]).

write_group_stats(G) :-
    group_stats(G, S),
    maplist(write_group_stat, S).

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
write_schedule([N,T1,T2,DoW,M,D,T,G]) :-
    from_military_time(T,Ti),
    format("~t~d~2+ ~a v ~a ~|~a~t~10+~a ~a ~a~n",
	[N,T1,T2,DoW,M,D,Ti]).

write_schedules(Ma,Da,Mb,Db) :-
    findall(R, schedule(Ma,Da,Mb,Db,R), Rs),
    predsort(sched_cmp, Rs, ToWrite),
    maplist(write_schedule, ToWrite), !.

% signs are such that leader is first in list
stat_cmp('>',[_,_,_,_,_,_,_,_,P1],[_,_,_,_,_,_,_,_,P2]) :- P1<P2.
stat_cmp('<',[_,_,_,_,_,_,_,_,P1],[_,_,_,_,_,_,_,_,P2]) :- P1>P2.
stat_cmp('>',[_,_,_,_,_,_,_,GD1,P],[_,_,_,_,_,_,_,GD2,P]) :- GD1<GD2.
stat_cmp('<',[_,_,_,_,_,_,_,GD1,P],[_,_,_,_,_,_,_,GD2,P]) :- GD1>GD2.
stat_cmp('>',[_,_,_,_,_,Gf1,_,GD,P],[_,_,_,_,_,Gf2,_,GD,P]) :- Gf1<Gf2.
stat_cmp('<',[_,_,_,_,_,Gf1,_,GD,P],[_,_,_,_,_,Gf2,_,GD,P]) :- Gf1>Gf2.
stat_cmp('<',[_,_,_,_,_,Gf,_,GD,P],[_,_,_,_,_,Gf,_,GD,P]).

sched_cmp('<',[G1,_,_,_,M,D,T,_],[G2,_,_,_,M,D,T,_]) :- G1<G2.
sched_cmp('>',[G1,_,_,_,M,D,T,_],[G2,_,_,_,M,D,T,_]) :- G1>G2.
sched_cmp('<',[_,_,_,_,M,D,Ta,_],[_,_,_,_,M,D,Tb,_]) :- Ta<Tb.
sched_cmp('>',[_,_,_,_,M,D,Ta,_],[_,_,_,_,M,D,Tb,_]) :- Ta>Tb.
sched_cmp('<',[_,_,_,_,M,Da,_,_],[_,_,_,_,M,Db,_,_]) :- Da<Db.
sched_cmp('>',[_,_,_,_,M,Da,_,_],[_,_,_,_,M,Db,_,_]) :- Da>Db.
sched_cmp('<',[_,_,_,_,Ma,_,_,_],[_,_,_,_,Mb,_,_,_]) :-
    month_to_ord(Ma,M1), month_to_ord(Mb,M2), M1<M2.
sched_cmp('>',[_,_,_,_,Ma,_,_,_],[_,_,_,_,Mb,_,_,_]) :-
    month_to_ord(Ma,M1), month_to_ord(Mb,M2), M1>M2.

% vim: filetype=prolog
