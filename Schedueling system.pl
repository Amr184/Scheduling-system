ta_slot_assignment([ta(N,L1) | T], [ta(N, L2) | T], N) :- L1 > 0, L2 is L1 - 1.
ta_slot_assignment([ta(N1,L1) | T], [ta(N1, L1) | T1], N) :- N \= N1, ta_slot_assignment(T, T1, N).

ta_slot_assignment_extended(TAs, TAs, []).
ta_slot_assignment_extended(TAs, RemTAs, [H|T]) :- ta_slot_assignment(TAs, R, H), ta_slot_assignment_extended(R, RemTAs, T).

slot_assignment(N , TAs , Res , A ) :- length(A, N),
                                       subsets(TAs, X),
                                       get_assignment(X, A),
                                       ta_slot_assignment_extended(TAs, Res, A).
                                       


max_slots_per_day(DaySched,Max) :- flatten(DaySched, T), sort(T, Letters), max_helper(T, Letters, Max).

max_helper(F, [], _).
max_helper(F, [H|T], Max) :- count(F, H, N), N =< Max, max_helper(F, T, Max).

count([], _, 0).
count([X|T], X, N) :- count(T, X, N1), N is N1 + 1.
count([H|T], X, N) :- H \= X, count(T, X, N).



day_schedule(DaySlots, TAs, RemTAs , Assignment ) :- length(DaySlots, N), length(Assignment, N), DaySlots = [ H |T ] ,
                                                                slot_assignment(H, TAs, RemTAs1, H1) ,
                                                                Assignment = [ H1 | T1 ] ,
                                                                day_schedule( T , RemTAs1 , RemTAs , T1 ).

day_schedule([], TAs, TAs, _).







week_schedule([ H | T ], TAs, DaysMax, [ H2 | T2]) :-     length([ H | T],N) ,
                                                          length([ H2 | T2],N) ,
                                                          day_schedule( H , TAs , RemTAs2 , H2 ) ,
                                                          max_slots_per_day(H2 , DaysMax ),
                                                          week_schedule(T,RemTAs2,DaysMax,T2) .

week_schedule([],_,_,_).




subset([], []).
subset([E|Tail], [E|NTail]):-
  subset(Tail, NTail).
subset([_|Tail], NTail):-
  subset(Tail, NTail).

subsets(X, Y) :- subset(X, Z), permutation(Z,Y).
get_assignment([ta(N,_) | T], [N | T1]) :- length([ta(N,_) | T], X), length([N | T1], X), get_assignment(T,T1).
get_assignment([], _).


