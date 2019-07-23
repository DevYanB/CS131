%---------------------------
% plain_tower (since, according to Tim, it will be more logical to build this first then extend capabilities)

%-----
% Different allows us to check row uniqueness
%-----
% Note: sort merges lists that are the same
different(L) :- sort(L, Sorted), length(L, Lh1), length(Sorted, Lh2), Lh1 == Lh2.

%REWORD
%-----
% Solving the actual thing
%-----
% Basically, for every row, you see from left how many you can count
% This is where the benifit of transposing and reversing come into effect with Up, Down, Left, and Right
lft_slve([A | As], Bo) :- lft_slve(As, A, 1, Bo).
% Two cases: A > Max, or A < Max
lft_slve([], _, B, B).
lft_slve([A | As], Max, B, Bo) :-  A > Max, B1 is B+1,
    % A is max, and we see another tower
    lft_slve(As, A, B1, Bo).
lft_slve([A | As], Max, B, Bo) :- A < Max,lft_slve(As, Max, B, Bo).

% Checks counts for one side of tower
slve([], []).
slve([[A | As] | T], [B | Bs]) :- lft_slve([A | As], B), slve(T, Bs).

%-----
% Flips each row
%-----
flip([], []).
flip([T | LTs], [Tr | RTs]) :- reverse(T, Tr), flip(LTs, RTs).

%-----
% Transpose the matrix
%-----
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
    lists_firsts_rests(Ms, Ts, Ms1),
    transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
    lists_firsts_rests(Rest, Fs, Oss).

%-----
% Checking elems
%-----
% Take size and will create array including those vals of that size and bind to P.
% need to use the between predicate here with findall:
% https://stackoverflow.com/questions/20514022/prolog-between-3-with-a-list-at-the-end
makeP(N,P) :- findall(X, between(1,N,X), P).

%-----
% Enumerate over rows
%-----
% this thiq boi is a row by row check to determine elements. Initial thought was to create a linear
% search through per element, but that will definetely be inefficient and redundant, since prolog has 
% some inbuilt functionality to tackle this. Above are the helper functions
enum([],_).
enum([H|T],N) :- makeP(N,P), permutation(H,P), enum(T,N). 

%-----
% Checking length for row
%-----
% This is just to check each element in the list of lists has a correct length
rowL([], _).
rowL([H|T],N) :- length(H,N) , rowL(T,N). 


plain_tower(N,T,C) :- 
    % first check if the lengths are alright for the height.
    length(T,N),
    % now to also check each list for each element in the list of lists. This leads to
    % creating a new predicate for this: rowL.
    rowL(T,N),
    % now need to check that every row has the right elements. So, we create enum,
    % a predicate that will check through each element and make sure its a permutation of 1-N
    enum(T,N),
    % Now, let us check that every row and column contain unique elements
    % First we should figure out an easier way to check columns, and then let us check the rows for that and the
    % transpose rows.
    transpose(T,NT),
    % Ok, since we now have the transposed matrix, time to check uniqueness!
    maplist(different , NT),
    maplist(different, T),
    % Ok, now we have proved that columns are good! 
    % These will reverse the elements of each row
    flip(T, RT),
    flip(NT, RNT),
    counts(U,D,L,R) = C,
    % Starting to take steps to build the array!!!
    % Init the rows with the lengths
    length(U,N),
    length(D,N),
    length(L,N),
    length(R,N),
    slve(NT, U),
    slve(RNT, D),
    slve(T, L),
    slve(RT, R).
%---------------------------

%---------------------------
% Time to implement tower now! With the above logic, it should now be doable!


%-----
% Values correct check with fd_domain
%-----
dom([] , _).
dom([H|T] , N) :- 
    fd_domain(H,1,N) , 
    dom(T,N).


tower(N,T,C) :-
    % Same basics checks as before for length constraints
    length(T,N),
    rowL(T,N),
    % Now that we have the lovely FD solver, we can use this instead of our enum to check values line up
    dom(T,N),
    maplist(fd_all_different , T),
    transpose(T, NT),
    maplist(fd_all_different , NT),
    maplist(fd_labeling , T),
    flip(T, RT),
    flip(NT, RNT),
    counts(U,D,L,R) = C,
    % Starting to take steps to build the array!!!
    % Init the rows with the lengths
    length(U,N),
    length(D,N),
    length(L,N),
    length(R,N),
    slve(NT, U),
    slve(RNT, D),
    slve(T, L),
    slve(RT, R).    
%---------------------------

%---------------------------
% Last case: ambiguous!

ambiguous(N,C,T1,T2) :-
    tower(N,T1,C),
    tower(N,T2,C),
    T1 \= T2.

%---------------------------

%---------------------------
% Time for some testing and statistics
% https://stackoverflow.com/questions/34970061/display-the-execution-times-for-each-goal-of-a-predicate-clause

time(X) :-
    statistics(cpu_time,[Start|_]),
    tower(4, T, counts([3,2,4,1],[2,2,1,2],[3,2,1,3],[1,3,3,2])),
    statistics(cpu_time,[Stop|_]),
    X is Stop - Start.

ptime(Y) :-
    statistics(cpu_time,[Start2|_]),
    plain_tower(4, T, counts([3,2,4,1],[2,2,1,2],[3,2,1,3],[1,3,3,2])),
    statistics(cpu_time,[Stop2|_]),
    Y is Stop2 - Start2.

speedup(Ratio) :- 
    time(X),
    ptime(Y), 
    Ratio is Y/X.
%---------------------------
