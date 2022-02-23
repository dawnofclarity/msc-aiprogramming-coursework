%
% A London Underground Route Planner in Prolog
%
% Dawn Raison (6609229)
%
% Atoms representing the lines/stations are specified in the database,
% which should be loaded before attempting to run any searches.
%
% Main entry points:
%
% 1) Execute search, and return result in human readable format
% on output stream
%
% search(+Start, +End) nondet
% e.g.
% search(stnHydeParkCorner, stnRegentsPark).
% Take the Piccadilly Line from Hyde Park Corner to Piccadilly Circus for
% 2 stops (Hyde Park Corner -> Green Park -> Piccadilly Circus; approx 4
% mins). Change to the Bakerloo Line at Piccadilly Circus towards Regent's
% Park for 2 stops (Piccadilly Circus -> Oxford Circus -> Regent's Park;
% approx 5 mins).
%
% 2) Execute search, and return raw result in variable
%
% search(+Start, +End, -Result) nondet
% In this case the result is returned as a list of (station, line, distance)
% sequences detaining how to get from the start to the end node.
% e.g.
% search(stnHydeParkCorner, stnRegentsPark, R).
% R = [(stnHydeParkCorner, linePiccadilly, 0),
% (stnGreenPark, linePiccadilly, 2), (stnPiccadillyCircus, linePiccadilly, 2),
% (stnOxfordCircus, lineBakerloo, 2), (stnRegentsPark, lineBakerloo, 3)].
%
% Implemented using an A* Search using a heuristic f(n) = g(n) + h(n)
% where:
%  g(n) = cumulative path to this point
%  h(n) = SLD approximation based on the Euclidian distance between the two
%         stations being compared using approx x,y coordinates from the
%         station facts
%
% Expanded nodes are stored in a list on expansion, using a sorted insert
% such that lower f values sort to the top.
% List elements each are a sequence containing:
%  (Station, F, G, Path)
% Path is a list of sequences containing:
%  (Station, Line, Duration)

% search(+Start, +End) nondet
%
% @arg +Start         Station atom denoting the start station
% @arg +End           Station atom denoting the end station
%
% Search for the best route between Start and End,
% then pretty print it to the output stream.
search(Start, End) :-
    search(Start, End, Route),
    pretty_print_route(Route, Result),
    writeln(Result).

% search(+Start, +End, -Result) nondet
%
% @arg +Start         Station atom denoting the start station
% @arg +End           Station atom denoting the end station
% @arg -Result        Result of search - array
%                     of (station, line, distance) sequences
%
% Search for the best route between Start and End and return it in Result.

% Nothing to do variant (Start matches End).
search(Start, Start, []) :-
    !.

% Find shortest path variant
search(Start, End, Result) :-
    search_start(Start, AList),
    search_list(End, AList, Out),
    search_end(Out, Result).

% search_start(+Start, -AList) det
%
% @arg +Start         Station atom denoting the start station
% @arg -AList         Sorted search node list to be initialised
%
% Initialise the AList for a search starting at Start.
search_start(Start, [(Start, 0, 0, [(Start, _, 0)])]).

% search_end(+Out, -Result) det
%
% @arg +Out           Search data to be made ready for return to caller
% @arg -Result        Data to be returned to caller
%
% Post process the data accumulated by the search for returning to the caller.
% In this implementation, the results are normalised by reversing the list.
search_end(Out, Result) :-
    reverse(Out, Result).

% search_list(+End, +AList, -Result) det
%
% @arg +End           Station atom denoting the end station
% @arg +AList         Sorted search node list
% @arg -Result        Result of search - array
%                     of (station, line, distance) sequences
%
% Execute an A* search using the sorted AList to find the most favourable path
% to the end station.

% No solutions case; typically the start or end atom are incorrect
search_list(_, [], []) :-
    !,
    writeln(['No solutions found!']).

% Get the next node from the AList and process it.
search_list(End, AList, Result) :-
    get_node_from_list(AList, Node, AList1),
    process_node(End, Node, AList1, Result).

% process_node(+End, +Node, +AList, -Result) det
%
% @arg +End           Station atom denoting the end station
% @arg +Node          Current node to be examined
% @arg +AList         Sorted search node list
% @arg -Result        Result of search - array
%                     of (station, line, distance) sequences
%
% Examine the current node, determine if we reached our goal, or need to expand
% further

% Goal reached state; note the cut.
process_node(End, Node, _, Result) :-
    check_if_goal_reached(End, Node),
    % Cut here as we only want the first result.
    % Comment this to generate less optimal routes via backtracking.
    !,
    update_result(Node, Result).

% Goal NOT reached state
% log_expand can be uncommented to view search graph expansion
process_node(End, Node, AList, Result) :-
    expand_node(End, Node, ChildNodes),
%    log_expand(Node, ChildNodes),
    insert_all(ChildNodes, AList, AList1),
    search_list(End, AList1, Result).

% update_result(+Node, -Path) det
%
% @arg +Node          Current node
% @arg -Path          Result of search - array
%                     of (station, line, distance) sequences
%
% Updates the result from a node
update_result(Node, Path) :-
    (_, _, _, Path) = Node.

% get_node_from_list(+AList, -Node, -AList1) det
%
% @arg +AList         Sorted search node list
% @arg -Node          Node as extracted from the head of the list
% @arg -AList1        Sorted search node list, less the extracted node.
%
% Extracts a node from the head of the sorted list
get_node_from_list([Node | Tail], Node, Tail).

% check_if_goal_reached(+End, +Node) nondet
%
% @arg +End           Station atom denoting the end station
% @arg +Node          Node to be examined
%
% Checks the node against the end criteria to see if we have reached
% the destination
check_if_goal_reached(End, Node) :-
    (A, _) = Node,
    End = A.

% expand_node(+End, +Node, -ChildNodes) det
%
% @arg +End           Station atom denoting the end station
% @arg +Node          Node to be expanded
% @arg -ChildNodes    List of child nodes derived from this node
%
% Expand the child nodes of node.
% Loops within the path to this point are culled.
% If no expansion is possible, an empty bag is returned.
% Each child calculates it's g(), h() and hence f()
expand_node(End, Node, ChildNodes) :-
    (_, _, _, PathSoFar) = Node,
    bagof(
        (NextStn, NextF, NextG, [(NextStn, Line, Duration) | PathSoFar]),
        node_generator(End, Node, NextStn, Line, Duration, NextF, NextG),
        ChildNodes
    ) -> true; ChildNodes = []. % Return empty bag, rather than fail.

% node_generator(+End, +Node, ?NextS, ?Line, ?Duration, ?NextF, ?NextG) semidet
%
% @arg +End           Station atom denoting the end station
% @arg +Node          Node to be expanded
% @arg ?NextS         Receives next candidate station atom
% @arg ?Line          Receives line atom leading to next station
% @arg ?Duration      Receives time taken to navigate to next station
% @arg ?NextF         Receives f() computed for this transition
% @arg ?NextStn       Receives g() computed for this transition
%
% Extracted bagof goal for node expansion.
% Candidates are matched via linked/4 from the database.
% Loops back to stations already on the path are culled.
% Line change penalties are evaluated.
% g(), h() and hence f() are calculated
node_generator(End, Node, NextStn, Line, Duration, NextF, NextG) :-
    (ParentStn, _, GSoFar, PathSoFar) = Node,
    linked(ParentStn, NextStn, Line, Duration),
    loop_check(NextStn, PathSoFar),
    line_change_penalty(PathSoFar, Line, AddDuration),
    g(GSoFar, Duration, AddDuration, NextG),
    h(End, NextStn, NextH),
    f(NextG, NextH, NextF).

% loop_check(+TestStation, +Path) nondet
%
% @arg +TestStation   Station atom denoting the station to check for in path
% @arg +Path          Path to this point - array of
%                     (station, line, distance) sequences
%
% Checks for loops by examping the path taken so far for an occurance of
% test station, and failing if the test station is found.
loop_check(_, []).
loop_check(TestStation, [(Station, _) | Tail]) :-
     TestStation \= Station,
     loop_check(TestStation, Tail).

% line_change_penalty(+Path, +ProposedLine, -AddDuration) det
%
% @arg +Path          Path to this point - array of
%                     (station, line, distance) sequences
% @arg +ProposedLine  Line atom denoting the line we intend to take
% @arg -AddDuration   Receives a value representing the penalty incurred
%                     by changing line (if applicable)
%
% Determine the amount of any penalty due to changing line
% @see config fact in database.

% Previous line matches proposed line case; no penalty
line_change_penalty([(_, PreviousLine, _) | _], PreviousLine, AddDuration) :-
    AddDuration is 0.

% no previous line case; no penalty
line_change_penalty([], _, AddDuration) :-
    AddDuration is 0.

% Previous line does not match proposed line case; apply penalty
line_change_penalty([(_, PreviousLine, _) | _], ProposedLine, AddDuration) :-
    PreviousLine \= ProposedLine,
    config('changeLine', AddDuration).

% g(+GSoFar, +Duration, +AddDuration, -NextG) det
%
% @arg +GSoFar        Cumulative duration so far
% @arg +Duration      Transit duration from node to this station
% @arg +AddDuration   Any penalty duration incurred (e.g. by changing line)
% @arg -NextG         Receives computed g()
%
% Computes g() for the new node.
% g() represents the cumulative duration taken to reach this point.
g(GSoFar, Duration, AddDuration, NextG) :-
    NextG is GSoFar + Duration + AddDuration.

% h(+End, +NextStn, -NextH) det
%
% @arg +End           Station atom denoting the end station
% @arg +NextStn       Station atom denoting the next station
% @arg -NextH         Receives computed h()
%
% Derive a heuristic representing an approx duration should we travel
% to the destination node as the crow flies.
% Computed as the cartesian L2 distance between the end station co-ords
% and the next station co-ords, then divided by a tunable value to yield
% an approximation of duration
h(End, NextStn, NextH) :-
    station(End, _, XEnd, YEnd),
    station(NextStn, _, XNext, YNext),
    DX is XNext - XEnd,
    DY is YNext - YEnd,
    config('avSpeed', AvSpeed),
    NextH is sqrt(DX^2 + DY^2) / AvSpeed.

% f(+NextG, +NextH, -NextF) det
%
% @arg +NextG         Computed g() for this node
% @arg +NextH         Computed h() for this node
% @arg -NextF         Receives f() for this node
%
% Calculate a cost of using this node as a continuation of our path,
% based on the g and h functions.
f(NextG, NextH, NextF) :-
    NextF is NextG + NextH.


% insert_all(+NodesToAdd, +InputList, -OutputList) det
%
% @arg +NodesToAdd    List of Nodes to be added into the list
% @arg +InputList     Existing list of Nodes
% @arg -OutputList    Updated list of Nodes
%
% Added a list of Nodes to an existing list of Nodes, maintaining them in
% in a sorted order such that those nodes with a LOW f() float to the head
% of the list

% Insert Head into list and recurse on tail
insert_all([Head | Tail], InputList, OutputList) :-
    insert_sort(Head, InputList, IntermediateList),
    insert_all(Tail, IntermediateList, OutputList).

% Nothing to insert case
insert_all([], InputList, InputList).

% insert_sort(+Node, +InputList, -OutputList) det
%
% @arg +Node          Node to be added into the list
% @arg +InputList     Existing list of Nodes
% @arg -OutputList    Updated list of Nodes
%
% Added a Node to an existing list of Nodes, maintaining then in
% in a sorted order such that those nodes with a LOW f() float to the head
% of the list

% Inserting a node into an empty list results in a list with the node in it.
insert_sort(Node, [], [Node]) :-
    !.

% Inserting a node at head succeeds if F is lower
insert_sort(Node, [HeadNode | Tail], [Node, HeadNode | Tail]) :-
    (_, NodeF, _) = Node,       % Extract F
    (_, HeadF, _) = HeadNode,
    NodeF =< HeadF,             % Are we prior to HeadF?
    !.                          % Thou shalt not pass

% Try next step down the list
insert_sort(Node, [Head | InTail], [Head | OutTail]) :-
    insert_sort(Node, InTail, OutTail).

% pretty_print_route(+Path, -Result) det
%
% @arg +Path          Path to this point - array of
%                     (station, line, distance) sequences
% @arg -Result        List of textual instructions
%
% Formats the given path in a human readable format.
pretty_print_route(Route, Result) :-
    init_parse(Route, Stations, CurrentLine, CurrentDuration, Out),
    parse_next_station(Route, Stations,
        CurrentLine, CurrentDuration, Out, Result).

% init_parse(+Path, -Stations, -Line, -Duration, -Out) det
%
% @arg +Path          Path to be formatted - array of
%                     (station, line, distance) sequences
% @arg -Stations      Receives Station accumulator list
% @arg -Line          Initialised to the staring line line atom
% @arg -Duration      Initialised to 0
% @arg -Out           Initialised to an empty list
%
% Prepares the formatter to begin translating the path into a more human
% friendly readable format
init_parse([(_, CurrentLine, _) | _], [], CurrentLine, 0, []).

% parse_next_station(+Path, +Stations, +Line, +Duration, +ARoute, -Result) det
%
% @arg +Path          Path to be formatted - array of
%                     (station, line, distance) sequences
% @arg +Stations      List of stations on this line traversed so far
% @arg +Line          Line atom denoting the current line
% @arg +ARoute        Accumulated formatted route so far
% @arg -Out           receives the final formatted route
%
% Parses the next station and decides if this is a new travel segment,
% an extension of an travel segment, or the end of a travel segment.

% Reached the end of the path; close travel segment and update route
parse_next_station([], [PrevStation | Stations],
        CurrentLine, CurrentDuration, AccumRoute, Result) :-
    format_segment(AccumRoute,
        AccumRoute1, PrevStation, Stations, CurrentLine, CurrentDuration),
    reverse(AccumRoute1, Result).

% An extension of an existing travel segment; extend it.
parse_next_station([(HeadStation, CurrentLine, HeadDuration) | Tail],
        Stations, CurrentLine, CurrentDuration, AccumRoute, Result) :-
    !,
    CurrentDuration1 is CurrentDuration + HeadDuration,
    parse_next_station(Tail, [HeadStation | Stations],
        CurrentLine, CurrentDuration1, AccumRoute, Result).

% Line change; end the open segment and start a new one
parse_next_station([(HeadStation, HeadLine, HeadDuration) | Tail],
       [PrevStation | Stations], CurrentLine,
       CurrentDuration, AccumRoute, Result) :-
    format_segment(AccumRoute, AccumRoute1,
        PrevStation, Stations, CurrentLine, CurrentDuration),
    parse_next_station(Tail, [HeadStation, PrevStation],
        HeadLine, HeadDuration, AccumRoute1, Result).

% format_segment(+AccumIn, -AccumOut, +PrevStn, +Stations, +Line, +Duration) det
%
% @arg +AccumIn       Accumulated formatted route so far
% @arg +AccumOut      Receives updated formatted route so far
% @arg +PrevStn       Station atom denoting previous station
% @arg +Line          Line atom denoting current line
% @arg +Duration      Elapsed time taken in this segment
%
% Formats the travel segment into english by translating the line/station atoms,
% then embedding the info into an easily readable textual representation
% of the route

% Initial segment; use "Take the..." wording
format_segment([], [AccumRoute],
        PrevStation, Stations, CurrentLine, CurrentDuration) :-
    !,  % We don't want a backtrack decision point here.
    get_first_station(Stations, FirstStation),
    format_line(CurrentLine, FormattedCurrentLine),
    format_station(FirstStation, FormattedFirstStation),
    format_station(PrevStation, FormattedPrevStation),
    format_station_list([PrevStation | Stations], FormattedStations),
    length(Stations, StopCount),
    format_plural(CurrentDuration, 'min', 'mins', MinText),
    format_plural(StopCount, 'stop', 'stops', StopText),
    atomic_list_concat(FormattedStations, ' -> ', StationList),
    atomic_list_concat(
      [
        'Take the ', FormattedCurrentLine, ' from ', FormattedFirstStation,
        ' to ', FormattedPrevStation, ' for ', StopCount, ' ', StopText,
        ' (', StationList, '; approx ', CurrentDuration, ' ', MinText, ').'
      ], AccumRoute).

% Subsequent segment; use "Change to..." wording
format_segment(AccumRoute, [AccumRoute1 | AccumRoute],
        PrevStation, Stations, CurrentLine, CurrentDuration) :-
    get_first_station(Stations, FirstStation),
    format_line(CurrentLine, FormattedCurrentLine),
    format_station(FirstStation, FormattedFirstStation),
    format_station(PrevStation, FormattedPrevStation),
    format_station_list([PrevStation | Stations], FormattedStations),
    length(Stations, StopCount),
    format_plural(CurrentDuration, 'min', 'mins', MinText),
    format_plural(StopCount, 'stop', 'stops', StopText),
    atomic_list_concat(FormattedStations, ' -> ', StationList),
    atomic_list_concat(
      [
        'Change to the ', FormattedCurrentLine, ' at ', FormattedFirstStation,
        ' towards ', FormattedPrevStation, ' for ', StopCount, ' ', StopText,
        ' (', StationList, '; approx ', CurrentDuration, ' ', MinText, ').'
      ], AccumRoute1).

% format_plural(+Number, +Singular, +Plural, -Out) det
%
% @arg +Number        The number to be tested for plurality
% @arg +Singular      The text to return if Number is singular.
% @arg +Plural        The text to return if Number is Plural.
% @arg -Out           Receives the appropriate text.
%
% Formats a text either singular or plural based on Number.
format_plural(Number, Out, _, Out) :-
    Number = 1,
    !.  % We don't want a backtrack decision point here.
format_plural(Number, _, Out, Out) :-
    Number \= 1.

% format_station(+Station, -Formatted) nondet
%
% @arg +Station       The station atom to be formatted
% @arg -Formatted     Receives the human readable version of the station.
%
% Formats a station atom by looking up the associated station fact, and
% extracting the name element
format_station(Station, Formatted) :-
    station(Station, Formatted, _, _).

% format_station_list(+Stations, -Out) nondet
%
% @arg +Stations      A list of station atoms to be formatted
% @arg -Out           Receives a list of formatted stations (reversed).
%
% Formats a list of stations.
format_station_list(Stations, Out) :-
    format_station_list(Stations, [], Out).

format_station_list([], Out, Out).
format_station_list([Head | Tail], FormattedStations, Out) :-
    format_station(Head, FormattedStation),
    format_station_list(Tail, [FormattedStation | FormattedStations], Out).


% format_line(+Line, -Formatted) nondet
%
% @arg +Line          The line atom to be formatted
% @arg -Formatted     Receives the human readable version of the line.
%
% Formats a line atom by looking up the associated line fact, and
% extracting the name element
format_line(Line, Formatted) :-
    line(Line, Formatted).

% get_first_station(+Stations, -First) det
%
% @arg +Stations      A list of stations
% @arg -First         Receives the first (added) station from the list
%
% Retrieves the initial station from this travel route

% No list left case
get_first_station([Head | []], Head) :-
    !.  % We don't want a backtrack decision point here.

% Recurse down the list until we get to the end
get_first_station([_ | Tail], FirstStation) :-
    get_first_station(Tail, FirstStation).

% log_expand(+Node, +ChildNodes) det
%
% @arg +Node          The node that was expanded
% @arg +ChildNodes    The list/bag of children generated
%
% Writes a list of child node expansions to the output stream

% Empty list case
log_expand(_, []) :-
    !.  % We don't want a backtrack decision point here.

% Populated list case
log_expand(ParentNode, [Node|Tail]) :-
    (Stn, F, G, _) = Node,
    (ParentStn, _) = ParentNode,
    writeln([' Ex >', ParentStn, '->', Stn, 'G:', G, 'F:', F]),
    log_expand(ParentNode, Tail).

% writeln(+List) det
%
% @arg +List          A list of items to write
%
% Writes a list of items to the standard output stream separated with spaces,
% and terminated with a new line.
writeln([X | Xs]) :-
    write(X),
    write(' '),
    writeln(Xs).

writeln([]) :-
    nl.
